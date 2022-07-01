//! `Section` objects
use std::{
  collections::{HashMap, HashSet},
  path::{Path, PathBuf},
};
use bimap::BiHashMap;
use crate::{InputFormat, Entries, Unknown, SkipEmpty};
use crate::utils::reduce_array;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum DataSourceType {
  File,
  //
}

pub struct DataSource {
  pub(crate) typ: DataSourceType,
  pub(crate) name: String,
  pub(crate) datatype: InputFormat,
  pub(crate) encoding: String,
  pub(crate) glob: Option<String>,
} 

struct StateSet {
  pc: HashMap<String, HashSet<String>>,
  cp: HashMap<String, HashSet<String>>,
}

impl StateSet {
  fn new() -> Self {
    Self {
      pc: HashMap::new(),
      cp: HashMap::new(),
    }
  }
}

pub struct Section {
  number: u32,
  bibentries: Entries,
  namepartlengths: HashMap<String, u32>,
  keytofromrelclone: BiHashMap<String, String>,
  relkeys: HashSet<String>,
  allkeys: bool,
  allkeysnocite: bool,
  citekeys: Vec<String>,
  /// For faster hash-based lookup of individual keys
  citekeys_h: HashSet<String>,
  labelcache_l: HashMap<String, Unknown>,
  everykey: HashSet<String>,
  everykey_lc: HashMap<String, String>,
  //bcfkeycache: {},
  labelcache_v: HashMap<String, Unknown>,
  dkeys: HashMap<String, Vec<String>>,
  keytods: HashMap<String, PathBuf>,
  orig_order_citekeys: Vec<String>,
  undef_citekeys: Vec<String>,
  cite_citekeys: HashSet<String>,
  nocite_citekeys: HashSet<String>,
  citekey_alias: HashMap<String, String>,
  //static_keys: {},
  state_set: StateSet,
  seenkeys: HashMap<String, u32>,
  citecount: HashMap<String, u32>,
  datasources: Option<Vec<Unknown>>,
}

impl Section {
  /// Initialize a crate::Section object
  fn new(number: u32) -> Self {
    Self {
      number,
      bibentries: Entries::new(),
      namepartlengths: HashMap::new(),
      keytofromrelclone: BiHashMap::new(),
      relkeys: HashSet::new(),
      allkeys: false,
      allkeysnocite: false,
      citekeys: Vec::new(),
      citekeys_h: HashSet::new(),
      labelcache_l: HashMap::new(),
      everykey: HashSet::new(),
      everykey_lc: HashMap::new(),
      labelcache_v: HashMap::new(),
      dkeys: HashMap::new(),
      keytods: HashMap::new(),
      orig_order_citekeys: Vec::new(),
      undef_citekeys: Vec::new(),
      cite_citekeys: HashSet::new(),
      nocite_citekeys: HashSet::new(),
      citekey_alias: HashMap::new(),
      state_set: StateSet::new(),
      seenkeys: HashMap::new(),
      citecount: HashMap::new(),
      datasources: None, // Vec<PathBuf>?
    }
  }

  /// Set the citecount of a key. This comes from biblatex via the
  /// citecounter option and reflects the actual number of citations using
  /// this key, taking into account things like \citeauthor etc. which are not
  /// real citations.
  fn set_citecount(&mut self, key: &str, count: u32) {
    self.citecount.insert(key.into(), count);
  }

  /// Get the citecount of a key. This comes from biblatex via the
  /// citecounter option and reflects the actual number of citations using
  /// this key, taking into account things like \citeauthor etc. which are not
  /// real citations. A zero or undef value needs to be less than 0 which does
  /// not fail if() checks - required for the delicate sorting dispatch logic
  fn get_citecount(& self, key: &str) -> i32 {
    self.citecount.get(key).map(|&n| n as i32).filter(|&n| n > 0).unwrap_or(-1)
  }

  /// Get the count of a key
  fn get_seenkey(&self, key: &str) -> Option<u32> {
    self.seenkeys.get(key).copied()
  }

  /// Increment the seen count of a key
  fn incr_seenkey(&mut self, key: &str) {
    match self.seenkeys.get_mut(key) {
      Some(seenkey) => *seenkey += 1,
      None => {
        self.seenkeys.insert(key.into(), 0);
      }
    }
  }

  /// Reset section caches which need it
  fn reset_caches(&mut self) {
    self.labelcache_l.clear();
    self.labelcache_v.clear();
    //self.bcfkeycache.clear();
  }

  /// Check and record max namepart length. Needed to construct sort keys for names
  fn set_np_length(&mut self, np: &str, len: Option<u32>) {
    if let Some(len) = len {
      if len > self.namepartlengths.get(np).copied().unwrap_or(0) {
        self.namepartlengths.insert(np.into(), len);
      }
    }
  }

  /// Return max namepart length. Needed to construct sort keys for names
  fn get_np_length(&self, np: &str) -> u32 {
    self.namepartlengths.get(np).copied().unwrap_or(0)
  }

  /// Record a parent->child set relationship
  fn set_set_pc(&mut self, parent: &str, child: &str) {
    match self.state_set.pc.get_mut(parent) {
      Some(parent) => {
        parent.insert(child.into());
      }
      None => {
        self.state_set.pc.insert(parent.into(),  HashSet::from([child.into()]));
      }
    }
  }

  /// Record a child->parent set relationship
  fn set_set_cp(&mut self, child: &str, parent: &str) {
    match self.state_set.cp.get_mut(child) {
      Some(child) => {
        child.insert(parent.into());
      }
      None => {
        self.state_set.cp.insert(child.into(),  HashSet::from([parent.into()]));
      }
    }
  }

  /// Return a boolean saying if there is a parent->child set relationship
  fn get_set_pc(&self, parent: &str, child: &str) -> bool {
    match self.state_set.pc.get(parent) {
      Some(map) => map.contains(child),
      None => false,
    }
  }

  /// Return a boolean saying if there is a child->parent set relationship
  fn get_set_cp(&self, child: &str, parent: &str) -> bool {
    match self.state_set.cp.get(child) {
      Some(map) => map.contains(parent),
      None => false,
    }
  }

  /// Return a list of children for a parent set
  fn get_set_children(&self, parent: &str) -> Vec<&String> {
    match self.state_set.pc.get(parent) {
      Some(map) => map.iter().collect(),
      None => Vec::new(),
    }
  }

  /// Return a list of parents for a child of a set
  fn get_set_parents(&self, child: &str) -> Vec<&String> {
    match self.state_set.cp.get(child) {
      Some(map) => map.iter().collect(),
      None => Vec::new(),
    }
  }

  ///  Save information about citekey->datasource name mapping. Used for error reporting.
  fn set_keytods(&mut self, key: &str, ds: &Path) {
    self.keytods.insert(key.into(), ds.into());
  }

  /// Get information about citekey->datasource name mapping. Used for error reporting.
  fn get_keytods(&self, key: &str) -> Option<&PathBuf> {
    self.keytods.get(key)
  }

  /// Returns a value to say if we've seen a key differing only in case before
  /// <previouskey>  - we've seen a differently cased variant of this key so we can warn about this
  /// undef  - Not seen this key at all in any case variant before
  fn has_badcasekey(&self, key: &str) -> Option<&String> {
    self.everykey_lc.get(&key.to_lowercase()).filter(|&ckey| ckey != key)
  }

  /// Check if a key is specifically cited by \cite{key} or \nocite{key}
  fn is_specificcitekey(&self, key: &str) -> bool {
    self.cite_citekeys.contains(key) || self.nocite_citekeys.contains(key)
  }

  /// Record that a key is used as a related entry
  fn add_related(&mut self, key: &str) {
    self.relkeys.insert(key.into());
  }

  /// Check if a key is used as a related entry key
  fn is_related(&self, key: &str) -> bool {
    self.relkeys.contains(key)
  }

  /// Record a key<->clone key mapping.
  fn keytorelclone(&mut self, key: &str, clonekey: &str) {
    self.keytofromrelclone.insert(key.into(), clonekey.into());
  }

  /// Fetch a related entry clone key, given a cite key
  fn get_keytorelclone(&self, key: &str) -> Option<&String> {
    self.keytofromrelclone.get_by_left(key)
  }

  /// Fetch a related entry key, given a clone key
  fn get_relclonetokey(&self, key: &str) -> Option<&String> {
    self.keytofromrelclone.get_by_right(key)
  }

  /// Return boolean saying if a cite key has a related entry clone in the current section
  fn has_keytorelclone(&self, key: &str) -> bool {
    self.keytofromrelclone.contains_left(key)
  }

  /// Return boolean saying if a related clone key has a citekey in the current section
  fn has_relclonetokey(&self, key: &str) -> bool {
    self.keytofromrelclone.contains_right(key)
  }

  /// Adds a key to the list of those that came via \cite
  fn add_cite(&mut self, key: &str) {
    self.cite_citekeys.insert(key.into());
    return;
  }

  /// Returns a boolean to say if a key came via \cite
  fn is_cite(&self, key: &str) -> bool {
    self.cite_citekeys.contains(key)
  }

  /// Adds a key to the list of those that came via \nocite
  fn add_nocite(&mut self, key: &str) {
    self.nocite_citekeys.insert(key.into());
    return;
  }

  /// Returns a boolean to say if a key came via \nocite
  fn is_nocite(&self, key: &str) -> bool {
    self.nocite_citekeys.contains(key)
  }

  /// Adds a datasource key to the section list of all datasource keys
  fn add_everykey(&mut self, key: &str) {
    self.everykey.insert(key.into());
    self.everykey_lc.insert(key.to_lowercase(), key.into());
  }

  /// Delete everykey cache. For use in tests.
  fn del_everykeys(&mut self) {
    self.everykey.clear();
    self.everykey_lc.clear();
  }

  /// Returns a boolean to say if we've seen a key in any datasource for this section.
  /// This used to be an array ref which was checked using first() and it
  /// was twenty times slower.
  fn has_everykey(&self, key: &str) -> bool {
    self.everykey.contains(key)
  }

  /// Sets flag to say citekey '*' occurred through \nocite
  /// We allow setting it to false too because it's useful in tests
  fn set_allkeys_nocite(&mut self, val: bool) {
    self.allkeysnocite = val;
  }

  /// Sets flag to say citekey '*' occurred in citekeys
  /// We allow setting it to false too because it's useful in tests
  fn set_allkeys(&mut self, val: bool) {
    self.allkeys = val;
  }

  /// Checks flag which says citekey '*' occurred in via \nocite
  fn is_allkeys_nocite(&self) -> bool {
    self.allkeysnocite
  }

  /// Checks flag which says citekey '*' occurred in citekeys
  fn is_allkeys(&self) -> bool {
    self.allkeys
  }

  /// Returns a crate::Entry object for the given citation key
  /// Understands citekey aliases
  fn bibentry(&self, key: &str) -> Option<&Entry> {
    if let Some(realkey) = self.get_citekey_alias(key) {
      self.bibentries.entry(realkey)
    } else {
      self.bibentries.entry(key)
    }
  }

  /// Return crate::Entries object for this section
  fn bibentries(&self) -> &Entries {
    &self.bibentries
  }

  /// Delete all crate::Entry objects from the crate::Section object
  fn del_bibentries(&mut self) {
    self.bibentries = crate::Entries::new();
  }

  /// Sets the citekeys in a crate::Section object
  fn set_citekeys(&mut self, keys: &[&str]) {
    for &key in keys {
      self.citekeys_h.insert(key.into());
    }
    self.citekeys = keys.iter().map(|&s| s.to_string()).collect();
  }

  /// Sets the original order of citekeys in a crate::Section object
  fn set_orig_order_citekeys(&mut self, keys: &[&str]) {
    self.orig_order_citekeys = keys.iter().map(|&s| s.to_string()).collect();
  }

  /// Gets the citekeys of a crate::Section object
  /// Returns a normal array
  fn get_citekeys(&self) -> &Vec<String> {
    &self.citekeys
  }

  /// Gets the citekeys of a crate::Section object
  /// excluding dynamic set entry keys
  /// Returns a normal array
  fn get_static_citekeys(&self) -> Vec<&String> {
    return reduce_array(self.citekeys.iter(), self.dynamic_set_keys());
  }

  /// Returns true when $key was one of the actually cited keys in the section
  fn has_cited_citekey(&self, key: &str) -> bool {
    self.citekeys_h.contains(key)
  }

  /// Adds a citekey to the crate::Section object as an undefined
  /// key. This allows us to output this information to the .bbl and
  /// so biblatex can do better reporting to external utils like latexmk
  fn add_undef_citekey(&mut self, key: &str) {
    self.undef_citekeys.push(key.into());
  }

  /// Gets the list of undefined citekeys of a crate::Section object
  /// Returns a normal array
  fn get_undef_citekeys(&self) -> &Vec<String> {
    &self.undef_citekeys
  }

  /// Gets the citekeys of a crate::Section object in their original order
  /// This is just to ensure we have a method that will return this, just in
  /// case we mess about with the order at some point. This is needed by
  /// citeorder sorting.
  fn get_orig_order_citekeys(&self) -> &Vec<String> {
    &self.orig_order_citekeys
  }

  /// Returns true when $key is in the crate::Section object
  /// Understands key alaises
  fn has_citekey(&self, key: &str) -> bool {
    let key = self.get_citekey_alias(key).skip_empty().map(|s| s.as_str()).unwrap_or(key);
    self.citekeys_h.contains(key)
  }

  /// Deletes a citekey from a crate::Section object
  fn del_citekey(&mut self, key: &str) {
    if !self.has_citekey(key) {
      return;
    }
    self.citekeys.retain(|k| k != key);
    self.orig_order_citekeys.retain(|k| k != key);
    self.citekeys_h.remove(key);
  }

  /// Deletes all citekeys from a crate::Section object
  fn del_citekeys(&mut self) {
    self.citekeys.clear();
    self.citekeys_h.clear();
    self.orig_order_citekeys.clear();
  }

  /// Adds citekeys to the crate::Section object
  fn add_citekeys(&mut self, keys: &[String]) {
    for key in keys {
      if self.has_citekey(key) {
        continue;
      }
      self.citekeys_h.insert(key.into());
      self.citekeys.push(key.clone());
      self.orig_order_citekeys.push(key.clone());
    }
    return;
  }

  /// Set citekey alias information
  fn set_citekey_alias(&mut self, alias: &str, key: &str) {
    self.citekey_alias.insert(alias.into(), key.into());
  }

  /// Get citekey alias information
  fn get_citekey_alias(&self, alias: &str) -> Option<&String> {
    self.citekey_alias.get(alias)
  }

  /// Delete citekey alias
  fn del_citekey_alias(&mut self, alias: &str) {
    self.citekey_alias.remove(alias);
  }

  /// Get a list of all citekey aliases for the section
  fn get_citekey_aliases(&self) -> impl Iterator<Item=&String> {
    self.citekey_alias.keys()
  }

  /// Sets the variable label disambiguation cache for a field
  fn set_labelcache_v(&mut self, field: &str, cache: Unknown) {
    self.labelcache_v.insert(field.into(), cache);
  }

  /// Gets the variable label disambiguation cache for a field
  fn get_labelcache_v(&self, field: &str) -> Option<&Unknown> {
    self.labelcache_v.get(field)
  }

  /// Sets the list label disambiguation cache for a field
  fn set_labelcache_l(&mut self, field: &str, cache: Unknown) {
    self.labelcache_l.insert(field.into(), cache);
  }

  /// Gets the list label disambiguation cache for a field
  fn get_labelcache_l(&self, field: &str) -> Option<&Unknown> {
    self.labelcache_l.get(field)
  }

  /// Test if a key is a dynamic set
  fn is_dynamic_set(&self, dkey: &str) -> bool {
    self.dkeys.contains_key(dkey)
  }

  /// Record a mapping of dynamic key to member keys
  fn set_dynamic_set<'a>(&mut self, dkey: &str, members: impl Iterator<Item=&'a str>) {
    self.dkeys.insert(dkey.into(), members.map(|s| s.to_string()).collect());
  }

  /// Retrieve member keys for a dynamic set key
  /// Check that reference returning anything to stop spurious warnings
  /// about empty dereference in return.
  fn get_dynamic_set(&self, dkey: &str) -> &[String] {
    if let Some(set_members) = self.dkeys.get(dkey).skip_empty() {
      set_members
    } else {
      &[]
    }
  }

  /// Retrieve all dynamic set keys
  fn dynamic_set_keys(&self) -> impl Iterator<Item=&String> {
    self.dkeys.keys()
  }

  /// Returns true of false depending on whether the section has any dynamic set keys
  fn has_dynamic_sets(&self) -> bool {
    !self.dkeys.is_empty()
  }

  /// Adds a data source to a section
  fn add_datasource(&mut self, source: DataSource) {
    match &mut self.datasources {
      Some(ds) => ds.push(source),
      None => self.datasources = Some(vec![source]),
    }
  }

  /// Sets the data sources for a section
  fn set_datasources(&mut self, sources: Vec<DataSource>) {
    self.datasources = Some(sources);
  }

  /// Gets an array of data sources for this section
  fn get_datasources(&self) -> &Option<Vec<DataSource>> {
    &self.datasources
  }

  /// Gets the section number of a crate::Section object
  fn number(&self) -> u32 {
    self.number
  }
}