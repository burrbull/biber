//! `Section` objects
use std::{
  collections::{HashMap, HashSet},
  path::{Path, PathBuf},
};

use crate::Entries;
use crate::Utils;
use List::Util qw( first );

pub struct Section;

impl Section {
  /// Initialize a crate::Section object
  fn new(number: u32) -> Self {
    Self {
      number,
      bibentries: crate::Entries::new(),
      namepartlengths: HashMap::<String, u32>::new(),
      keytorelclone: {},
      relclonetokey: {},
      relkeys: HashSet::<String>::new(),
      allkeys: false,
      allkeysnocite: false,
      citekeys: Vec::<String>::new(),
      citekeys_h: HashSet::<String>::new(), // For faster hash-based lookup of individual keys
      labelcache_l: {},
      everykey: {},
      everykey_lc: HashMap::<String, String>::new(),
      bcfkeycache: {},
      labelcache_v: {},
      dkeys: {},
      keytods: HashMap::<String, PathBuf>::new(),
      orig_order_citekeys: Vec::<String>::new(),
      undef_citekeys: [],
      cite_citekeys: HashSet::<String>::new(),
      nocite_citekeys: HashSet::<String>::new(),
      citekey_alias: HashMap::<String, String>::new(),
      static_keys: {},
      state: {},
      seenkeys: {},
      citecount: Hash::<String, u32>::new(),
    }
  }

  /// Set the citecount of a key. This comes from biblatex via the
  /// citecounter option and reflects the actual number of citations using
  /// this key, taking into account things like \citeauthor etc. which are not
  /// real citations.
  fn set_citecount(&mut self, key: &str, count: u32) {
    self.citecount.insert(key.into(), count)
  }

  /// Get the citecount of a key. This comes from biblatex via the
  /// citecounter option and reflects the actual number of citations using
  /// this key, taking into account things like \citeauthor etc. which are not
  /// real citations. A zero or undef value needs to be less than 0 which does
  /// not fail if() checks - required for the delicate sorting dispatch logic
  fn get_citecount(& self, key: &str) -> i32 {
    self.citecount.get(key).map(|n| n as i32).filter(|n| n > 0).unwrap_or(-1)
  }

  /// Get the count of a key
  fn get_seenkey(self, $key) {
    return $self->{seenkeys}{$key};
  }

  /// Increment the seen count of a key
  fn incr_seenkey(self, $key) {
    $self->{seenkeys}{$key}++;
    return;
  }

  /// Reset section caches which need it
  fn reset_caches(&mut self) {
    self.labelcache_l.clear();
    self.labelcache_v.clear();
    self.bcfkeycache.clear();
  }

  /// Check and record max namepart length. Needed to construct sort keys for names
  fn set_np_length(self, $np, $len) {
    if !(defined $len) {
      return;
    }
    if ($len > ($self->{namepartlengths}{$np}.unwrap_or(0))) {
      $self->{namepartlengths}{$np} = $len;
    }
    return;
  }

  /// Return max namepart length. Needed to construct sort keys for names
  fn get_np_length(self, np: &str) -> u32 {
    self.namepartlengths.get(np).unwrap_or(0);
  }

  /// Record a parent->child set relationship
  fn set_set_pc(self, $parent, $child) {
    $self->{state}{set}{pc}{$parent}{$child} = true;
    return;
  }

  /// Record a child->parent set relationship
  fn set_set_cp(self, $child, $parent) {
    $self->{state}{set}{cp}{$child}{$parent} = true;
    return;
  }

  /// Return a boolean saying if there is a parent->child set relationship
  fn get_set_pc(self, $parent, $child) -> bool {
    return exists($self->{state}{set}{pc}{$parent}{$child}) ? 1 : 0;
  }

  /// Return a boolean saying if there is a child->parent set relationship
  fn get_set_cp(self, $child, $parent) -> bool {
    return exists($self->{state}{set}{cp}{$child}{$parent}) ? 1 : 0;
  }

  /// Return a list of children for a parent set
  fn get_set_children(self, $parent) {
    if (exists($self->{state}{set}{pc}{$parent})) {
      return (keys $self->{state}{set}{pc}{$parent}->%*);
    }
    else {
      return ();
    }
  }

  /// Return a list of parents for a child of a set
  fn get_set_parents(self, $child) {
    if (exists($self->{state}{set}{cp}{$child})) {
      return (keys $self->{state}{set}{cp}{$child}->%*);
    }
    else {
      return ();
    }
  }

  ///  Save information about citekey->datasource name mapping. Used for error reporting.
  fn set_keytods(&mut self, key: &str, ds: &Path) {
    self.keytods.insert(key.into(), ds.into());
  }

  /// Get information about citekey->datasource name mapping. Used for error reporting.
  fn get_keytods(&self, key: &str) -> &Option<PathBuf> {
    self.keytods.get(key);
  }

  /// Returns a value to say if we've seen a key differing only in case before
  /// <previouskey>  - we've seen a differently cased variant of this key so we can warn about this
  /// undef  - Not seen this key at all in any case variant before
  fn has_badcasekey(&self, key: &str) -> &Option<String> {
    self.everykey_lc.get(&key.to_lowercase()).filter(|ckey| ckey != key)
  }

  /// Check if a key is specifically cited by \cite{key} or \nocite{key}
  fn is_specificcitekey(&self, key: &str) -> bool {
    self.cite_citekeys.contains(key) || self.nocite_citekeys.contains(key)
  }

  /// Record that a key is used as a related entry
  fn add_related(&mut self, key: &str) {
    self.relkeys.insert(key.into())
  }

  /// Check if a key is used as a related entry key
  fn is_related(self, key: &str) -> bool {
    self.relkeys.contains(key)
  }

  /// Record a key<->clone key mapping.
  fn keytorelclone(self, $key, $clonekey) {
    $self->{keytorelclone}{$key} = $clonekey;
    $self->{relclonetokey}{$clonekey} = $key;
    return;
  }

  /// Fetch a related entry clone key, given a cite key
  fn get_keytorelclone(self, $key) {
    return $self->{keytorelclone}{$key};
  }

  /// Fetch a related entry key, given a clone key
  fn get_relclonetokey(self, $key) {
    return $self->{relclonetokey}{$key};
  }

  /// Return boolean saying if a cite key has a related entry clone in the current section
  fn has_keytorelclone(self, $key) {
    return defined($self->{keytorelclone}{$key}) ? 1 : 0;
  }

  /// Return boolean saying if a related clone key has a citekey in the current section
  fn has_relclonetokey(self, $key) {
    return defined($self->{relclonetokey}{$key}) ? 1 : 0;
  }

  /// Adds a key to the list of those that came via \cite
  fn add_cite(self, $key) {
    self.cite_citekeys.insert(key.into());
    return;
  }

  /// Returns a boolean to say if a key came via \cite
  fn is_cite(&self, key: &str) -> bool {
    self.cite_citekeys.contains_key(key)
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
  fn add_everykey(self, $key) {
    $self->{everykey}{$key} = 1;
    $self->{everykey_lc}{lc($key)} = $key;
    return;
  }

  /// Delete everykey cache. For use in tests.
  fn del_everykeys(self) {
    $self->{everykey} = undef;
    $self->{everykey_lc} = undef;
    return;
  }

  /// Returns a boolean to say if we've seen a key in any datasource for this section.
  /// This used to be an array ref which was checked using first() and it
  /// was twenty times slower.
  fn has_everykey(self, $key) {
    return $self->{everykey}{$key} ? 1 : 0;
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
  fn bibentry(self, $key) {
    if (let $realkey = $self->get_citekey_alias($key)) {
      return $self->bibentries->entry($realkey);
    }
    else {
      return $self->bibentries->entry($key);
    }
  }

  /// Return crate::Entries object for this section
  fn bibentries(self) {
    return $self->{bibentries};
  }

  /// Delete all crate::Entry objects from the crate::Section object
  fn del_bibentries(self) {
    $self->{bibentries} = new crate::Entries;
    return;
  }

  /// Sets the citekeys in a crate::Section object
  fn set_citekeys(self, keys: &[&str]) {
    for key in keys {
      self.citekeys_h.insert(key);
    }
    self.citekeys = keys.iter().map(|s| s.to_string()).collect();
  }

  /// Sets the original order of citekeys in a crate::Section object
  fn set_orig_order_citekeys(&mut self, &[&str]) {
    self.orig_order_citekeys = keys.iter().map(|s| s.to_string()).collect();
  }

  /// Gets the citekeys of a crate::Section object
  /// Returns a normal array
  fn get_citekeys(&self) -> &Vec<String> {
    &self.citekeys
  }

  /// Gets the citekeys of a crate::Section object
  /// excluding dynamic set entry keys
  /// Returns a normal array
  fn get_static_citekeys(self) {
    return reduce_array($self->{citekeys}, $self->dynamic_set_keys);
  }

  /// Returns true when $key was one of the actually cited keys in the section
  fn has_cited_citekey(self, key) {
    return $self->{citekeys_h}{$key} ? 1 : 0;
  }

  /// Adds a citekey to the crate::Section object as an undefined
  /// key. This allows us to output this information to the .bbl and
  /// so biblatex can do better reporting to external utils like latexmk
  fn add_undef_citekey(self, key) {
    push $self->{undef_citekeys}->@*, $key;
    return;
  }

  /// Gets the list of undefined citekeys of a crate::Section object
  /// Returns a normal array
  fn get_undef_citekeys(self) {
    return $self->{undef_citekeys}->@*;
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
    self.citekeys_h.contains(self.get_citekey_alias(key).unwrap_or(key))
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
  fn del_citekeys(self) {
    self.citekeys.clear();
    self.citekeys_h.clear();
    self.orig_order_citekeys.clear();
  }

  /// Adds citekeys to the crate::Section object
  fn add_citekeys(self, keys: &[String]) {
    for key in keys {
      if self.has_citekey(key) {
        continue;
      }
      self.citekeys_h.insert(key);
      self.citekeys.push(key.clone());
      self.orig_order_citekeys.push(key.clone());
    }
    return;
  }

  /// Set citekey alias information
  fn set_citekey_alias(&mut self, alias: &str, key: &str) {
    self.citekey_alias.insert(alias.into(), key.into())
  }

  /// Get citekey alias information
  fn get_citekey_alias(&self, alias: &str) -> &Option<String> {
    self.citekey_alias.get(alias)
  }

  /// Delete citekey alias
  fn del_citekey_alias(&mut self, alias: &str) {
    self.citekey_alias.remove(alias);
  }

  /// Get a list of all citekey aliases for the section
  fn get_citekey_aliases(&self) -> impl Iterator<&String> {
    citekey_alias.keys()
  }

  /// Sets the variable label disambiguation cache for a field
  fn set_labelcache_v(self, $field, $cache) {
    $self->{labelcache_v}{$field} = $cache;
    return;
  }

  /// Gets the variable label disambiguation cache for a field
  fn get_labelcache_v(self, $field) {
    return $self->{labelcache_v}{$field};
  }

  /// Sets the list label disambiguation cache for a field
  fn set_labelcache_l(self, $field, $cache) {
    $self->{labelcache_l}{$field} = $cache;
    return;
  }

  /// Gets the list label disambiguation cache for a field
  fn get_labelcache_l(self, $field) {
    return $self->{labelcache_l}{$field};
  }

  /// Test if a key is a dynamic set
  fn is_dynamic_set(self, dkey) {
    return defined($self->{dkeys}{$dkey}) ? 1 : 0;
  }

  /// Record a mapping of dynamic key to member keys
  fn set_dynamic_set(self, dkey, @members) {
    $self->{dkeys}{$dkey} = \@members;
    return;
  }

  /// Retrieve member keys for a dynamic set key
  /// Check that reference returning anything to stop spurious warnings
  /// about empty dereference in return.
  fn get_dynamic_set(self, dkey) {
    if (let $set_members = $self->{dkeys}{$dkey}) {
      return $set_members->@*;
    }
    else {
      return ();
    }
  }

  /// Retrieve all dynamic set keys
  fn dynamic_set_keys(self) {
    return [keys $self->{dkeys}->%*];
  }

  /// Returns true of false depending on whether the section has any dynamic set keys
  fn has_dynamic_sets(self) {
    return defined($self->{dkeys}) ? 1 : 0;
  }

  /// Adds a data source to a section
  fn add_datasource(self, source) {
    push $self->{datasources}->@*, $source;
    return;
  }

  /// Sets the data sources for a section
  fn set_datasources(self, sources) {
    $self->{datasources} = $sources;
    return;
  }

  /// Gets an array of data sources for this section
  fn get_datasources(self) {
    if (exists($self->{datasources})) {
      return $self->{datasources};
    }
    else {
      return undef;
    }
  }

  /// Gets the section number of a crate::Section object
  fn number(&self) {
    self.number
  }
}