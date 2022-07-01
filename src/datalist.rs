//! `DataLists` objects

use crate::Utils;
use crate::Constants;
use Data::Compare;
use Digest::MD5 qw( md5_hex );
use List::Util qw( first );

struct State {
  seenpa: HashMap<String, HashSet<String>>,
}

pub struct DataList {
  section: u32,
  name: String,
  typ: String,
  sortingtemplatename: String,
  sortingnamekeytemplatename: String,
  uniquenametemplatename: Option<String>,
  labelalphanametemplatename: String,
  labelprefix: String,
  state: State,
  //
}

impl DataList {
  /// Initialize a crate::DataList object
  pub fn new(
    section: u32,
    name: &str,
    typ: &str,
    sortingtemplatename: &str,
    sortingnamekeytemplatename: &str,
    uniquenametemplatename: Option<String>,
    labelalphanametemplatename: &str,
    labelprefix: &str,
  ) -> Self {
    Self {
      section,
      name: name.into(),
      typ: typ.into(),
      sortingtemplatename: sortingtemplatename.into(),
      sortingnamekeytemplatename: sortingnamekeytemplatename.into(),
      uniquenametemplatename,
      labelalphanametemplatename: labelalphanametemplatename.into(),
      labelprefix: labelprefix.into(),
      state: State::new(),
    }
  }

  /// Sets the section of a data list
  fn set_section(&mut self, section: u32) {
    self.section = section;//lc($section)
  }

  /// Gets the section of a data list
  fn get_section(&self) -> u32 {
    self.section
  }

  /// Resets all state data. Used mainly in tests which call crate::prepare()
  /// multiple times without re-creating datalists
  fn reset_state(&mut self) {
    self.state.clear();
  }

  /// Increment the count of occurrences of a primary author base name
  /// if it has a different non-base part. How many variants of the basename
  /// are there in the dlist?
  fn incr_seenpa(&mut self, base: &str, hash: &str) {
    // increment the number of base variants
    match self.state.seenpa.get_mut(base) {
      Some(v) => v.insert(hash),
      None => {
        let mut s = HashSet::new();
        s.insert(hash);
        self.state.seenpa.insert(base.into(), s);
      }
  }

  /// Get the count of unique (i.e. with different hash) occurrences of a primary
  /// author base name
  fn get_seenpa(&self, base: &str) -> u32 {
    self.state.seenpa.get(base).map(|m| m.len() as u32).unwrap_or(0)
  }

  /// Resets all entryfield data in a list
  fn reset_entryfields(&mut self) {
    self.state.fields.clear();
  }

  /// Retrieves per-list datafield information for an entry
  fn get_entryfield(&self, citekey: &str, f: &str) -> Option<Unknown> {
    return self.state.fields{$citekey}{$f};
  }

  /// Records per-list datafield information for an entry
  fn set_entryfield(&mut self, citekey: &str, $f, $v) {
    $self->{state}{fields}{$citekey}{$f} = $v;
    return;
  }

  /// Add a name to the list of name contexts which have the name in it
  /// (only called for visible names)
  fn add_uniquenamecount(&mut self, $name, $namecontext, $key) {
    $self->{state}{uniquenamecount}{$name}{$namecontext}{$key}++;
    return;
  }

  /// Add a name to the list of name contexts which have the name in it
  /// (called for all names)
  fn add_uniquenamecount_all(&mut self, $name, $namecontext, $key) {
    $self->{state}{uniquenamecount_all}{$name}{$namecontext}{$key}++;
    return;
  }

  /// Get the number of uniquelist entries for a (possibly partial) list
  fn get_uniquelistcount(self, $namelist) {
    return $self->{state}{uniquelistcount}{global}{join("\x{10FFFD}", $namelist->@*)};
  }

  /// Increment the count for a list part to the data for a name
  fn add_uniquelistcount(self, $namelist) {
    $self->{state}{uniquelistcount}{global}{join("\x{10FFFD}", $namelist->@*)}++;
    return;
  }

  /// Increment the count for a complete list to the data for a name
  fn add_uniquelistcount_final(self, $namelist, $labelyear) {
    $self->{state}{uniquelistcount}{global}{final}{join("\x{10FFFD}", $namelist->@*)}++;
    if ($labelyear) { // uniquelist=minyear
      $self->{state}{uniquelistcount}{global}{final}{$labelyear}{join("\x{10FFFD}", $namelist->@*)}++;
    }
    return;
  }

  /// Increment the count for a list and year for a name
  /// Used to track uniquelist = minyear
  fn add_uniquelistcount_minyear(self, $minyearnamelist, $year, $namelist) {
    // Allow year a default in case labelyear is undef
    $self->{state}{uniquelistcount}{minyear}{join("\x{10FFFD}", $minyearnamelist->@*)}{$year.unwrap_or("0")}{join("\x{10FFFD}", $namelist->@*)}++;
    return;
  }

  /// Get the count for a list and year for a name
  /// Used to track uniquelist = minyear
  fn get_uniquelistcount_minyear(self, $minyearnamelist, $year) {
    return scalar keys $self->{state}{uniquelistcount}{minyear}{join("\x{10FFFD}", $minyearnamelist->@*)}{$year.uniquework("0")}->%*;
  }

  /// Get the number of uniquelist entries for a full list
  fn get_uniquelistcount_final(self, $namelist) {
    let $c = $self->{state}{uniquelistcount}{global}{final}{join("\x{10FFFD}", $namelist->@*)};
    return $c.unwrap_or(0);
  }

  /// Reset the count for list parts and complete lists
  fn reset_uniquelistcount(self) {
    $self->{state}{uniquelistcount} = {};
    return;
  }

  /// Reset the list of names which have the name part in it
  fn reset_uniquenamecount(&mut self) {
    self.state.uniquenamecount.clear();
    self.state.uniquenamecount_all.clear();
  }

  /// Get a basenamestring for a particular name
  fn get_basenamestring(&self, nlid: Id, nid: Id) {
    return $self->{state}{namelistdata}{$nlid}{$nid}{basenamestring};
  }

  /// Get a namestring for a particular name
  fn get_namestring(&self, nlid: Id, nid: Id) {
    return $self->{state}{namelistdata}{$nlid}{$nid}{namestring};
  }

  /// Get namestrings for a particular name
  fn get_namestrings(&self, nlid: Id, nid: Id) {
    return $self->{state}{namelistdata}{$nlid}{$nid}{namestrings};
  }

  /// Set name disambiguation metadata
  fn set_namedis(&mut self, nlid: Id, nid: Id, $ns, $nss, $nds) {
    $self->{state}{namelistdata}{$nlid}{$nid}{namestring} = $ns;
    $self->{state}{namelistdata}{$nlid}{$nid}{namestrings} = $nss;

    for (i, se) in nds.iter().enumerate() {
      // make these explicit for faster lookup since they are static
      if se[0] == "base" {
        $self->{state}{namelistdata}{$nlid}{$nid}{basenamestring} = nss[i];
        $self->{state}{namelistdata}{$nlid}{$nid}{basenamestringparts} = se[1];
        break;
      }
    }

    $self->{state}{namelistdata}{$nlid}{$nid}{namedisschema} = $nds;
    return;
  }

  /// Return boolean to say if a namepart is a base part according to
  /// template which created the information
  fn is_unbasepart(&self, nlid: Id, nid: Id, $np) -> bool {
    if (first {$_ == $np} $self->{state}{namelistdata}{$nlid}{$nid}{basenamestringparts}->@*) {
      return true;
    }
    else {
      return false;
    }
  }

  /// Get hash for a name
  fn get_namehash(&self, nlid: Id, nid: Id) {
    return $self->{state}{namelistdata}{$nlid}{$nid}{hash};
  }

  /// Set hash for a name
  fn set_namehash(&mut self, nlid: Id, nid: Id, s: &str) {
    $self->{state}{namelistdata}{$nlid}{$nid}{hash} = $s;
    return;
  }

  /// Get uniquename minimalness info for a name
  fn get_unmininfo(&self, nlid: Id, nid: Id) {
    return $self->{state}{namelistdata}{$nlid}{$nid}{unmininfo};
  }

  /// Set uniquename minimalness info for a name
  fn set_unmininfo(&mut self, nlid: Id, nid: Id, $s) {
    $self->{state}{namelistdata}{$nlid}{$nid}{unmininfo} = $s;
    return;
  }

  /// Get a name disambiguation schema for a name
  fn get_namedisschema(&self, nlid: Id, nid: Id) {
    self.state.namelistdata{$nlid}{$nid}.namedisschema;
  }

  /// Get legacy uniquename summary for a name
  fn get_unsummary(&self, nlid: Id, nid: Id) -> Option<u32> {
    let $un = $self->{state}{namelistdata}{$nlid}{$nid}{un};
    if !defined($un) {
      return None;
    }
    if ($un->[1] == "none" || $un->[0] == "base") {
      return Some(0);
    }
    else if ($un->[1] == "init") {
      return Some(1);
    }
    else if ($un->[1] == "full" || $un->[1] == "fullonly") {
      return Some(2);
    }
    return Some(0);
  }

  /// Get uniquename summary part for a name
  fn get_unpart(&self, nlid: Id, nid: Id) {
    let $un = $self->{state}{namelistdata}{$nlid}{$nid}{un};
    if !defined($un) {
      return undef;
    }
    return $un->[0]
  }

  /// Get uniquename parts for a name
  fn get_unparts(&self, nlid: Id, nid: Id, $np) {
    return $self->{state}{namelistdata}{$nlid}{$nid}{unparts}{$np};
  }

  /// Set uniquename parts for a name
  fn set_unparts(&mut self, nlid: Id, nid: Id, $np, $s) {
    $self->{state}{namelistdata}{$nlid}{$nid}{unparts}{$np} = $s;
    return;
  }

  /// Get the list of name contexts which contain a name
  /// Mainly for use in tests
  fn _get_uniquename(&self, $name, $namecontext) {
    let @list = sort keys $self->{state}{uniquenamecount}{$name}{$namecontext}->%*;
    return \@list;
  }

  /// Get uniquename for a name
  fn get_uniquename(&self, nlid: Id, nid: Id) -> Option<Unknown> {
    return $self->{state}{namelistdata}{$nlid}{$nid}{un};
  }

  /// Set uniquename for a name
  fn set_uniquename(&mut self, nlid: Id, nid: Id, s: Unknown) {

    let currval = $self->{state}{namelistdata}{$nlid}{$nid}{un};
    // Set modified flag to positive if we changed something
    if (currval.is_none() || !Compare($currval, $s)) {
      self.set_unul_changed(true);
    }
    $self->{state}{namelistdata}{$nlid}{$nid}{un} = Some(s);
    return;
  }

  /// Reset uniquename for a name
  fn reset_uniquename(&mut self, nlid: Id, nid: Id) {
    $self->{state}{namelistdata}{$nlid}{$nid}{un} = ["base", $self->{state}{namelistdata}{$nlid}{$nid}{basenamestringparts}];
    return;
  }

  /// Get uniquename for a name, regardless of visibility
  fn get_uniquename_all(&self, nlid: Id, nid: Id) -> Option<Unknown> {
    return $self->{state}{namelistdata}{$nlid}{$nid}{unall};
  }

  /// Set uniquename for a name, regardless of visibility
  fn set_uniquename_all(&mut self, nlid: Id, nid: Id, s) {
    $self->{state}{namelistdata}{$nlid}{$nid}{unall} = $s;
    return;
  }

  /// Count the names in a string used to determine uniquelist.
  fn count_uniquelist(&self, namelist: Unknown) -> usize {
    namelist.len()
  }

  /// Gets a uniquelist setting for a namelist
  fn get_uniquelist(&self, nlid: Id) -> Option<Unknown> {
    return $self->{state}{namelistdata}{$nlid}{ul};
  }

  /// Sets a uniquelist setting for a namelist
  fn set_uniquelist(self, nl, namelist, labelyear, ul, maxcn, mincn) {
    // $nl is the namelist object
    // $namelist is the extracted string concatenation from $nl which forms the tracking key
    let nlid = nl.get_id();
    let uniquelist = self.count_uniquelist(namelist);
    let num_names = nl.count();
    let $currval = $self->{state}{namelistdata}{$nlid}{ul};

    // Set modified flag to positive if we changed something
    if (!defined($currval) || $currval != $uniquelist) {
      self.set_unul_changed(true);
    }

    // Special case $uniquelist <=1 is meaningless
    if uniquelist <= 1 {
      return;
    }

    // Don't set uniquelist unless the list is longer than maxcitenames as it was therefore
    // never truncated to mincitenames in the first place and uniquelist is a "local mincitenames"
    if !(num_names > maxcn) {
      return;
    }

    // No disambiguation needed if uniquelist is <= mincitenames as this makes no sense
    // since it implies that disambiguation beyond mincitenames was needed.
    // This doesn't apply when the list length is mincitenames as maxmanes therefore
    // (since it can't be less than mincitenames) could also be the same as the list length
    // and this is a special case where we need to preserve uniquelist (see comments in
    // create_uniquelist_info())
    // $uniquelist cannot be undef or 0 either since every list occurs at least once.
    // This guarantees that uniquelist, when set, is >1 because mincitenames cannot
    // be <1
    if uniquelist <= mincn && mincn != num_names {
      return;
    }

    // Special case
    // No point disambiguating with uniquelist lists which have the same count
    // for the complete list as this means they are the same list. So, if this
    // is the case, don't set uniquelist at all.
    // BUT, this only applies if there is nothing else which these identical lists
    // need disambiguating from so check if there are any other lists which differ
    // up to any index. If there is such a list, set uniquelist using that index.

    // if final count > 1 (identical lists)
    if self.get_uniquelistcount_final(namelist) > 1 {
      // index where this namelist begins to differ from any other
      // Can't be 0 as that means it begins differently in which case $index is undef
      let index = self.namelist_differs_index(namelist);
      if !index {
        return;
      }
      // Now we know that some disambiguation is needed from other similar list(s)
      uniquelist = index+1;// convert zero-based index into 1-based uniquelist value
    }
    // this is an else if because for final count > 1, we are setting uniquelist and don't
    // want to mess about with it any more
    else if num_names > uniquelist &&
          !self.namelist_differs_nth($namelist, $uniquelist, $ul, $labelyear) {
      // If there are more names than uniquelist, reduce it by one unless
      // there is another list which differs at uniquelist and is at least as long
      // so we get:
      //
      // AAA and BBB and CCC
      // AAA and BBB and CCC et al
      //
      // instead of
      //
      // AAA and BBB and CCC
      // AAA and BBB and CCC and DDD et al
      //
      // BUT, we also want
      //
      // AAA and BBB and CCC
      // AAA and BBB and CCC and DDD et al
      // AAA and BBB and CCC and EEE et al

      $uniquelist--;
    }

    $self->{state}{namelistdata}{$nlid}{ul} = $uniquelist;
    return;
  }

  /// Gets citation name list visibility
  fn get_visible_cite(&self, nlid: Id) {
    return $self->{state}{namelistdata}{$nlid}{viscite};
  }

  /// Gets citation name list visibility
  fn set_visible_cite(&mut self, nlid: Id, $s) {
    $self->{state}{namelistdata}{$nlid}{viscite} = $s;
    return;
  }

  /// Gets bib name list visibility
  fn get_visible_bib(&self, nlid: Id) {
    return $self->{state}{namelistdata}{$nlid}{visbib};
  }

  /// Gets bib name list visibility
  fn set_visible_bib(&mut self, nlid: Id, $s) {
    $self->{state}{namelistdata}{$nlid}{visbib} = $s;
    return;
  }

  /// Gets sort name list visibility
  fn get_visible_sort(&self, nlid: Id) {
    return $self->{state}{namelistdata}{$nlid}{vissort};
  }

  /// Gets sort name list visibility
  fn set_visible_sort(&mut self, nlid: Id, $s) {
    $self->{state}{namelistdata}{$nlid}{vissort} = $s;
    return;
  }

  /// Gets alpha name list visibility
  fn get_visible_alpha(&self, nlid: Id) {
    return $self->{state}{namelistdata}{$nlid}{visalpha};
  }

  /// Gets alpha name list visibility
  fn set_visible_alpha(&mut self, nlid: Id, $s) {
    $self->{state}{namelistdata}{$nlid}{visalpha} = $s;
    return;
  }

  /// Get the number of uniquenames entries for a visible name
  fn get_numofuniquenames(&self, name: &str, namecontext: &str) -> usize {
    return scalar keys $self->{state}{uniquenamecount}{$name}{$namecontext}->%*;
  }

  /// Get the number of uniquenames entries for a name
  fn get_numofuniquenames_all(&self, name: &str, namecontext: &str) -> usize {
    return scalar keys $self->{state}{uniquenamecount_all}{$name}{$namecontext}->%*;
  }

  /// Return a boolean saying whether uniquenename+uniquelist processing is finished
  fn get_unul_done(&self) -> bool {
    self.unulchanged
  }

  /// Set a boolean saying whether uniquename+uniquelist has changed
  fn set_unul_changed(&mut self, val: bool) {
    self.unulchanged = val;
  }

  /// Reset the counters for extra*
  fn reset_seen_extra(&mut, self) {
    $self->{state}{seen_extradate} = {};
    $self->{state}{seen_extraname} = {};
    $self->{state}{seen_extratitle} = {};
    $self->{state}{seen_extratitleyear} = {};
    $self->{state}{seen_extraalpha} = {};
    $self->{state}{seen_namedateparts} = {};
    $self->{state}{seen_labelname} = {};
    $self->{state}{seen_nametitle} = {};
    $self->{state}{seen_titleyear} = {};
    return;
  }

  /// Increment and return the counter for extradate
  fn incr_seen_extradate(self, $ey) {
    return ++$self->{state}{seen_extradate}{$ey};
  }

  /// Increment and return the counter for extraname
  fn incr_seen_extraname(self, $en) {
    return ++$self->{state}{seen_extraname}{$en};
  }

  /// Increment and return a counter used to track extraname
  fn incr_seen_labelname(self, $ln) {
    return ++$self->{state}{seen_labelname}{$ln};
  }

  /// Increment and return the counter for extratitle
  fn incr_seen_extratitle(self, $et) {
    return ++$self->{state}{seen_extratitle}{$et};
  }

  /// Increment and return the counter for extratitleyear
  fn incr_seen_extratitleyear(self, $ety) {
    return ++$self->{state}{seen_extratitleyear}{$ety};
  }

  /// Increment and return the counter for extraalpha
  fn incr_seen_extraalpha(self, $ea) {
    return ++$self->{state}{seen_extraalpha}{$ea};
  }

  /// Get the count of an labelname/dateparts combination for tracking
  /// extradate. It uses labelyear plus name as we need to disambiguate
  /// entries with different labelyear (like differentiating 1984--1986 from
  /// just 1984)
  fn get_seen_namedateparts(self, $ny) {
    return $self->{state}{seen_namedateparts}{$ny}.unwrap_or(0);
  }

  /// Increment the count of an labelname/dateparts combination for extradate
  ///
  /// We pass in the name and date strings separately as we have to
  /// be careful and only increment this counter beyond 1 if there is
  /// a name component. Otherwise, extradate gets defined for all
  /// entries with no name but the same year etc.
  fn incr_seen_namedateparts(self, ns: &str, ys: &str) {
    let $tmp = format!("{ns},{ys}");
    // We can always increment this to 1
    if !(exists($self->{state}{seen_namedateparts}{$tmp})) {
      $self->{state}{seen_namedateparts}{$tmp}++;
    }
    // But beyond that only if we have a labelname in the entry since
    // this counter is used to create extradate which doesn't mean anything for
    // entries with no name
    // We allow empty year so that we generate extradate for the same name with no year
    // so we can do things like "n.d.-a", "n.d.-b" etc.
    else {
      if ($ns) {
        $self->{state}{seen_namedateparts}{$tmp}++;
      }
    }
    return;
  }

  /// Get the count of a labelname hash for tracking extraname
  fn get_seen_labelname(&self, ln: &str) -> u32 {
    self.state.seen_labelname.get(ln).unwrap_or(0);
  }

  /// Get the count of an labelname/labeltitle combination for tracking
  /// extratitle.
  fn get_seen_nametitle(self, nt: &str) {
    self.state.seen_nametitle.get(nt).unwrap_or(0);
  }

  /// Increment the count of an labelname/labeltitle combination for extratitle
  ///
  /// We pass in the name and year strings separately as we have to
  /// be careful and only increment this counter beyond 1 if there is
  /// a title component. Otherwise, extratitle gets defined for all
  /// entries with no title.
  fn incr_seen_nametitle(self, ns: &str, ts: &str) {
    let tmp = format!("{ns},{ts}");
    // We can always increment this to 1
    if !($self->{state}{seen_nametitle}{$tmp}) {
      $self->{state}{seen_nametitle}{$tmp}++;
    }
    // But beyond that only if we have a labeltitle in the entry since
    // this counter is used to create extratitle which doesn't mean anything for
    // entries with no title
    else {
      if !ts.is_empty() {
        $self->{state}{seen_nametitle}{$tmp}++;
      }
    }
    return;
  }

  /// Get the count of an labeltitle/labelyear combination for tracking
  /// extratitleyear
  fn get_seen_titleyear(&self, ty: &str) {
    return $self->{state}{seen_titleyear}{$ty}.unwrap_or(0);
  }

  /// Increment the count of an labeltitle/labelyear combination for extratitleyear
  ///
  /// We pass in the title and year strings separately as we have to
  /// be careful and only increment this counter beyond 1 if there is
  /// a title component. Otherwise, extratitleyear gets defined for all
  /// entries with no title.
  fn incr_seen_titleyear(self, ts: &str, ys: &str) {
    let $tmp = format!("{ts},{ys}");
    // We can always increment this to 1
    if !($self->{state}{seen_titleyear}{$tmp}) {
      $self->{state}{seen_titleyear}{$tmp}++;
    }
    // But beyond that only if we have a labeltitle in the entry since
    // this counter is used to create extratitleyear which doesn't mean anything for
    // entries with no title
    else {
      if ($ts) {
        $self->{state}{seen_titleyear}{$tmp}++;
      }
    }
    return;
  }

  /// Reset various work uniqueness counters
  fn reset_workuniqueness(&mut self) {
    self.state.seenname.reset();
    self.state.seentitle.reset();
    self.state.seenbaretitle.reset();
    self.state.seenwork.reset();
  }

  /// Get the count of occurrences of a labelname or labeltitle
  fn get_seenname(&self, identifier: &str) -> u32 {
    return $self->{state}{seenname}{$identifier};
  }

  /// Increment the count of occurrences of a labelname or labeltitle
  fn incr_seenname(&mut self, identifier: &str) {
    $self->{state}{seenname}{$identifier}++;
    return;
  }

  /// Get the count of occurrences of a labeltitle
  fn get_seentitle(self, $identifier) {
    return $self->{state}{seentitle}{$identifier};
  }

  /// Increment the count of occurrences of a labeltitle
  fn incr_seentitle(self, $identifier) {
    $self->{state}{seentitle}{$identifier}++;
    return;
  }

  /// Get the count of occurrences of a labeltitle when there is
  /// no labelname
  fn get_seenbaretitle(self, $identifier) {
    return $self->{state}{seenbaretitle}{$identifier};
  }

  /// Increment the count of occurrences of a labeltitle
  /// when there is no labelname
  fn incr_seenbaretitle(self, $identifier) {
    $self->{state}{seenbaretitle}{$identifier}++;
    return;
  }

  /// Get the count of occurrences of a labelname and labeltitle
  fn get_seenwork(self, $identifier) {
    return $self->{state}{seenwork}{$identifier};
  }

  /// Increment the count of occurrences of a labelname and labeltitle
  fn incr_seenwork(self, $identifier) {
    $self->{state}{seenwork}{$identifier}++;
    return;
  }

  /// Increment a counter to say we have seen this labelalpha
  fn incr_la_disambiguation(self, $la) {
    $self->{state}{ladisambiguation}{$la}++;
    return;
  }

  /// Get the disambiguation counter for this labelalpha.
  /// Return a 0 for undefs to avoid spurious errors.
  fn get_la_disambiguation(self, $la) {
    return $self->{state}{ladisambiguation}{$la}.unwrap_or(0);
  }

  /// Sets the sortingtemplate name of a data list
  fn set_sortingtemplatename(self, stn) {
    $self->{sortingtemplatename} = lc($stn);
    return;
  }

  /// Gets the attributes of a data list
  fn get_attrs(self) {
    return join('/', ($self->{sortingtemplatename},
                      $self->{sortingnamekeytemplatename},
                      $self->{labelprefix},
                      $self->{uniquenametemplatename},
                      $self->{labelalphanametemplatename}));
  }

  /// Gets the sortingtemplatename of a data list
  fn get_sortingtemplatename(&self) {
    return $self->{sortingtemplatename};
  }

  /// Sets the sortingnamekeytemplate name of a data list
  fn set_sortingnamekeytemplatename(self, snksn) {
    $self->{sortingnamekeytemplatename} = lc($snksn);
    return;
  }

  /// Gets the sortingnamekeytemplatename of a data list
  fn get_sortingnamekeytemplatename(&self) -> &Option<String> {
    &self.sortingnamekeytemplatename()
  }

  /// Sets the uniquenametemplate name of a data list
  fn set_uniquenametemplatename(&mut self, untn: &str) {
    self.uniquenametemplatename = Some(untn.to_lowercase());
  }

  /// Gets the uniquenametemplate name of a data list
  fn get_uniquenametemplatename(&self) -> &Option<String> {
    &self.uniquenametemplatename;
  }

  /// Sets the labelalphanametemplate name of a data list
  fn set_labelalphanametemplatename(self, latn) {
    $self->{labelalphanametemplatename} = lc($latn);
    return;
  }

  /// Gets the labelalphanametemplate name of a data list
  fn get_labelalphanametemplatename(self) {
    return $self->{labelalphanametemplatename};
  }

  /// Sets the sortinit collator for this list
  fn set_sortinit_collator(&mut self, collator) {
    $self->{sortinitcollator} = collator;
    return;
  }

  /// Gets the sortinit collator for this list
  fn get_sortinit_collator(self) {
    return $self->{sortinitcollator};
  }

  /// Gets the labelprefix setting of a data list
  fn get_labelprefix(&self) -> String {
    &self.labelprefix
  }

  /// Sets the labelprefix setting of a data list
  fn set_labelprefix(&mut self, pn: &str) {
    self.labelprefix = pn.into();
  }

  /// Sets the name of a data list
  fn set_name(&mut self, name: &str) {
    self.name = name.into();
  }

  /// Gets the name of a data list
  fn get_name(&self) -> &String {
    &self.name
  }

  /// Sets the type of a data list
  fn set_type(&mut self, typ: &str) {
    self.typ = typ.to_lowercase();
    return;
  }

  /// Gets the type of a section list
  fn get_type(&self) -> &String {
    self.typ
  }

  /// Sets the keys for the list
  fn set_keys(self, $keys) {
    $self->{keys} = $keys;
    return;
  }

  /// Gets the keys for the list
  fn get_keys(&self) -> &Vec<String> {
    &self.keys
  }

  /// Count the keys for the list
  fn count_keys(&self) {
    self.keys.len()
  }

  /// Gets  name list data
  fn get_namelistdata(self) {
    return $self->{state}{namelistdata};
  }

  /// Saves name list data
  fn set_namelistdata(self, $nld) {
    $self->{state}{namelistdata} = $nld;
    return;
  }

  /// Gets labelalpha field data
  fn get_labelalphadata(self) {
    return $self->{state}{labelalphadata};
  }

  /// Saves labelalpha data
  fn set_labelalphadata(self, $lad) {
    $self->{state}{labelalphadata} = $lad;
    return;
  }

  /// Gets labelalpha field data for a key
  fn get_labelalphadata_for_key(self, $key) {
    return $self->{state}{labelalphadata}{$key};
  }

  /// Saves labelalpha field data for a key
  fn set_labelalphadata_for_key(self, $key, $la) {
    if !defined($key) {
      return;
    }
    $self->{state}{labelalphadata}{$key} = $la;
    return;
  }

  /// Saves extradate field data for a key
  fn set_extradatedata_for_key(self, $key, $ed) {
    if !defined($key) {
      return;
    }
    $self->{state}{extradatedata}{$key} = $ed;
    return;
  }

  /// Saves extraname field data for a key
  fn set_extranamedata_for_key(self, $key, $en) {
    if !defined($key) {
      return;
    }
    $self->{state}{extranamedata}{$key} = $en;
    return;
  }

  /// Gets the extraname field data for a key
  fn get_extranamedata_for_key(self, $key) {
    if !defined($key) {
      return;
    }
    return $self->{state}{extranamedata}{$key};
  }

  /// Saves extradate field data for all keys
  fn set_extradatedata(self, $ed) {
    $self->{state}{extradatedata} = $ed;
    return;
  }

  /// Gets the extradate field data for a key
  fn get_extradatedata_for_key(self, $key) {
    if !defined($key) {
      return;
    }
    return $self->{state}{extradatedata}{$key};
  }

  /// Saves extratitle field data for a key
  fn set_extratitledata_for_key(self, $key, $ed) {
    if !defined($key) {
      return;
    }
    $self->{state}{extratitledata}{$key} = $ed;
    return;
  }

  /// Saves extratitle field data for all keys
  fn set_extratitledata(self, $ed) {
    $self->{state}{extratitledata} = $ed;
    return;
  }

  /// Gets the extratitle field data for a key
  fn get_extratitledata_for_key(self, $key) {
    if !defined($key) {
      return;
    }
    return $self->{state}{extratitledata}{$key};
  }

  /// Saves extratitleyear field data for a key
  fn set_extratitleyeardata_for_key(self, $key, $ed) {
    if !defined($key) {
      return;
    }
    $self->{state}{extratitleyeardata}{$key} = $ed;
    return;
  }

  /// Saves extratitleyear field data for all keys
  fn set_extratitleyeardata(self, $ed) {
    $self->{state}{extratitleyeardata} = $ed;
    return;
  }

  /// Gets the extratitleyear field data for a key
  fn get_extratitleyeardata_for_key(self, $key) {
    if !defined($key) {
      return;
    }
    return $self->{state}{extratitleyeardata}{$key};
  }

  /// Saves extraalpha field data for a key
  fn set_extraalphadata_for_key(self, $key, $ed) {
    if !defined($key) {
      return;
    }
    $self->{state}{extraalphadata}{$key} = $ed;
    return;
  }

  /// Saves extraalpha field data for all keys
  fn set_extraalphadata(self, $ed) {
    $self->{state}{extraalphadata} = $ed;
    return;
  }

  /// Gets the extraalpha field data for a key
  fn get_extraalphadata_for_key(self, $key) {
    if !defined($key) {
      return;
    }
    return $self->{state}{extraalphadata}{$key};
  }

  /// Gets the sortdata schema for a sortlist
  fn get_sortdataschema(self) {
    return $self->{sortdataschema};
  }

  /// Saves the sortdata schema for a sortlist
  fn set_sortdataschema(self, $ss) {
    $self->{sortdataschema} = $ss;
    return;
  }

  /// Saves sorting data in a list for a key
  fn set_sortdata(self, $key, $sd) {
    if !defined($key) {
      return;
    }
    $self->{sortdata}{$key} = $sd;
    return;
  }

  /// Gets the sorting data in a list for a key
  fn get_sortdata_for_key(self, $key) {
    if !defined($key) {
      return;
    }
    return $self->{sortdata}{$key};
  }

  /// Saves sortinit data for a specific key
  fn set_sortinitdata_for_key(self, $key, $init) {
    if !defined($key) {
      return;
    }
    $self->{sortinitdata}{$key} = {init => $init};
    return;
  }

  /// Saves sortinit data for all keys
  fn set_sortinitdata(self, $sid) {
    $self->{sortinitdata} = $sid;
    return;
  }

  /// Gets the sortinit in a list for a key
  fn get_sortinit_for_key(self, $key) {
    if !defined($key) {
      return;
    }
    return $self->{sortinitdata}{$key}{init};
  }

  /// Sets the sortingtemplate of a list
  fn set_sortingtemplate(self, sortingtemplate) {
    $self->{sortingtemplate} = $sortingtemplate;
    return;
  }

  /// Gets the sortingtemplate of a list
  fn get_sortingtemplate(self) {
    return $self->{sortingtemplate};
  }

  /// Adds a filter to a list object
  fn add_filter(self, filter) {
    push $self->{filters}->@*, $filter;
    return;
  }

  /// Gets all filters for a list object
  fn get_filters(self) {
    return $self->{filters};
  }

  /// Do any dynamic information replacement for information
  /// which varies in an entry between lists. This is information which
  /// needs to be output to the .bbl for an entry but which is a property
  /// of the reference context and not the entry per se so it cannot be stored
  /// statically in the entry and must be retrieved from the specific datalist
  /// when outputting the entry.
  fn instantiate_entry(self, section: Section, $entry, key: &str, fmt: Option<OutputFormat>) {
    let be = section.bibentry(key);
    let bee = be.get_field("entrytype");

    if !(entry && be) {
      return "";
    }

    let $dmh = crate::config::get_dm_helpers();

    let fmt = fmt.unwrap_or(OutputFormat::Bbl); // default

    let $entry_string = $$entry;

    // .bbl output
    if fmt == OutputFormat::Bbl {
      // entryset
      if let Some(es) = self.get_entryfield(key, "entryset").filter(|es| !es.is_empty()) {
        let s = format!("\\set{{{}}}", es.join(","));
        entry_string = entry_string.replace("<BDS>ENTRYSET</BDS>", &s);
      }

      // uniqueprimaryauthor
      if self.get_entryfield(key, "uniqueprimaryauthor") {
        let s = "\\true{uniqueprimaryauthor}";
        entry_string = entry_string.replace("<BDS>UNIQUEPRIMARYAUTHOR</BDS>", s);
      }

      // uniquework
      if self.get_entryfield(key, "uniquework") {
        let s = "\\true{uniquework}";
        entry_string = entry_string.replace("<BDS>UNIQUEWORK</BDS>", s);
      }

      // uniquebaretitle
      if self.get_entryfield(key, "uniquebaretitle") {
        let s = "\\true{uniquebaretitle}";
        entry_string = entry_string.replace("<BDS>UNIQUEBARETITLE</BDS>", s);
      }

      // uniquetitle
      if self.get_entryfield(key, "uniquetitle") {
        let s = "\\true{uniquetitle}";
        entry_string = entry_string.replace("<BDS>UNIQUETITLE</BDS>", s);
      }

      // extraalpha
      if let Some(e) = self.get_extraalphadata_for_key(key).filter(|es| !es.is_empty()) {
        let s = format!("\\field{{extraalpha}}{{{e}}}");
        entry_string = entry_string.replace("<BDS>EXTRAALPHA</BDS>", &s);
      }

      // labelalpha
      if let Some(e) = self.get_labelalphadata_for_key(key).filter(|es| !es.is_empty()) {
        let s = format!("\\field{{labelalpha}}{{{e}}}");
        entry_string = entry_string.replace("<BDS>LABELALPHA</BDS>", &s);
      }

      // uniquelist
      for namefield in &dmh.namelists {
        let nl = be.get_field(namefield);
        if !nl {
          continue;
        }
        let nlid = nl.get_id();
        if let Some(ul) = self.get_uniquelist(nlid) {
          let s = format!("ul={ul}");
          entry_string = entry_string.replace(&format!("<BDS>UL-{nlid}</BDS>"), &s);
        } else {
          let r = Regex::new(&format!(r"(?xms)<BDS>UL-{nlid}</BDS>,?")).unwrap();
          entry_string = r.replace_all(entry_string, "");
        }
      }

      // uniquename
      for namefield in ($dmh->{namelists}->@*) {
        let nl = be.get_field(namefield);
        if !nl {
          continue;
        }
        let nlid = nl.get_id();
        for n in nl.names() {
          let nid = n.get_id();
          if let Some(unsummary) = self.get_unsummary(nlid, nid) {
            let $str = format!("un={unsummary}");
            $entry_string =~ s|<BDS>UNS-$nid</BDS>|$str|gxms;
            $str = "uniquepart=" . $self->get_unpart($nlid, $nid);
            $entry_string =~ s|<BDS>UNP-$nid</BDS>|$str|gxms;
            for np in ($n->get_nameparts) {
              if ($self->is_unbasepart($nlid, $nid, $np)) {
                $entry_string =~ s|\s+<BDS>UNP-$np-$nid</BDS>,?||gxms;
              }
              else {
                $str = "${np}un=" . $self->get_unparts($nlid, $nid, $np);
                $entry_string =~ s|<BDS>UNP-$np-$nid</BDS>|$str|gxms;
              }
            }
          }
          else {
            $entry_string =~ s|<BDS>UN[SP]-$nid</BDS>,?||gxms;
            for np in ($n->get_nameparts) {
              $entry_string =~ s|\s+<BDS>UNP-$np-$nid</BDS>,?||gxms;
            }
          }
        }
      }

      // extratitleyear
      if (let $e = $self->get_extratitleyeardata_for_key($key)) {
        let  $str = "\\field{extratitleyear}{$e}";
        $entry_string =~ s|<BDS>EXTRATITLEYEAR</BDS>|$str|gxms;
      }

      // extratitle
      if (let $e = $self->get_extratitledata_for_key($key)) {
        let $str = "\\field{extratitle}{$e}";
        $entry_string =~ s|<BDS>EXTRATITLE</BDS>|$str|gxms;
      }

      // per-namelist bibnamehash and namehash
      for namefield in ($dmh->{namelists}->@*) {

        // per-namelist bibnamehash
        if (let $e = $self->get_entryfield($key, "${namefield}bibnamehash")) {
          let $str = "\\strng{${namefield}bibnamehash}{$e}";
          $entry_string =~ s|<BDS>${namefield}BIBNAMEHASH</BDS>|$str|gxms;
        }

        // per-namelist namehash
        if (let $e = $self->get_entryfield($key, "${namefield}namehash")) {
          let $str = "\\strng{${namefield}namehash}{$e}";
          $entry_string =~ s|<BDS>${namefield}NAMEHASH</BDS>|$str|gxms;

        }
      }

      // bibnamehash
      if (let $e = $self->get_entryfield($key, "bibnamehash")) {
        let $str = "\\strng{bibnamehash}{$e}";
        $entry_string =~ s|<BDS>BIBNAMEHASH</BDS>|$str|gxms;
      }

      // namehash
      if (let $e = $self->get_entryfield($key, "namehash")) {
        let $str = "\\strng{namehash}{$e}";
        $entry_string =~ s|<BDS>NAMEHASH</BDS>|$str|gxms;
      }

      // per-namehash
      for pn in ($dmh->{namelistsall}->@*) {
        let nl = $be->get_field($pn);
        if !nl {
          continue;
        }
        for n in nl.names() {
          let nid = n.get_id();
          if (let $e = $self->{state}{namelistdata}{nl.get_id()}{nid}{hash}) {
            let $str = "hash=$e";
            $entry_string =~ s|<BDS>$nid-PERNAMEHASH</BDS>|$str|gxms;
          }
          else {
            $entry_string =~ s|<BDS>$nid-PERNAMEHASH</BDS>,?||gxms;
          }
        }
      }

      // extraname
      if (let $e = $self->get_extranamedata_for_key($key)) {
        let $str = "\\field{extraname}{$e}";
        $entry_string =~ s|<BDS>EXTRANAME</BDS>|$str|gxms;
      }

      // extradate
      if (let $e = $self->get_extradatedata_for_key($key)) {
        let $str = "\\field{extradate}{$e}";
        $entry_string =~ s|<BDS>EXTRADATE</BDS>|$str|gxms;
      }

      // sortinit + sortinithash
      let sinit = self.get_sortinit_for_key(key);
      if (defined($sinit)) {
        let $str = "\\field{sortinit}{$sinit}";
        $entry_string =~ s|<BDS>SORTINIT</BDS>|$str|gxms;
        let $sinithash = md5_hex($self->{sortinitcollator}->viewSortKey($sinit));
        $str = "\\field{sortinithash}{$sinithash}";
        $entry_string =~ s|<BDS>SORTINITHASH</BDS>|$str|gxms;
      }

      // labelprefix
      if (let $pn = self.get_labelprefix(key)) {
        let $str = "\\field{labelprefix}{$pn}";
        $entry_string =~ s|<BDS>LABELPREFIX</BDS>|$str|gxms;
      }

      // singletitle
      if self.get_entryfield(key, "singletitle") {
        let $str = "\\true{singletitle}";
        $entry_string =~ s|<BDS>SINGLETITLE</BDS>|$str|gxms;
      }
    }

    // .bblxml output
    if fmt == OutputFormat::BblXML {
      // entryset
      if (let $es = self.get_entryfield(key, "entryset")) {
        let $str = "<bbl:set>\n";
        for m in ($es->@*) {
          $str .= "    <bbl:member>$m</bbl:member>\n";
        }
        $str .= "  </bbl:set>";
        $entry_string =~ s|<BDS>ENTRYSET</BDS>|$str|gxms;
      }

      // uniqueprimaryauthor
      if self.get_entryfield(key, "uniqueprimaryauthor") {
        let $str = "true";
        $entry_string =~ s|\[BDS\]UNIQUEPRIMARYAUTHOR\[/BDS\]|$str|gxms;
      }
      else {
        $entry_string =~ s|\suniqueprimaryauthor="\[BDS\]UNIQUEPRIMARYAUTHOR\[/BDS\]"||gxms;
      }

      // uniquework
      if self.get_entryfield(key, "uniquework") {
        let $str = "true";
        $entry_string =~ s|\[BDS\]UNIQUEWORK\[/BDS\]|$str|gxms;
      }
      else {
        $entry_string =~ s|\suniquework="\[BDS\]UNIQUEWORK\[/BDS\]"||gxms;
      }

      // uniquebaretitle
      if self.get_entryfield(key, "uniquebaretitle") {
        let $str = "true";
        $entry_string =~ s|\[BDS\]UNIQUEBARETITLE\[/BDS\]|$str|gxms;
      }
      else {
        $entry_string =~ s|\suniquebaretitle="\[BDS\]UNIQUEBARETITLE\[/BDS\]"||gxms;
      }

      // uniquetitle
      if self.get_entryfield(key, "uniquetitle") {
        let $str = "true";
        $entry_string =~ s|\[BDS\]UNIQUETITLE\[/BDS\]|$str|gxms;
      }
      else {
        $entry_string =~ s|\suniquetitle="\[BDS\]UNIQUETITLE\[/BDS\]"||gxms;
      }

      // extraalpha
      if (let $e = self.get_extraalphadata_for_key(key)) {
        let $str = "<bbl:field name=\"extraalpha\">$e</bbl:field>";
        $entry_string =~ s|<BDS>EXTRAALPHA</BDS>|$str|gxms;
      }

      // labelalpha
      if (let $e = self.get_labelalphadata_for_key(key)) {
        let $str = "<bbl:field name=\"labelalpha\">$e</bbl:field>";
        $entry_string =~ s|<BDS>LABELALPHA</BDS>|$str|gxms;
      }

      // uniquelist
      for namefield in ($dmh->{namelists}->@*) {
        let nl = $be->get_field($namefield);
        if !nl {
          continue;
        }
        let nlid = nl.get_id();
        if let Some(s) = self.get_uniquelist(nlid) {
          entry_string = entry_string.replace(&format!("\[BDS\]UL-{nlid}\[/BDS\]"), s);
        }
        else {
          entry_string = entry_string.replace(&format!(r#"\sul="\[BDS\]UL-$nlid\[/BDS\]""#), "");
        }
      }

      // uniquename
      for namefield in ($dmh->{namelists}->@*) {
        let nl = be.get_field($namefield);
        if !nl {
          continue;
        }
        let nlid = nl.get_id();
        for n in nl.names() {
          let nid = n.get_id();
          if let Some(unsummary) = self.get_unsummary(nlid, nid) {
            let $str = format!("{unsummary}");
            $entry_string =~ s|\[BDS\]UNS-$nid\[/BDS\]|$str|gxms;
            $str = self.get_unpart(nlid, nid);
            $entry_string =~ s|\[BDS\]UNP-$nid\[/BDS\]|$str|gxms;
            for np in n.get_nameparts() {
              if (self.is_unbasepart(nlid, nid, np)) {
                $entry_string =~ s|\sun="\[BDS\]UNP-$np-$nid\[/BDS\]",?||gxms;
              }
              else {
                $str = self.get_unparts(nlid, nid, np);
                $entry_string =~ s|\[BDS\]UNP-$np-$nid\[/BDS\]|$str|gxms;
              }
            }
          }
          else {
            $entry_string =~ s#\s(?:un|uniquepart)="\[BDS\]UN[SP]-$nid\[/BDS\]",?##gxms;
            for np in n.get_nameparts() {
              $entry_string =~ s|\sun="\[BDS\]UNP-$np-$nid\[/BDS\]",?||gxms;
            }
          }
        }
      }

      // extratitleyear
      if (let $e = self.get_extratitleyeardata_for_key(key)) {
        let $str = "<bbl:field name=\"extratitleyear\">$e</bbl:field>";
        $entry_string =~ s|<BDS>EXTRATITLEYEAR</BDS>|$str|gxms;
      }

      // extratitle
      if (let $e = self.get_extratitledata_for_key(key)) {
        let $str = "<bbl:field name=\"extratitle\">$e</bbl:field>";
        $entry_string =~ s|<BDS>EXTRATITLE</BDS>|$str|gxms;
      }

      // per-namelist bibnamehash and namehash
      for namefield in ($dmh->{namelists}->@*) {

        // per-namelist bibnamehash
        if (let $e = self.get_entryfield(key, "${namefield}bibnamehash")) {
          let $str = "<bbl:field name=\"${namefield}bibnamehash\">$e</bbl:field>";
          $entry_string =~ s|<BDS>${namefield}BIBNAMEHASH</BDS>|$str|gxms;
        }

        // per-namelist namehash
        if (let $e = self.get_entryfield(key, "${namefield}namehash")) {
          let $str = "<bbl:field name=\"${namefield}namehash\">$e</bbl:field>";
          $entry_string =~ s|<BDS>${namefield}NAMEHASH</BDS>|$str|gxms;
        }
      }

      // bibnamehash
      if (let $e = self.get_entryfield(key, "bibnamehash")) {
        let $str = "<bbl:field name=\"bibnamehash\">$e</bbl:field>";
        $entry_string =~ s|<BDS>BIBNAMEHASH</BDS>|$str|gxms;
      }

      // namehash
      if (let $e = self.get_entryfield(key, "namehash")) {
        let $str = "<bbl:field name=\"namehash\">$e</bbl:field>";
        $entry_string =~ s|<BDS>NAMEHASH</BDS>|$str|gxms;
      }

      // per-namehash
      for pn in ($dmh->{namelistsall}->@*) {
        let nl = $be->get_field($pn);
        if !nl {
          continue;
        }
        for n in nl.names() {
          let nid = n.get_id();
          if (let $e = $self->{state}{namelistdata}{nl.get_id()}{nid}{hash}) {
            let $str = $e;
            $entry_string =~ s|\[BDS\]$nid-PERNAMEHASH\[/BDS\]|$str|gxms;
          }
          else {
            $entry_string =~ s|hash="\[BDS\]$nid-PERNAMEHASH\[/BDS\]"?,?||gxms;
          }
        }
      }

      // extraname
      if (let $e = $self->get_extranamedata_for_key($key)) {
        let $str = "<bbl:field name=\"extraname\">$e</bbl:field>";
        $entry_string =~ s|<BDS>EXTRANAME</BDS>|$str|gxms;
      }

      // extradate
      if (let $e = $self->get_extradatedata_for_key($key)) {
        let $str = "<bbl:field name=\"extradate\">$e</bbl:field>";
        $entry_string =~ s|<BDS>EXTRADATE</BDS>|$str|gxms;
      }

      // sortinit + sortinithash
      let sinit = self.get_sortinit_for_key(key);
      if (defined($sinit)) {
        let $str = "<bbl:field name=\"sortinit\">$sinit</bbl:field>";
        $entry_string =~ s|<BDS>SORTINIT</BDS>|$str|gxms;
        let $sinithash = md5_hex($self->{sortinitcollator}->viewSortKey($sinit));
        $str = "<bbl:field name=\"sortinithash\">$sinithash</bbl:field>";
        $entry_string =~ s|<BDS>SORTINITHASH</BDS>|$str|gxms;
      }

      // labelprefix
      if (let $pn = self.get_labelprefix(key)) {
        let $str = "<bbl:field name=\"labelprefix\">$pn</bbl:field>";
        $entry_string =~ s|<BDS>LABELPREFIX</BDS>|$str|gxms;
      }

      // singletitle
      if ($self->get_entryfield($key, "singletitle")) {
        let $str = "true";
        $entry_string =~ s|\[BDS\]SINGLETITLE\[/BDS\]|$str|gxms;
      }
      else {
        $entry_string =~ s|\ssingletitle="\[BDS\]SINGLETITLE\[/BDS\]"||gxms;
      }
    }

    // Clean up dangling commas
    $entry_string =~ s|,(?:\n\s+)?\}\}|}}|gxms;

    // Clean up generic metadata which was not replaced
    $entry_string =~ s|^\s+<BDS>[^<]+</BDS>\n||gxms;

    return $entry_string;
  }

  /// Returns the index where the name list begins to differ from any other list
  ///
  /// Assuming these lists
  ///
  /// ```
  /// [a, b]
  /// [a, b, d, e, f, g, h, i, j]
  /// [a, b, d, e, f]
  /// [a, b, e, z, z, y]
  ///
  /// namelist_differs_index([a, b, c, d, e]) -> 2
  /// namelist_differs_index([a]) -> 1
  /// ```
  fn namelist_differs_index(self, @list) {
    let mut index = None;
    for l_s in (keys $self->{state}{uniquelistcount}{global}{final}->%*) {
      let @l = split("\x{10FFFD}", $l_s);
      if Compare(\@list, \@l) {// Ignore identical lists
        continue;
      }
      for (i, val) in list.iter().enumerate() {
        if (defined(val) && defined(l[i]) && (val == l[i])) {
          if (!defined(index) || i > index) {
            index = Some(i);
          }
        }
        else {
          break;
        }
      }
    }

    if let Some(index) = index { // one or more similar lists
      if index == list.len() - 1 { // There is another list which is a superset, return last index
        return index;
      } else { // Differs with some list, return index of where difference begins
        return index+1;
      }
    }
    else { // no similar lists
      return None;
    }
  }

  /// Returns true if some other name list differs at passed nth place
  /// and is at least as long
  ///
  /// ```
  /// namelist_differs_nth([a, b, c, d, e], 3) = true
  /// ```
  ///
  /// if there is another name list like any of these:
  ///
  /// ```
  /// [a, b, d, e, f]
  /// [a, b, e, z, z, y]
  /// ```
  fn namelist_differs_nth(self, list, n, ul: &str, labelyear) -> bool {
    let @list_one = $list->@*;
    // Loop over all final lists, looking for ones which match:
    // * up to n - 1
    // * differ at $n
    // * are at least as long

    // uniquelist=minyear should only disambiguate from entries with the
    // same labelyear
    let $unames = $self->{state}{uniquelistcount}{global}{final};
    if ul == "minyear" {
      $unames = $self->{state}{uniquelistcount}{global}{final}{$labelyear};
    }

    for l_s in unames.keys() {
      let l: Vec<_> = l_s.split("\x{10FFFD}").collect();
      // If list is shorter than the list we are checking, it's irrelevant
      if l.len() < list.len() {
        continue;
      }
      // If list matches at $n, it's irrelevant
      if list_one[n-1] == l[n-1] {
        continue;
      }
      // If list doesn't match up to $n - 1, it's irrelevant
      if !Compare([@list_one[0 .. $n-2]], [@l[0 .. $n-2]]) {
        continue;
      }
      return true;
    }
    return false;
  }
}
