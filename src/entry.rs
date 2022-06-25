//! `Entry` objects

use crate::Utils;
use crate::Internals;
use crate::Constants;
use Data::Dump qw( pp );
use Digest::MD5 qw( md5_hex );
use Encode;
use Log::Log4perl qw( :no_extra_logdie_message );
use List::Util qw( first );

pub struct XData;

pub struct Entry {
  xdatarefs: Vec<XData>,
  labelnameinfo: Unknown,
  labelnamefhinfo: Unknown,
  labeltitleinfo: Unknown,
  labeldateinfo: Unknown,
  derivedfields: Hash<String, Unknown>,
  datafields: Hash<String, Unknown>,
}

impl Entry {
  /// Initialize a crate::Entry object
  ///
  /// There are three types of field possible in an entry:
  ///
  /// * data    - These are fields which derive directly from or are themselves fields in the
  ///             data source. Things like YEAR, MONTH, DAY etc. are such fields which are
  ///             derived from, for example, the DATE field. They are part of the original
  ///             data implicitly, derived from a field.
  /// * derived - These are fields, often meta-information like labelname, labelalpha etc.
  ///             which are more removed from the data fields.
  ///
  /// The reason for this division is largely the entry cloning required for the related entry and
  /// inheritance features. When we clone an entry or copy some fields from one entry to another
  /// we generally don't want the "derived" category as such derived meta-fields will often need
  /// to be re-created or ignored so we need to know which are the actual "data" fields to
  /// copy/clone.
  fn new(obj) -> Self {
    let $self;
    if (defined($obj) && ref($obj) == "HASH") {
      $self = bless $obj, $class;
    }
    else {
      $self = bless {}, $class;
    }
    return $self;
  }

  /// Recursively create related entry clones starting with an entry
  fn relclone(&mut self) {
    let $citekey = $self->get_field("citekey");
    let secnum = crate::MASTER.get_current_section();
    let section = crate::MASTER.sections().get_section(secnum);
    let $dmh = crate::config::get_dm_helpers();
    if (let $relkeys = $self->get_field("related")) {
        debug!("Found RELATED field in '{}' with contents {}", citekey, join(',', @$relkeys));
      let @clonekeys;
      foreach let $relkey (@$relkeys) {
        // Resolve any alias
        let $nrelkey = $section->get_citekey_alias($relkey).unwrap_or($relkey);
          if $relkey != $nrelkey {
            debug!("Resolved RELATED key alias '{}' to '{}'", relkey, nrelkey);
          }
          debug!("Looking at RELATED key '{}'", relkey);
        $relkey = $nrelkey;

        // Loop avoidance, in case we are back in an entry again in the guise of a clone
        // We can record the related clone but don't create it again
        if (let $ck = $section->get_keytorelclone($relkey)) {
            debug!("Found RELATED key '{}' already has clone '{}'", relkey, ck);
          clonekeys.push(ck);

          // Save graph information if requested
          if (crate::Config->getoption("output_format") == "dot") {
            crate::Config->set_graph("related", $ck, $relkey, $citekey);
          }
        }
        else {
          let relentry = section.bibentry(relkey);
          // Digest::MD5 can't deal with straight UTF8 so encode it first (via NFC as this is "output")
          let clonekey = md5_hex(encode_utf8(relkey));
          clonekeys.push(clonekey);
          let $relclone = $relentry->clone(clonekey);
            debug!("Created new related clone for '{}' with clone key '{}'", relkey, clonekey);

          // Set related clone options
          if (let $relopts = $self->get_field("relatedoptions")) {
            // Check if this clone was also directly cited. If so, set skipbib/skipbiblist
            // if they are unset as otherwise this entry would appear twice in bibliographies
            // but with different keys.
            if section.has_citekey(relkey) {
              $relopts = merge_entry_options($relopts, ["skipbib", "skipbiblist"]);
            }

            process_entry_options($clonekey, $relopts, $secnum);
            $relclone->set_datafield("options", $relopts);
          }
          else {
            // related clone needs its own options plus all the dataonly opts, any conflicts and
            // explicit options win

            let $relopts = merge_entry_options(["skipbib", "skiplab","skipbiblist","uniquename=false","uniquelist=false"], $relentry->get_field("options"));

            // Preserve options already in the clone but add "dataonly" options
            process_entry_options($clonekey, $relopts, $secnum);
            $relclone->set_datafield("options", $relopts);
          }

          section.bibentries().add_entry(clonekey, relclone);
          section.keytorelclone(relkey, clonekey);

          // Save graph information if requested
          if (crate::Config->getoption("output_format") == "dot") {
            crate::Config->set_graph("related", clonekey, relkey, citekey);
          }

          // recurse so we can do cascading related entries
            debug!("Recursing into RELATED entry '{}'", clonekey);
          relclone.relclone();
        }
      }
      // point to clone keys and add to citekeys
      // We have to add the citekeys as we need these clones in the .bbl
      // but the dataonly will cause biblatex not to print them in the bib
      $section->add_citekeys(@clonekeys);
      $self->set_datafield("related", [ @clonekeys ]);
    }
  }

  /// Clone a crate::Entry object and return a copy
  /// Accepts optionally a key for the copy
  fn clone(self, $newkey) {
    let $new = new crate::Entry;
    let dmh = crate::config::get_dm_helpers();

    while (let ($k, $v) = each(%{$self->{datafields}})) {
      $new->{datafields}{$k} = $v;
    }
    while (let ($k, $v) = each(%{$self->{origfields}})) {
      $new->{origfields}{$k} = $v;
    }

    // Clone xdata information
    new.xdatarefs = self.xdatarefs.clone();

    // clone derived date fields
    for df in dmh.datefields.iter() {
      $df =~ s/date$//;
      for dsf in &["dateunspecified", "datesplit", "datejulian",
                      "enddatejulian", "dateapproximate", "enddateapproximate",
                      "dateuncertain", "enddateuncertain", "yeardivision", "yeardivision",
                      "era", "endera"] {
        let key = format!("{df}{dsf}");
        if let Some(ds) = self.derivedfields.get(&key) {
          new.derivedfields.insert(&key, ds.clone());
        }
      }
    }

    // Clone annotations
    crate::Annotation->copy_annotations($self->get_field("citekey"), $newkey);

    // Need to add entrytype and datatype
    $new->{derivedfields}{entrytype} = $self->{derivedfields}{entrytype};
    $new->{derivedfields}{datatype} = $self->{derivedfields}{datatype};

    // put in key if specified
    if ($newkey) {
      $new->{derivedfields}{citekey} = $newkey;
    }
    // Record the key of the source of the clone in the clone. Useful for loop detection etc.
    // in biblatex
    $new->{derivedfields}{clonesourcekey} = $self->get_field("citekey");
    return $new;
  }

  /// Test for an empty object
  fn notnull(self) {
    let @arr = keys %$self;
    return $#arr > -1 ? 1 : 0;
  }

  /// Add an XDATA reference to the entry
  /// Reference can be simply to an entire XDATA entry or a particular field+position in field
  /// Record reference and target positions so that the XDATA marker can be removed as otherwise
  /// it would break further parsing
  fn add_xdata_ref(self, $reffield, $value, $reffieldposition) {
    if ($reffield == "xdata") { // whole XDATA fields are a simple case
      push $self->{xdatarefs}->@*, {// field pointing to XDATA
                                    reffield => "xdata",
                                    refposition => 0,
                                    xdataentries => $value,
                                    xdatafield => undef,
                                    xdataposition => 0};
      return 1;
    }
    else { // Granular XDATA reference
      let $xnamesep = crate::Config->getoption("xnamesep");
      let $xdatamarker = crate::Config->getoption("xdatamarker");
      if (let ($xdataref) = $value =~ m/^$xdatamarker$xnamesep(\S+)$/xi) {
        let $xdatasep = crate::Config->getoption("xdatasep");
        let ($xe, $xf, $xfp) = $xdataref =~ m/^([^$xdatasep]+)$xdatasep([^$xdatasep]+)(?:$xdatasep(\d+))?$/x;
        if !($xf) { // There must be a field in a granular XDATA ref
          let $entry_key = $self->get_field("citekey");
          let secnum = crate::MASTER.get_current_section();
          biber_warn("Entry '$entry_key' has XDATA reference from field '$reffield' that contains no source field (section $secnum)", $self);
          return 0;
        }
        push $self->{xdatarefs}->@*, {// field pointing to XDATA
                                      reffield => $reffield,
                                      // field position pointing to XDATA, 1-based
                                      refposition => defined($reffieldposition) ? $reffieldposition+1 : 1,
                                      // XDATA entry
                                      xdataentries => [$xe],
                                      // XDATA field
                                      xdatafield => $xf,
                                      // XDATA field position, 1-based
                                      xdataposition => $xf.unwrap_or("*")};
        return 1;
      }
      else {
        return 0;
      }
    }
  }

  /// Get the XDATA references
  fn get_xdata_refs(self) {
    return $self->{xdatarefs};
  }

  /// Get a specific XDATA reference
  fn get_xdata_ref(self, $field, $pos) {
    foreach let $xdatum ($self->{xdatarefs}->@*) {
      if ($xdatum->{reffield} == $field) {
        if ($pos) {
          if ($xdatum->{refposition} == $pos) {
            return $xdatum;
          }
        }
        else {
          return $xdatum;
        }
      }
    }
    return undef;
  }

  /// Checks if an XDATA reference was resolved. Returns false also for
  /// "no such reference".
  fn is_xdata_resolved(self, $field, $pos) {
    foreach let $xdatum ($self->{xdatarefs}->@*) {
      if ($xdatum->{reffield} == $field) {
        if ($pos) {
          if ($xdatum->{refposition} == $pos) {
            return $xdatum->{resolved};
          }
        }
        else {
          return $xdatum->{resolved};
        }
      }
    }
    return 0;
  }

  /// Record the labelname information. This is special
  /// meta-information so we have a separate method for this
  /// Takes a hash ref with the information.
  fn set_labelname_info(&mut self, data: Unknown) {
    self.labelnameinfo = data;
  }

  /// Retrieve the labelname information. This is special
  /// meta-information so we have a separate method for this
  /// Returns a hash ref with the information.
  fn get_labelname_info(&self) -> &Option<String> {
    &self.labelnameinfo
  }

  /// Record the fullhash labelname information. This is special
  /// meta-information so we have a separate method for this
  /// Takes a hash ref with the information.
  fn set_labelnamefh_info(&mut self, data: Unknown) {
    self.labelnamefhinfo = data;
  }

  /// Retrieve the fullhash labelname information. This is special
  /// meta-information so we have a separate method for this
  /// Returns a hash ref with the information.
  fn get_labelnamefh_info(&self) -> Unknown {
    self.labelnamefhinfo
  }

  /// Record the labeltitle information. This is special
  /// meta-information so we have a separate method for this
  /// Takes a hash ref with the information.
  fn set_labeltitle_info(&mut self, data: Unknown) {
    self.labeltitleinfo = data;
  }

  /// Retrieve the labeltitle information. This is special
  /// meta-information so we have a separate method for this
  /// Returns a hash ref with the information.
  fn get_labeltitle_info(&self) -> Unknown {
    self.labeltitleinfo
  }

  /// Record the labeldate information. This is special
  /// meta-information so we have a separate method for this
  /// Takes a hash ref with the information.
  fn set_labeldate_info(&mut self, data: Unknown) {
    self.labeldateinfo = data;
  }

  /// Retrieve the labeldate information. This is special
  /// meta-information so we have a separate method for this
  /// Returns a hash ref with the information.
  fn get_labeldate_info(&self) -> Unknown {
    self.labeldateinfo
  }

  /// Set a derived field for a crate::Entry object, that is, a field
  /// which was not an actual bibliography field
  fn set_field(&mut self, key: &str, val: Unknown) {
    // All derived fields can be null
    self.derivedfields.insert(key, val);
  }

  /// Get a field for a crate::Entry object
  /// Uses // as fields can be null (end dates etc).
  fn get_field(&self, key: &str) -> Option<Unknown> {
    if key.is_empty() {
      return None;
    }
    self.datafields.get(key).or_else(|| self.derivedfields.get(key))
  }

  /// Set a field which is in the .bib data file
  fn set_datafield(self, $key, $val) {
    $self->{datafields}{$key} = $val;
    return;
  }

  /// Get a field that was in the original data file
  fn get_datafield(self, $key) {
    return $self->{datafields}{$key};
  }

  /// Delete a field in a crate::Entry object
  fn del_field(&mut self, key: &str) {
    self.datafields.remove(key);
    self.derivedfields.remove(key);
  }

  /// Delete an original data source data field in a crate::Entry object
  fn del_datafield(self, $key) {
    delete $self->{datafields}{$key};
    return;
  }

  /// Check whether a field exists (even if null)
  fn field_exists(self, $key) -> bool {
    return (exists($self->{datafields}{$key}) || exists($self->{derivedfields}{$key}));
  }

  /// Check whether any parts of a date field exist when passed a datepart field name
  fn date_fields_exist(&self, field: &str) -> bool {
    let r = Regex::new("(?:end)?(?:year|month|day|hour|minute|second|yeardivision|timezone)$").unwrap();
    let t = field.replace(field, "");
    for dp in ["year", "month", "day", "hour", "minute", "second", "yeardivision", "timezone"] {
      if self.datafields.get(&format!("{t}{dp}")).is_some() || self.datafields.get(&format!("{t}end{dp}")).is_some() {
        return true;
      }
    }
    return false;
  }

  /// Delete all parts of a date field when passed any datepart field name
  fn delete_date_fields(&mut self, field: &str)
    let r = Regex::new("(?:end)?(?:year|month|day|hour|minute|second|yeardivision|timezone)$").unwrap();
    let t = field.replace(field, "");
    for dp in ["year", "month", "day", "hour", "minute", "second", "yeardivision", "timezone"] {
      self.datafields.remove(&format!("{t}{dp}"));
      self.datafields.remove(&format!("{t}end{dp}"));
    }
  }

  /// Returns a sorted array of the fields which came from the data source
  fn datafields(&self) -> Vec<&String> {
    let mut fields = self.datafields.keys().collect();
    fields.sort();
    fields
  }

  /// Returns the number of datafields
  fn count_datafields(self) -> usize {
    self.datafields.len()
  }

  /// Returns a sorted array of the fields which were added during processing
  fn derivedfields(self) {
    use locale;
    return sort keys %{$self->{derivedfields}};
  }

  /// Returns a sorted array of all field names, including ones
  /// added during processing which are not necessarily fields
  /// which came from the data file
  fn fields(self) {
    use locale;
    let %keys = (%{$self->{derivedfields}}, %{$self->{datafields}});
    return sort keys %keys;
  }

  /// Returns the number of fields
  fn count_fields(self) {
    let %keys = (%{$self->{derivedfields}}, %{$self->{datafields}});
    return keys %keys;
  }

  /// Check if a crate::Entry object has a particular keyword in
  /// in the KEYWORDS field.
  fn has_keyword(self, keyword) {
    no autovivification;
    if (let $keywords = $self->{datafields}{keywords}) {
      return (first {$_ == $keyword} @$keywords) ? 1 : 0;
    }
    else {
      return 0;
    }
    return undef; // shouldn't get here
  }

  /// Append a warning to a crate::Entry object
  fn add_warning(self, $warning) {
    push $self->{derivedfields}{warnings}->@*, $warning;
    return;
  }

  /// Inherit fields from first child entry
  ///
  /// ```
  /// $entry->set_inherit_from($firstchild);
  /// ```
  ///
  /// Takes a second crate::Entry object as argument
  ///
  /// The purpose here is to inherit fields so that sorting/labelling defaults
  /// can be generated for set parents from the first child set member data, unless
  /// the set parent itself already has some fields set that will do this. Set
  /// parents only have certain fields output in the .bbl and those that output but
  /// are not used in sorting/labelling data generation should not be inherited.
  fn set_inherit_from(self, $parent) {
    let $dmh = crate::config::get_dm_helpers();

    // Data source fields
    foreach let $field ($parent->datafields) {
      if $self->field_exists($field) { // Don't overwrite existing fields
        continue;
      }

      // Annotations are allowed for set parents themselves so never inherit these.
      // This can't be suppressed at .bbl writing as it is impossible to know there
      // whether the field came from the parent or first child because inheritance
      // is a low-level operation on datafields
      if unicase::eq(field, "annotation") {
        continue;
      }

      $self->set_datafield($field, $parent->get_field($field));
    }

    // Datesplit is a special non datafield and needs to be inherited for any
    // validation checks which may occur later
    foreach let $df ($dmh->{datefields}->@*) {
      $df =~ s/date$//;
      if (let $ds = $parent->get_field("${df}datesplit")) {
        $self->set_field("${df}datesplit", $ds);
      }
    }
    return;
  }

  /// Recursively resolve XDATA in an entry. Sets a flag in the XDATA metadata to
  /// say if the reference was successfully resolved.
  ///
  /// ```
  /// $entry->resolve_xdata($xdata);
  /// ```
  fn resolve_xdata(self, $xdata) {
    let secnum = crate::MASTER.get_current_section();
    let section = crate::MASTER.sections().get_section(secnum);
    let $entry_key = $self->get_field("citekey");
    let $dm = crate::config::get_dm();

    // $xdata =
    // [
    //  { // xdata info for an actual XDATA field (XDATA = {key, key})
    //    reffield      => "xdata",
    //    refposition   => 0,
    //    xdataentries  => // array ref of XDATA entry keys
    //    xdatafield    => undef,
    //    xdataposition => 0,
    //    resolved      => 1 or 0
    //  },
    //  { // xdata info for an granular XDATA ref in another field
    //    reffield      => // field pointing to XDATA
    //    refposition   => // field position pointing to XDATA (or 1), 1-based
    //    xdataentries  => // array ref containing single XDATA entry key
    //    xdatafield    => // field within XDATA entry
    //    xdataposition => // position in list field within XDATA entry (or 1), 1-based
    //    resolved      => 1 or 0
    //  }
    //  {
    //    .
    //    .
    //    .
    //  }
    // ]

    foreach let $xdatum ($xdata->@*) {
      foreach let $xdref ($xdatum->{xdataentries}->@*) {
        let xdataentry = section.bibentry(xdref);
        if !xdataentry {
          biber_warn("Entry '$entry_key' references XDATA entry '$xdref' which does not exist, not resolving (section $secnum)", $self);
          $xdatum->{resolved} = 0;
          continue;
        }
        else {
          if !($xdataentry->get_field("entrytype") == "xdata") {
            biber_warn("Entry '$entry_key' references XDATA entry '$xdref' which is not an XDATA entry, not resolving (section $secnum)", $self);
            $xdatum->{resolved} = 0;
            continue;
          }

          // record the XDATA resolve between these entries to prevent loops
          crate::Config->set_inheritance("xdata", $xdref, $entry_key);
          // Detect XDATA loops
          if !(crate::Config->is_inheritance_path("xdata", $entry_key, $xdref)) {
            if (let $recurse_xdata = $xdataentry->get_xdata_refs) { // recurse
              $xdataentry->resolve_xdata($recurse_xdata);
            }

            // Whole entry XDATA reference so inherit all fields
            if (!defined($xdatum->{xdatafield})) {
              foreach let $field ($xdataentry->datafields()) { // set fields
                if $field == "ids" { // Never inherit aliases
                  continue;
                }
                $self->set_datafield($field, $xdataentry->get_field($field));

                // Record graphing information if required
                if (crate::Config->getoption("output_format") == "dot") {
                  crate::Config->set_graph("xdata", $xdataentry->get_field("citekey"), $entry_key, $field, $field);
                }
                  debug!("Setting field '{}' in entry '{}' via XDATA", field, entry_key);
              }
            }
            else { // Granular XDATA inheritance
              let $xdatafield = $xdatum->{xdatafield};
              let $xdataposition = $xdatum->{xdataposition};
              let $reffield = $xdatum->{reffield};
              let $refposition = $xdatum->{refposition};
              let $reffielddm = $dm->get_dm_for_field($reffield);
              let $xdatafielddm = $dm->get_dm_for_field($xdatafield);

              if !($reffielddm->{fieldtype} == $xdatafielddm->{fieldtype} &&
                      $reffielddm->{datatype} == $xdatafielddm->{datatype}) {
                biber_warn("Field '$reffield' in entry '$entry_key' which xdata references field '$xdatafield' in entry '$xdref' are not the same types, not resolving (section $secnum)", $self);
                $xdatum->{resolved} = 0;
                continue;
              }

              if !($xdataentry->get_field($xdatafield)) {
                biber_warn("Field '$reffield' in entry '$entry_key' references XDATA field '$xdatafield' in entry '$xdref' and this field does not exist, not resolving (section $secnum)", $self);
                $xdatum->{resolved} = 0;
                continue;
              }

              // Name lists
              if ($dm->field_is_type("list", "name", $reffield)) {
                if ($xdatum->{xdataposition} == '*') { // insert all positions from XDATA field
                  let bibentries = section.bibentries();
                  let be = bibentries.entry($xdatum->{xdataentries}[0]);
                  $self->get_field($reffield)->splice($xdataentry->get_field($xdatafield), $refposition);
                    debug!("Inserting at position {} in name field '{}' in entry '{}' via XDATA", refposition, reffield, entry_key);
                }
                else {
                  if !($xdataentry->get_field($xdatafield)->is_nth_name($xdataposition)) {
                    biber_warn("Field '$reffield' in entry '$entry_key' references field '$xdatafield' position $xdataposition in entry '$xdref' and this position does not exist, not resolving (section $secnum)", $self);
                    $xdatum->{resolved} = 0;
                    continue;
                  }

                  $self->get_field($reffield)->replace_name($xdataentry->get_field($xdatafield)->nth_name($xdataposition), $refposition);

                    debug!("Setting position {} in name field '{}' in entry '{}' via XDATA", refposition, reffield, entry_key);
                }
              }
              // Non-name lists
              else if ($dm->field_is_fieldtype("list", $reffield)) {
                if ($xdatum->{xdataposition} == '*') { // insert all positions from XDATA field
                  let bibentries = section.bibentries();
                  let be = bibentries.entry($xdatum->{xdataentries}[0]);
                  splice($self->get_field($reffield)->@*, $refposition-1, 1, $be->get_field($xdatum->{xdatafield})->@*);
                    debug!("Inserting at position {} in list field '{}' in entry '{}' via XDATA", refposition, reffield, entry_key);
                }
                else {
                  if !($xdataentry->get_field($xdatafield)->[$xdataposition-1]) {
                    biber_warn("Field '$reffield' in entry '$entry_key' references field '$xdatafield' position $xdataposition in entry '$xdref' and this position does not exist, not resolving (section $secnum)", $self);
                    $xdatum->{resolved} = 0;
                    continue;
                  }
                  $self->get_field($reffield)->[$refposition-1] =
                    $xdataentry->get_field($xdatafield)->[$refposition-1];
                    debug!("Setting position {} in list field '{}' in entry '{}' via XDATA", refposition, reffield, entry_key);
                }
              }
              // Non-list
              else {
                $self->set_datafield($reffield, $xdataentry->get_field($xdatafield));
                  debug!("Setting field '{}' in entry '{}' via XDATA", reffield, entry_key);
              }
            }
            $xdatum->{resolved} = 1;
          }
          else {
            biber_error("Circular XDATA inheritance between '$xdref'<->'$entry_key'");
          }
        }
      }
    }
  }

  /// Inherit fields from parent entry (as indicated by the crossref field)
  ///
  /// ```
  /// $entry->inherit_from($parententry);
  /// ```
  ///
  /// Takes a second crate::Entry object as argument
  /// Uses the crossref inheritance specifications from the .bcf
  fn inherit_from(self, $parent) {
    let $dmh = crate::config::get_dm_helpers();

    let secnum = crate::MASTER.get_current_section();
    let section = crate::MASTER.sections().get_section(secnum);

    let $target_key = $self->get_field("citekey"); // target/child key
    let $source_key = $parent->get_field("citekey"); // source/parent key

    // record the inheritance between these entries to prevent loops and repeats.
    crate::Config->set_inheritance("crossref", $source_key, $target_key);

    // Detect crossref loops
    if !(crate::Config->is_inheritance_path("crossref", $target_key, $source_key)) {
      // cascading crossrefs
      if (let $ppkey = parent.get_field("crossref")) {
        parent.inherit_from(section.bibentry(ppkey));
      }
    }
    else {
      biber_error("Circular inheritance between '$source_key'<->'$target_key'");
    }

    let $type        = $self->get_field("entrytype");
    let $parenttype  = $parent->get_field("entrytype");
    let $inheritance = crate::Config->getblxoption(undef, "inheritance");
    let %processed;
    // get defaults
    let $defaults = $inheritance->{defaults};
    // global defaults ...
    let $inherit_all = $defaults->{inherit_all};
    let $override_target = $defaults->{override_target};
    let $dignore = $defaults->{ignore};

    // override with type_pair specific defaults if they exist ...
    foreach let $type_pair ($defaults->{type_pair}->@*) {
      if (($type_pair->{source} == '*' || $type_pair->{source} == $parenttype) &&
          ($type_pair->{target} == '*' || $type_pair->{target} == $type)) {
        if $type_pair->{inherit_all} {
          $inherit_all = $type_pair->{inherit_all};
        }
        if $type_pair->{override_target} {
          $override_target = $type_pair->{override_target};
        }
        if defined($type_pair->{ignore}) {
          $dignore = $type_pair->{ignore};
        }
      }
    }

    // First process any fields that have special treatment
    foreach let $inherit ($inheritance->{inherit}->@*) {
      // Match for this combination of entry and crossref parent?
      foreach let $type_pair ($inherit->{type_pair}->@*) {
        if (($type_pair->{source} == '*' || $type_pair->{source} == $parenttype) &&
            ($type_pair->{target} == '*' || $type_pair->{target} == $type)) {
          foreach let $field ($inherit->{field}->@*) {
            // Skip for fields in the per-entry noinerit datafield set
            if (let $niset = crate::Config->getblxoption($secnum, "noinherit", undef, $target_key) &&
              exists($field->{target})) {
              if (first {$field->{target} == $_} $DATAFIELD_SETS{$niset}->@*) {
                continue;
              }
            }
            if !($parent->field_exists($field->{source})) {
              continue;
            }
            $processed{$field->{source}} = 1;
            // localise defaults according to field, if specified
            let $field_override_target = $field->{override_target}.unwrap_or("false");
            // Skip this field if requested
            if ($field->{skip}) {
              $processed{$field->{source}} = 1;
            }
            // Set the field if it doesn't exist or override is requested
            else if (!$self->field_exists($field->{target}) ||
                  $field_override_target == "true") {
                debug!("Entry '{}' is inheriting field '{}' as '{}' from entry '{}'", target_key, $field->{source}, $field->{target}, source_key);

              $self->set_datafield($field->{target}, $parent->get_field($field->{source}));

              // Ignore uniqueness information tracking for this inheritance?
              let $ignore = $inherit->{ignore} || $dignore;
              crate::Config->add_uniq_ignore($target_key, $field->{target}, $ignore);

              // Record graphing information if required
              if (crate::Config->getoption("output_format") == "dot") {
                crate::Config->set_graph("crossref", $source_key, $target_key, $field->{source}, $field->{target});
              }
            }
          }
        }
      }
    }

    // Now process the rest of the (original data only) fields, if necessary
    if ($inherit_all == "true") {
      let @fields = parent.datafields;

      // Special case: WITH NO override: If the child has any Xdate datepart,
      // don't inherit any Xdateparts from parent otherwise you can end up
      // with rather broken dates in the child. Remove such fields before we
      // start since it can't be done in the loop because as soon as one
      // Xdatepart field has been inherited, no more will be. Save removed
      // fields as this is needed when copying derived special date fields
      // below as these also need skipping if we have skipped the *date field
      // from which they were derived
      // WITH override: Remove all related dateparts so that there is no conflict
      // with inherited
      // ONLY DO THIS FOR ENTRIES WITH xDATE FIELDS - LEGACY YEAR/MONTH MESS THINGS UP
      // AND WE JUST IGNORE THEM FOR THIS PRE-PROCESSING STEP
      let filtered_fields = Vec::new();
      let removed_fields = Vec::new();
      for field in fields {
        if dmh.dateparts.contains(field) {
          if parent.get_field("datesplit") && self.get_field("datesplit") {
            if self.date_fields_exist(field) {
              if override_target == "true" {
                self.delete_date_fields(field); // clear out all date field parts in target
              } else {
                removed_fields.push(field.to_string());
                continue;
              }
            }
          }
        }
        filtered_fields.push(field.to_string());
      }
      let fields = filtered_fields;

      // copy derived date fields as these are technically data
      for datefield in &dmh.datefields {
        let df = datefield.strip_suffix("date");
        // Ignore derived date special fields from date fields which we have skipped
        // because they already exist in the child.
        if removed_fields.contains(datefield) {
          continue;
        }
        for dsf in ["dateunspecified", "datesplit", "datejulian",
                        "enddatejulian", "dateapproximate", "enddateapproximate",
                        "dateuncertain", "enddateuncertain", "yeardivision", "endyeardivision",
                        "era", "endera"] {
          let f = format!("{df}{dsf}");
          if let Some(ds) = parent.derivedfields.get(&f) {
            // Set unless the child has the *date datepart, otherwise you can
            // end up with rather broken dates in the child.
            self.derivedfields.insert(f, ds);
          }
        }
      }

      foreach let $field (@fields) {
        // Skip for fields in the per-entry noinherit datafield set
        if (let $niset = crate::Config->getblxoption($secnum, "noinherit", undef, $target_key)) {
          if (first {$field == $_} $DATAFIELD_SETS{$niset}->@*) {
            continue;
          }
        }
        if $processed{$field} { // Skip if we have already dealt with this field above
          continue;
        }

        // Set the field if it doesn't exist or override is requested
        if (!$self->field_exists($field) || $override_target == "true") {
            debug!("Entry '{}' is inheriting field '{}' from entry '{}'", target_key, field, source_key);

          $self->set_datafield($field, $parent->get_field($field));

          // Ignore uniqueness information tracking for this inheritance?
          crate::Config->add_uniq_ignore($target_key, $field, $dignore);

          // Record graphing information if required
          if (crate::Config->getoption("output_format") == "dot") {
            crate::Config->set_graph("crossref", $source_key, $target_key, $field, $field);
          }
        }
      }
    }
    // Datesplit is a special non datafield and needs to be inherited for any
    // validation checks which may occur later
    foreach let $df ($dmh->{datefields}->@*) {
      $df =~ s/date$//;
      if (let $ds = $parent->get_field("${df}datesplit")) {
        $self->set_field("${df}datesplit", $ds);
      }
    }

    return;
  }

  /// Dump crate::Entry object
  fn dump(self) {
    return pp($self);
  }
}