use Carp;
use crate::Annotation;
use crate::Constants;
use crate::DataModel;
use crate::Entries;
use crate::Entry;
use crate::Entry::Names;
use crate::Entry::Name;
use crate::Sections;
use crate::Section;
use crate::Utils;
use crate::Config;
use Encode;
use File::Spec;
use File::Slurper;
use File::Temp;
use Log::Log4perl qw(:no_extra_logdie_message);
use List::AllUtils qw( uniq first );
use XML::LibXML;
use XML::LibXML::Simple;
use Data::Dump qw(dump);
use Unicode::Normalize;
use Unicode::GCString;
use URI;

let $orig_key_order = {};

let $BIBLATEXML_NAMESPACE_URI = "http://biblatex-biber.sourceforge.net/biblatexml";
let $NS = "bltx";

// Determine handlers from data model
let $dm = crate::config::get_dm();
let $handlers = {
                "CUSTOM" => {"related" => \&_related,
                             "annotation" => \&_annotation},
                "field" => {
                            "default" => {
                                          "code"     => \&_literal,
                                          "date"     => \&_datetime,
                                          "entrykey" => \&_literal,
                                          "integer"  => \&_literal,
                                          "key"      => \&_literal,
                                          "literal"  => \&_literal,
                                          "range"    => \&_range,
                                          "verbatim" => \&_literal,
                                          "uri"      => \&_uri
                                         },
                            "xsv"     => {
                                           "entrykey" => \&_xsv,
                                           "keyword"  => \&_xsv,
                                           "option"   => \&_xsv,
                                         }
                           },
                "list" => {
                           "default" => {
                                         "entrykey" => \&_list,
                                         "key"      => \&_list,
                                         "literal"  => \&_list,
                                         "name"     => \&_name
                                        }
                          }
};

/// Main data extraction routine.
/// Accepts a data source identifier (filename in this case),
/// preprocesses the file and then looks for the passed keys,
/// creating entries when it finds them and passes out an
/// array of keys it didn't find.
fn extract_entries(filename, _encoding, keys) {
  // $encoding is ignored as it is always assumed to be UTF-8 for XML
  let secnum = crate::MASTER.get_current_section();
  let section = crate::MASTER.sections().get_section(secnum);
  let bibentries = section.bibentries();

  let @rkeys = $keys->@*;

  trace!("Entering extract_entries() in driver 'biblatexml'");

  // Check for empty files because they confuse btparse
  if !(check_empty($filename)) { // File is empty
    biber_warn("Data source '$filename' is empty, ignoring");
    return @rkeys;
  }

  // Get a reference to the correct sourcemap sections, if they exist
  let $smaps = [];
  // Maps are applied in order USER->STYLE->DRIVER
  if (defined(crate::Config->getoption("sourcemap"))) {
    // User maps
    if (let @m = grep {$_->{datatype} == "biblatexml" && $_->{level} == "user" } @{crate::Config->getoption("sourcemap")} ) {
      push $smaps->@*, @m;
    }
    // Style maps
    // Allow multiple style maps from multiple \DeclareStyleSourcemap
    if (let @m = grep {$_->{datatype} == "biblatexml" && $_->{level} == "style" } @{crate::Config->getoption("sourcemap")} ) {
      push $smaps->@*, @m;
    }
    // Driver default maps
    if (let $m = first {$_->{datatype} == "biblatexml" && $_->{level} == "driver"} @{crate::Config->getoption("sourcemap")} ) {
      push $smaps->@*, $m;
    }
  }

  // Log that we found a data file
  info!("Found BibLaTeXML data file '{}'", filename);

  // Set up XML parser and namespace
  let $xml = slurp_switchr($filename)->$*;
  $xml = NFD($xml);// Unicode NFD boundary
  let $bltxml = XML::LibXML->load_xml(string => $xml);
  let $xpc = XML::LibXML::XPathContext->new($bltxml);
  $xpc->registerNs($NS, $BIBLATEXML_NAMESPACE_URI);

  if section.is_allkeys()) {
      debug!("All citekeys will be used for section '{}'", secnum);
    // Loop over all entries, creating objects
    for entry in ($xpc->findnodes("/$NS:entries/$NS:entry")) {
        debug!('Parsing BibLaTeXML entry object {}', $entry->nodePath);

      // If an entry has no key, ignore it and warn
      if !($entry->hasAttribute("id")) {
        biber_warn("Invalid or undefined BibLaTeXML entry key in file '$filename', skipping ...");
        continue;
      }

      let $key = $entry->getAttribute("id");

      // Check if this key has already been registered as a citekey alias, if
      // so, the key takes priority and we delete the alias
      if ($section->get_citekey_alias($key)) {
        biber_warn("Citekey alias '$key' is also a real entry key, skipping ...");
        $section->get_citekey_alias($key);
      }

      // Any secondary keys?
      // We can't do this with a driver entry for the IDS field as this needs
      // an entry object creating first and the whole point of aliases is that
      // there is no entry object
      for id in ($entry->findnodes("./$NS:ids/$NS:key")) {
        let $idstr = $id->textContent();

        // Skip aliases which are also real entry keys
        if ($section->has_everykey($idstr)) {
          biber_warn("Citekey alias '$idstr' is also a real entry key, skipping ...");
          continue;
        }

        // Warn on conflicting aliases
        if (let $otherid = $section->get_citekey_alias($idstr)) {
          if ($otherid != $key) {
            biber_warn("Citekey alias '$idstr' already has an alias '$otherid', skipping ...");
          }
        }
        else {
          // Since this is allkeys, we are guaranteed that the real entry for the alias
          // will be available
          $section->set_citekey_alias($idstr, $key);
            debug!("Citekey '{}' is an alias for citekey '{}'", idstr, key);
        }
      }

      // If we've already seen a case variant, warn
      if (let $okey = $section->has_badcasekey($key)) {
        biber_warn("Possible typo (case mismatch): '$key' and '$okey' in file '$filename', skipping '$key' ...");
      }

      // If we've already seen this key, ignore it and warn
      if ($section->has_everykey($key)) {
        biber_warn("Duplicate entry key: '$key' in file '$filename', skipping ...");
        continue;
      }
      else {
        $section->add_everykey($key);
      }

      // Record a key->datasource name mapping for error reporting
      $section->set_keytods($key, $filename);

      create_entry($key, $entry, $filename, $smaps, \@rkeys);

      // We do this as otherwise we have no way of determining the origing .bib entry order
      // We need this in order to do sorting=none + allkeys because in this case, there is no
      // "citeorder" because nothing is explicitly cited and so "citeorder" means .bib order
      push @{$orig_key_order->{$filename}}, $key;

    }

    // if allkeys, push all bibdata keys into citekeys (if they are not already there)
    // We are using the special "orig_key_order" array which is used to deal with the
    // situation when sorting=none and allkeys is set. We need an array rather than the
    // keys from the bibentries hash because we need to preserver the original order of
    // the .bib as in this case the sorting sub "citeorder" means "bib order" as there are
    // no explicitly cited keys
    $section->add_citekeys(@{$orig_key_order->{$filename}});
      debug!("Added all citekeys to section '{}': {}", secnum, join(', ', $section.get_citekeys()));
  }
  else {
    // loop over all keys we're looking for and create objects
      debug!("Wanted keys: {}", join(', ', $keys->@*));
    for wanted_key in ($keys->@*) {

        debug!("Looking for key '{}' in BibLaTeXML file '{}'", wanted_key, filename);
      if (let @entries = $xpc->findnodes("/$NS:entries/$NS:entry[\@id='$wanted_key']")) {
        // Check to see if there is more than one entry with this key and warn if so
        if ($#entries > 0) {
          biber_warn("Found more than one entry for key '$wanted_key' in '$filename': " .
                       join(',', map {$_->getAttribute("id")} @entries) . ' - skipping duplicates ...');
        }
        let $entry = $entries[0];

          debug!("Found key '{}' in BibLaTeXML file '{}'", wanted_key, filename);
          debug!("Parsing BibLaTeXML entry object {}", $entry->nodePath);
        // See comment above about the importance of the case of the key
        // passed to create_entry()
        // Skip creation if it's already been done, for example, via a citekey alias
        if !section.bibentries().entry_exists(wanted_key) {
          // Record a key->datasource name mapping for error reporting
          section.set_keytods(wanted_key, filename);

          create_entry($wanted_key, $entry, $filename, $smaps, \@rkeys);
        }
        // found a key, remove it from the list of keys we want
        @rkeys = grep {$wanted_key != $_} @rkeys;
      }
      else if ($xpc->findnodes("/$NS:entries/$NS:entry/$NS:id[text()='$wanted_key']")) {
        let $key = $xpc->findnodes("/$NS:entries/$NS:entry/\@id");
          debug!("Citekey '{}' is an alias for citekey '{}'", wanted_key, key);
        $section->set_citekey_alias($wanted_key, $key);

        // Make sure there is a real, cited entry for the citekey alias
        // just in case only the alias is cited
        if !section.bibentries().entry_exists(key) {
          let entry = xpc.findnodes("/$NS:entries/$NS:entry/[\@id='$key']");

          // Record a key->datasource name mapping for error reporting
          section.set_keytods(key, filename);

          create_entry($key, entry, $filename, $smaps, \@rkeys);
          section.add_citekeys(key);
        }

        // found a key, remove it from the list of keys we want
        rkeys.retain(|k| wanted_key != k);
      }
        debug!("Wanted keys now: ", join(', ', @rkeys));
    }
  }

  return @rkeys;
}

/// Create a crate::Entry object from an entry found in a biblatexml data source
fn create_entry(key, entry, datasource, smaps, rkeys) {
  let secnum = crate::MASTER.get_current_section();
  let section = crate::MASTER.sections().get_section(secnum);

  let dm = crate::config::get_dm();
  let bibentries = section.bibentries();

  let %newentries; // In case we create a new entry in a map

  // Datasource mapping applied in $smap order (USER->STYLE->DRIVER)
  for smap in ($smaps->@*) {
    $smap->{map_overwrite} = $smap->{map_overwrite}.unwrap_or(0); // default
    let $level = $smap->{level};

  'MAP:    for map in (@{$smap->{map}}) {

      // Skip if this map element specifies a particular refsection and it is not this one
      if (exists($map->{refsection})) {
        if $secnum != $map->{refsection} {
          continue;
        }
      }

      // defaults to the entrytype unless changed below
      let $last_type = $entry->getAttribute("entrytype");
      let $last_field = undef;
      let $last_fieldval = undef;
      let $cnerror;

      let @imatches; // For persisting parenthetical matches over several steps

      // Check pertype restrictions
      // Logic is "-(-P v Q)" which is equivalent to "P & -Q" but -Q is an array check so
      // messier to write than Q
      if !(!exists($map->{per_type}) ||
              first {lc($_->{content}) == $entry->type} @{$map->{per_type}}) {
        continue;
      }

      // Check negated pertype restrictions
      if (exists($map->{per_nottype}) &&
          first {lc($_->{content}) == $entry->getAttribute("entrytype")} @{$map->{per_nottype}}) {
        continue;
      }

      // Check per_datasource restrictions
      // Don't compare case insensitively - this might not be correct
      // Logic is "-(-P v Q)" which is equivalent to "P & -Q" but -Q is an array check so
      // messier to write than Q
      if !(!exists($map->{per_datasource}) ||
              first {$_->{content} == $datasource} @{$map->{per_datasource}}) {
        continue;
      }

      // Set up any mapping foreach loop
      let @maploop = ("");
      if (let $foreach = $map->{map_foreach}) {
        // just a field name, make it XPATH
        if ($foreach !~ m|/|) {
          $foreach = "./bltx:$foreach";
        }

        if (let $felist = $entry->findnodes($foreach)) {
          @maploop = split(/\s*,\s*/, $felist);
        }
      }

      for maploop in (@maploop) {
        let $MAPUNIQVAL;
        // loop over mapping steps
        for step in (@{$map->{map_step}}) {

          // entry deletion. Really only useful with allkeys or tool mode
          if ($step->{map_entry_null}) {
              debug!("Source mapping (type={}, key={}): Ignoring entry completely", level, key);
            return 0;           // don't create an entry at all
          }

          // new entry
          if (let $newkey = maploopreplace($step->{map_entry_new}, $maploop)) {
            let $newentrytype;
            if !($newentrytype = maploopreplace($step->{map_entry_newtype}, $maploop)) {
              biber_warn("Source mapping (type=$level, key=$key): Missing type for new entry '$newkey', skipping step ...");
              continue;
            }
              debug!("Source mapping (type={}, key={}): Creating new entry with key '$newkey'", level, key);
            let $newentry = XML::LibXML::Element->new("$NS:entry");
            $newentry->setAttribute("id", NFC($newkey));
            $newentry->setAttribute("entrytype", NFC($newentrytype));

            // found a new entry key, remove it from the list of keys we want since we
            // have "found" it by creating it
            $rkeys->@* = grep {$newkey != $_} $rkeys->@*;

            // for allkeys sections initially
            if section.is_allkeys() {
              section.add_citekeys(newkey);
            }
            $newentries{$newkey} = $newentry;
          }

          // entry clone
          if (let $prefix = maploopreplace($step->{map_entry_clone}, $maploop)) {
              debug!("Source mapping (type={}, key={}): cloning entry with prefix '{}'", level, key, prefix);
            // Create entry with no sourcemapping to avoid recursion
            create_entry("$prefix$key", $entry);

            // found a prefix clone key, remove it from the list of keys we want since we
            // have "found" it by creating it along with its clone parent
            $rkeys->@* = grep {"$prefix$key" != $_} $rkeys->@*;
            // Need to add the clone key to the section if allkeys is set since all keys are cleared
            // for allkeys sections initially
            if section.is_allkeys() {
              section.add_citekeys(&[format!("{prefix}{key}")]);
            }
          }

          // An entry created by map_entry_new previously can be the target for field setting
          // options
          // A newly created entry as target of operations doesn't make sense in all situations
          // so it's limited to being the target for field sets
          let $etarget;
          let $etargetkey;
          if ($etargetkey = maploopreplace($step->{map_entrytarget}, $maploop)) {
            if !($etarget = $newentries{$etargetkey}) {
              biber_warn("Source mapping (type=$level, key=$key): Dynamically created entry target '$etargetkey' does not exist skipping step ...");
              continue;
            }
          }
          else {             // default is that we operate on the same entry
            $etarget = $entry;
            $etargetkey = $key;
          }

          // Entrytype map
          if (let $typesource = maploopreplace($step->{map_type_source}, $maploop)) {
            $typesource = lc($typesource);
            if !($etarget->getAttribute("entrytype") == $typesource) {
              // Skip the rest of the map if this step doesn't match and match is final
              if ($step->{map_final}) {
                  debug!("Source mapping (type={}, key={}): Entry type is '", level, etargetkey, $etarget->getAttribute("entrytype") . "' but map wants '$typesource' and step has 'final' set, skipping rest of map ...");
                continue 'MAP;
              }
              else {
                // just ignore this step
                  debug!("Source mapping (type={}, key={}): Entry type is '", level, etargetkey, $etarget->getAttribute("entrytype") . "' but map wants '$typesource', skipping step ...");
                  continue;
              }
            }
            // Change entrytype if requested
            $last_type = $etarget->getAttribute("entrytype");
            let $t = lc(maploopreplace($step->{map_type_target}, $maploop));
              debug!("Source mapping (type={}, key={}): Changing entry type from '{}' to {}", level, etargetkey, last_type, t);
            $etarget->setAttribute("entrytype", NFC($t));
          }

          let $fieldcontinue = 0;
          let $xp_nfieldsource_s;
          let $xp_nfieldsource;
          let $xp_fieldsource_s;
          let $xp_fieldsource;
          // Negated source field map
          if ($xp_nfieldsource_s = _getpath(maploopreplace($step->{map_notfield}, $maploop))) {
            $xp_nfieldsource = XML::LibXML::XPathExpression->new($xp_nfieldsource_s);

            if ($etarget->exists($xp_nfieldsource)) {
              if ($step->{map_final}) {
                  debug!("Source mapping (type={}, key={}): Field xpath '{}' exists and step has 'final' set, skipping rest of map ...", level, etargetkey, xp_nfieldsource_s);
                continue 'MAP;
              }
              else {
                  debug!("Source mapping (type={}, key={}): Field xpath '{}' exists, skipping step ...", level, etargetkey, xp_nfieldsource_s);
              }
            }
            $fieldcontinue = 1;
          }

          // \cite{key}   -> is_cite(key)=true, is_specificcitekey(key)=true
          // \nocite{key} -> is_nocite(key)=true, is_specificcitekey(key)=true
          // \nocite{*}   -> is_allkeys_nocite=true
          // Check entry cited/nocited verbs

          // \cite{key} or \nocite{key}
          if ($step->{map_entrykey_citedornocited}) {
            if (!section.is_specificcitekey(key)) { // check if NOT \cited{} and NOT \nocited{}
              if ($step->{map_final}) {
                  debug!("Source mapping (type={}, key={}): Key is neither \\cited nor \\nocited and step has 'final' set, skipping rest of map ...", level, key);
                continue 'MAP;
              }
              else {
                // just ignore this step
                  debug!("Source mapping (type={}, key={}): Key is neither \\cited nor \\nocited, skipping step ...", level, key);
                continue;
              }
            }
          }

          // \cite{key}
          if ($step->{map_entrykey_cited}) {
            if !section.is_cite(key) { // check if NOT cited
              if ($step->{map_final}) {
                  debug!("Source mapping (type={}, key={}): Key is not explicitly \\cited and step has 'final' set, skipping rest of map ...", level, key);
                continue 'MAP;
              }
              else {
                // just ignore this step
                  debug!("Source mapping (type={}, key={}): Key is not explicitly \\cited, skipping step ...", level, key);
                continue;
              }
            }
          }

          // \nocite{key}
          if ($step->{map_entrykey_nocited}) {
            // If cited, don't want to do the allkeys_nocite check as this overrides
            if section.is_cite(key) ||
                (!section.is_nocite(key) && !section.is_allkeys_nocite()) { // check if NOT nocited
              if ($step->{map_final}) {
                  debug!("Source mapping (type={}, key={}): Key is not \\nocited and step has 'final' set, skipping rest of map ...", level, key);
                continue 'MAP;
              }
              else {
                // just ignore this step
                  debug!("Source mapping (type={}, key={}): Key is not \\nocited, skipping step ...", level, key);
                  continue;
              }
            }
          }

          // \nocite{key} or \nocite{*}
          if ($step->{map_entrykey_allnocited}) {
            if (!section.is_allkeys_nocite()) { // check if NOT allnoncited
              if ($step->{map_final}) {
                  debug!("Source mapping (type={}, key={}): Key is not \\nocite{*}'ed and step has 'final' set, skipping rest of map ...", level, key);
                continue 'MAP;
              }
              else {
                // just ignore this step
                  debug!("Source mapping (type={}, key={}): Key is not \\nocite{*}'ed, skipping step ...", level, key);
                continue;
              }
            }
          }

          // \nocite{*}
          if ($step->{map_entrykey_starnocited}) {
            if (section.is_allkeys_nocite() && (section.is_cite(key) || section.is_nocite(key))) { // check if NOT nocited
              if ($step->{map_final}) {
                  debug!("Source mapping (type={}, key={}): Key is \\nocite{*}'ed but also either \\cite'd or explicitly \\nocited and step has 'final' set, skipping rest of map ...", level, key);
                continue 'MAP;
              }
              else {
                // just ignore this step
                  debug!("Source mapping (type={}, key={}): Key is \\nocite{*}'ed but also either \\cite'd or explicitly \\nocited, skipping step ...", level, key);
                continue;
              }
            }
          }

          // Field map
          if ($xp_fieldsource_s = _getpath(maploopreplace($step->{map_field_source}, $maploop))) {
            $xp_fieldsource = XML::LibXML::XPathExpression->new($xp_fieldsource_s);

            // key is a pseudo-field. It's guaranteed to exist so
            // just check if that's what's being asked for
            if !($etarget->exists($xp_fieldsource)) {
              // Skip the rest of the map if this step doesn't match and match is final
              if ($step->{map_final}) {
                  debug!("Source mapping (type={}, key={}): No field xpath '{}' and step has 'final' set, skipping rest of map ...", level, etargetkey, xp_fieldsource_s);
                continue 'MAP;
              }
              else {
                // just ignore this step
                  debug!("Source mapping (type={}, key={}): No field xpath '{}', skipping step ...", level, etargetkey, xp_fieldsource_s);
                continue;
              }
            }
            $fieldcontinue = 1;
          }

          if ($fieldcontinue) {
            $last_field = $etarget->findnodes($xp_fieldsource)->get_node(1)->nodeName;
            $last_fieldval = $etarget->findvalue($xp_fieldsource);

            let $negmatch = 0;
            // Negated matches are a normal match with a special flag
            if (let $nm = $step->{map_notmatch}) {
              $step->{map_match} = $nm;
              $negmatch = 1;
            }

            let $caseinsensitive = 0;
            let $mi;
            // Case insensitive matches are a normal match with a special flag
            if ($mi = $step->{map_matchi} || $mi = $step->{map_notmatchi}) {
              $step->{map_match} = $mi;
              $caseinsensitive = 1;
            }

            let $caseinsensitives = 0;
            let $mis;
            // Case insensitive matches are normal matches with a special flag
            if ($mis = $step->{map_matchesi}) {
              $step->{map_matches} = $mis;
              $caseinsensitives = 1;
            }

            if (let $ms = $step->{map_matches}) {
              let @ms = split(/\s*,\s*/,$ms);
              let @rs = split(/\s*,\s*/,$step->{map_replace});
              if !(scalar(@ms) == scalar(@rs)) {
                debug!("Source mapping (type={}, key={}): Different number of fixed matches vs replaces, skipping ...", level, etargetkey);
                continue;
              }
              for (let $i = 0; $i <= $#ms; $i++) {
                if (($caseinsensitives && unicase::eq(last_fieldval, $ms[$i]))
                    || ($last_fieldval == $ms[$i])) {
                  $etarget->set(encode("UTF-8", NFC($xp_fieldsource_s)), $rs[$i]);
                }
              }
            }

            // map fields to targets
            if (let $m = maploopreplace($step->{map_match}, $maploop)) {
              if (defined($step->{map_replace})) { // replace can be null

                // Can't modify entrykey
                if (lc($xp_fieldsource_s) == './@id') {
                  debug!("Source mapping (type={}, key={}): Field xpath '{}' is entrykey- cannot remap the value of this field, skipping ...", level, etargetkey, xp_fieldsource_s);
                  continue;
                }

                let $r = maploopreplace($step->{map_replace}, $maploop);
                  debug!("Source mapping (type={}, key={}): Doing match/replace '{}' -> '{}' on field xpath '{}'", level, etargetkey, m, r, xp_fieldsource_s);

                if !(_changenode($etarget, $xp_fieldsource_s, ireplace($last_fieldval, $m, $r, $caseinsensitive)), \$cnerror) {
                  biber_warn("Source mapping (type=$level, key=$etargetkey): $cnerror");
                }
              }
              else {
                // Now re-instate any unescaped $1 .. $9 to get round these being
                // dynamically scoped and being null when we get here from any
                // previous map_match
                // Be aware that imatch() uses m//g so @imatches can have multiple paren group
                // captures which might be useful
                $m =~ s/(?<!\\)\$(\d)/$imatches[$1-1]/ge;
                if !(@imatches = imatch($last_fieldval, $m, $negmatch)) {
                  // Skip the rest of the map if this step doesn't match and match is final
                  if ($step->{map_final}) {
                      debug!("Source mapping (type={}, key={}): Field xpath '{}' does not match '{}' and step has 'final' set, skipping rest of map ...", level, etargetkey, xp_fieldsource_s, m);
                    continue 'MAP;
                  }
                  else {
                    // just ignore this step
                      debug!("Source mapping (type={}, key={}): Field xpath '{}' does not match '{}', skipping step ...", level, etargetkey, xp_fieldsource_s, m);
                    continue;
                  }
                }
              }
            }

            // Set to a different target if there is one
            if (let $xp_target_s = _getpath(maploopreplace($step->{map_field_target}, $maploop))) {
              let $xp_target = XML::LibXML::XPathExpression->new($xp_target_s);

              // Can't remap entry key pseudo-field
              if (lc($xp_target_s) == './@id') {
                  debug!("Source mapping (type={}, key={}): Field xpath '{}' is entrykey - cannot map this to a new field as you must have an entrykey, skipping ...", level, etargetkey, xp_fieldsource_s);
                  continue;
              }

            if ($etarget->exists($xp_target)) {
                if ($map->{map_overwrite}.or($smap->{map_overwrite})) {
                    debug!("Source mapping (type={}, key={}): Overwriting existing field xpath '{}'", level, etargetkey, xp_target_s);
                }
                else {
                    debug!("Source mapping (type={}, key={}): Field xpath '{}' is mapped to field xpath '{}' but both are defined, skipping ...", level, etargetkey, xp_fieldsource_s, xp_target_s);
                    continue;
                }
              }
              if !(_changenode($etarget, $xp_target_s, $xp_fieldsource_s, \$cnerror)) {
                biber_warn("Source mapping (type=$level, key=$key): $cnerror");
              }
              $etarget->findnodes($xp_fieldsource)->get_node(1)->unbindNode();
            }
          }

          // field changes
          if (let $xp_node_s = _getpath(maploopreplace($step->{map_field_set}, $maploop))) {
            let $xp_node = XML::LibXML::XPathExpression->new($xp_node_s);

            // Deal with special tokens
            if ($step->{map_null}) {
                debug!("Source mapping (type={}, key={}): Deleting field xpath '{}'", level, etargetkey, xp_node_s);
              $etarget->findnodes($xp_node)->get_node(1)->unbindNode();
            }
            else {
              if ($etarget->exists($xp_node)) {
                if !($map->{map_overwrite}.or($smap->{map_overwrite})) {
                  if ($step->{map_final}) {
                    // map_final is set, ignore and skip rest of step
                      debug!("Source mapping (type={}, key={}): Field xpath '{}' exists, overwrite is not set and step has 'final' set, skipping rest of map ...", level, etargetkey, xp_node_s);
                    continue 'MAP;
                  }
                  else {
                    // just ignore this step
                      debug!("Source mapping (type={}, key={}): Field xpath '{}' exists and overwrite is not set, skipping step ...", level, etargetkey, xp_node_s);
                    continue;
                  }
                }
              }

              let $orig = "";
              // If append or appendstrict is set, keep the original value
              // and append the new.
              if ($step->{map_append} || $step->{map_appendstrict}) {
                $orig = $etarget->findvalue($xp_node) || "";
              }

              if ($step->{map_origentrytype}) {
                if !last_type {
                  continue;
                }
                  debug!("Source mapping (type={}, key={}): Setting xpath '{}' to '{}{}'", level, etargetkey, xp_node_s, orig, last_type);

                if !(_changenode($etarget, $xp_node_s, appendstrict_check($step, $orig, $last_type), \$cnerror)) {
                  biber_warn("Source mapping (type=$level, key=$key): $cnerror");
                }
              }
              else if ($step->{map_origfieldval}) {
                if !last_fieldval {
                  continue;
                }
                  debug!("Source mapping (type={}, key={}): Setting field xpath '{}' to '{}{}'", level, etargetkey, xp_node_s, orig, last_fieldval);
                if !(_changenode($etarget, $xp_node_s, appendstrict_check($step, $orig, $last_fieldval), \$cnerror)) {
                  biber_warn("Source mapping (type=$level, key=$etargetkey): $cnerror");
                }
              }
              else if ($step->{map_origfield}) {
                if !last_field {
                  continue;
                }
                  debug!("Source mapping (type={}, key={}): Setting field xpath '{}' to '{}{}'", level, etargetkey, xp_node_s, orig, last_field);
                if !(_changenode($etarget, $xp_node_s, appendstrict_check($step, $orig, $last_field), \$cnerror)) {
                  biber_warn("Source mapping (type=$level, key=$etargetkey): $cnerror");
                }
              }
              else {
                let $fv = maploopreplace($step->{map_field_value}, $maploop);
                // Now re-instate any unescaped $1 .. $9 to get round these being
                // dynamically scoped and being null when we get here from any
                // previous map_match
                $fv =~ s/(?<!\\)\$(\d)/$imatches[$1-1]/ge;
                  debug!("Source mapping (type={}, key={}): Setting field xpath '{}' to '{}{}'", level, etargetkey, xp_node_s, orig, fv);
                if !(_changenode($etarget, $xp_node_s, appendstrict_check($step, $orig, $fv), \$cnerror)) {
                  biber_warn("Source mapping (type=$level, key=$key): $cnerror");
                }
              }
            }
          }
        }
      }
    }
  }

  // Need to also instantiate fields in any new entries created by map
  for e in ($entry, values %newentries) {
    if !e {             // newentry might be undef
      continue;
    }

    let bibentry = crate::Entry::new();
    let k = e.getAttribute("id");
    bibentry.set_field("citekey", k);
      debug!("Creating entry with key '{}'", k);

    // We put all the fields we find modulo field aliases into the object.
    // Validation happens later and is not datasource dependent
    for f in (uniq map { if (_norm($_->nodeName) == "names") { $_->getAttribute("type") }
                              else { $_->nodeName()} }  $e->findnodes('*')) {

      // We have to process local options as early as possible in order
      // to make them available for things that need them like name parsing
      if (_norm($f) == "options") {
        if (let $node = $entry->findnodes("./$NS:options")->get_node(1)) {
          process_entry_options($k, [ split(/\s*,\s*/, $node->textContent()) ], $secnum);
        }
      }

      // Now run any defined handler
      if ($dm->is_field(_norm($f))) {
        let $handler = _get_handler($f);
        $handler->($bibentry, $e, $f, $k);
      }
    }

    bibentry.set_field("entrytype", e.getAttribute("entrytype"));
    bibentry.set_field("datatype", "biblatexml");
    bibentries.add_entry(k, bibentry);
  }
  return;
}

// Annotations are special - there is a literal field and also more complex annotations
fn _annotation(bibentry, entry, f, key) {
  for node in ($entry->findnodes("./$f")) {
    let $field = $node->getAttribute("field");
    let $name = $node->getAttribute("name") || "default";
    let $literal = $node->getAttribute("literal") || '0';
    let $ann = $node->textContent();
    let $item = $node->getAttribute("item");
    let $part = $node->getAttribute("part");
    if ($field) {// Complex metadata annotation for another field
      if ($part) {
        crate::Annotation->set_annotation("part", $key, $field, $name, $ann, $literal, $item, $part);
      }
      else if ($item) {
        crate::Annotation->set_annotation("item", $key, $field, $name, $ann, $literal, $item);
      }
      else {
        crate::Annotation->set_annotation("field", $key, $field, $name, $ann, $literal);
      }
    }
    else {// Generic entry annotation
      $bibentry->set_datafield(_norm($f), $node->textContent());
    }
  }
  return;
}

// Related entries
fn _related(bibentry, entry, f, key) {
  let $Srx = crate::Config->getoption("xsvsep");
  let $S = qr/$Srx/;
  let $node = $entry->findnodes("./$f")->get_node(1);
  for item in ($node->findnodes("./$NS:list/$NS:item")) {
    $bibentry->set_datafield("related", [ split(/$S/, $item->getAttribute("ids")) ]);
    $bibentry->set_datafield("relatedtype", $item->getAttribute("type"));
    if (let $string = $item->getAttribute("string")) {
      $bibentry->set_datafield("relatedstring", $string);
    }
    if (let $string = $item->getAttribute("options")) {
      $bibentry->set_datafield("relatedoptions",
                               [ split(/$S/, $item->getAttribute("relatedoptions")) ]);
    }
  }
  return;
}

// literal fields
fn _literal(bibentry, entry, f, key) {
  let $node = $entry->findnodes("./$f")->get_node(1);
  let $setval = $node->textContent();
  let $xdmi = crate::Config->getoption("xdatamarker");
  let $xnsi = crate::Config->getoption("xnamesep");

  // XDATA is special, if found, set it
  if (let $xdatav = $node->getAttribute("xdata")) {
    $xdatav = "$xdmi$xnsi$xdatav"; // normalise to same as bibtex input
    $bibentry->add_xdata_ref(_norm($f), $xdatav);
    $setval = $xdatav;
  }

  // eprint is special case
  if ($f == "$NS:eprint") {
    $bibentry->set_datafield("eprinttype", $node->getAttribute("type"));
    if (let $ec = $node->getAttribute("class")) {
      $bibentry->set_datafield("eprintclass", $ec);
    }
  }
  else {
    $bibentry->set_datafield(_norm($f), $setval);
  }

  return;
}

// xSV field
fn _xsv(bibentry, entry, f, key) {
  let $node = $entry->findnodes("./$f")->get_node(1);

  // XDATA is special
  if unicase::eq(_norm($f), "xdata") {
    // Just split with no XDATA setting on list items
    let $value = _split_list($bibentry, $node, $key, $f, 1);
    $bibentry->add_xdata_ref("xdata", $value);
    $bibentry->set_datafield(_norm($f), $value);
  }
  else {
    $bibentry->set_datafield(_norm($f), _split_list($bibentry, $node, $key, $f));
  }

  return;
}


// uri fields
fn _uri(bibentry, entry, f, key) {
  let $node = $entry->findnodes("./$f")->get_node(1);
  let $setval = $node->textContent();
  let $xdmi = crate::Config->getoption("xdatamarker");
  let $xnsi = crate::Config->getoption("xnamesep");

  // XDATA is special, if found, set it
  if (let $xdatav = $node->getAttribute("xdata")) {
    $xdatav = "$xdmi$xnsi$xdatav"; // normalise to same as bibtex input
    $bibentry->add_xdata_ref(_norm($f), $xdatav);
    $setval = $xdatav;
  }
  else {
    // URL escape if it doesn't look like it already is
    // This is useful if we are generating URLs automatically with maps which may
    // contain UTF-8 from other fields
    if !($setval =~ /\%/) {
      $setval = URI->new($setval)->as_string;
    }
  }

  $bibentry->set_datafield(_norm($f), $setval);

  return;
}


// List fields
fn _list(bibentry, entry, f, key) {
  let $node = $entry->findnodes("./$f")->get_node(1);

  $bibentry->set_datafield(_norm($f), _split_list($bibentry, $node, $key, $f));

  return;
}

// Range fields
fn _range(bibentry, entry, f, key) {
  let $node = $entry->findnodes("./$f")->get_node(1);
  let $xdmi = crate::Config->getoption("xdatamarker");
  let $xnsi = crate::Config->getoption("xnamesep");

  // XDATA is special, if found, set it
  if (let $xdatav = $node->getAttribute("xdata")) {
    $xdatav = "$xdmi$xnsi$xdatav"; // normalise to same as bibtex input
    $bibentry->add_xdata_ref(_norm($f), $xdatav);
    $bibentry->set_datafield(_norm($f), [$xdatav]);
    return;
  }

  // List of ranges/values
  if (let @rangelist = $node->findnodes("./$NS:list/$NS:item")) {
    let $rl;
    for range in (@rangelist) {
      push $rl->@*, _parse_range_list($range);
    }
    $bibentry->set_datafield(_norm($f), $rl);
  }

  return;
}

// Date fields
// NOTE - the biblatex options controlling era, approximate and uncertain meta-information
// output are in the .bcf but biber does not used them as it always outputs this information
fn _datetime(bibentry, entry, f, key) {
  let secnum = crate::MASTER.get_current_section();
  let section = crate::MASTER.sections().get_section(secnum);
  let $ds = $section->get_keytods($key);

  for node in ($entry->findnodes("./$f")) {

    let $datetype = $node->getAttribute("type").unwrap_or("");

    $bibentry->set_field("${datetype}datesplit", 1);

    if (let $start = $node->findnodes("./$NS:start")) { // Date range
      let $end = $node->findnodes("./$NS:end");

      // Start of range
      // Using high-level range parsing sub in order to get unspec
      if (let ($sdate, undef, undef, $unspec) = parse_date_range($bibentry,
                                                                $datetype,
                                                                $start->get_node(1)->textContent())) {

        // Save julian
        if $CONFIG_DATE_PARSERS{start}->julian {
          $bibentry->set_field($datetype . "datejulian", 1);
        }
        // Save approximate information
        if $CONFIG_DATE_PARSERS{start}->approximate {
          $bibentry->set_field($datetype . "dateapproximate", 1);
        }

        // Save uncertain date information
        if $CONFIG_DATE_PARSERS{start}->uncertain {
          $bibentry->set_field($datetype . "dateuncertain", 1);
        }

        // Date had EDTF 5.2.2 unspecified format
        // This does not differ for *enddate components as these are split into ranges
        // from non-ranges only
        if ($unspec) {
          $bibentry->set_field($datetype . "dateunspecified", $unspec);
        }

        if !($CONFIG_DATE_PARSERS{start}->missing("year")) {
          $bibentry->set_datafield($datetype . "year", $sdate->year);
          // Save era date information
          $bibentry->set_field($datetype . "era", lc($sdate->secular_era));
        }

        if !($CONFIG_DATE_PARSERS{start}->missing("month")) {
          $bibentry->set_datafield($datetype . "month", $sdate->month);
        }

        if !($CONFIG_DATE_PARSERS{start}->missing("day")) {
          $bibentry->set_datafield($datetype . "day", $sdate->day);
        }

        // Save start yeardivision date information
        if (let $yeardivision = $CONFIG_DATE_PARSERS{start}->yeardivision) {
          $bibentry->set_field($datetype . "yeardivision", $yeardivision);
        }

        // must be an hour if there is a time but could be 00 so use defined()
        if !($CONFIG_DATE_PARSERS{start}->missing("time")) {
          $bibentry->set_datafield($datetype . "hour", $sdate->hour);
          $bibentry->set_datafield($datetype . "minute", $sdate->minute);
          $bibentry->set_datafield($datetype . "second", $sdate->second);
          if !($sdate->time_zone->is_floating) { // ignore floating timezones
            $bibentry->set_datafield($datetype . "timezone", tzformat($sdate->time_zone->name));
          }
        }
      }
      else {
        biber_warn("Datamodel: Entry '$key' ($ds): Invalid format '" . $start->get_node(1)->textContent() . "' of date field '$f' range start - ignoring", $bibentry);
      }

      // End of range
      let $edate = parse_date_end($end->get_node(1)->textContent());
      if (defined($edate)) { // no parse error
        if ($edate) { // not an empty range

          // Save julian
          if $CONFIG_DATE_PARSERS{end}->julian {
            $bibentry->set_field($datetype . "enddatejulian", 1);
          }

          // Save approximate information
          if $CONFIG_DATE_PARSERS{end}->approximate {
            $bibentry->set_field($datetype . "enddateapproximate", 1);
          }

          // Save uncertain date information
          if $CONFIG_DATE_PARSERS{end}->uncertain {
            $bibentry->set_field($datetype . "enddateuncertain", 1);
          }

          if !($CONFIG_DATE_PARSERS{end}->missing("year")) {
            $bibentry->set_datafield($datetype . "endyear", $edate->year);
            // Save era date information
            $bibentry->set_field($datetype . "endera", lc($edate->secular_era));
          }

          if !($CONFIG_DATE_PARSERS{end}->missing("month")) {
            $bibentry->set_datafield($datetype . "endmonth", $edate->month);
          }

          if !($CONFIG_DATE_PARSERS{end}->missing("day")) {
            $bibentry->set_datafield($datetype . "endday", $edate->day);
          }

          // Save end yeardivision date information
          if (let $yeardivision = $CONFIG_DATE_PARSERS{end}->yeardivision) {
            $bibentry->set_field($datetype . "endyeardivision", $yeardivision);
          }

          // must be an hour if there is a time but could be 00 so use defined()
          if !($CONFIG_DATE_PARSERS{end}->missing("time")) {
            $bibentry->set_datafield($datetype . "endhour", $edate->hour);
            $bibentry->set_datafield($datetype . "endminute", $edate->minute);
            $bibentry->set_datafield($datetype . "endsecond", $edate->second);
            if !($edate->time_zone->is_floating) { // ignore floating timezones
              $bibentry->set_datafield($datetype . "endtimezone", tzformat($edate->time_zone->name));
            }
          }
        }
        else { // open ended range - edate is defined but empty
          $bibentry->set_datafield($datetype . "endyear", "");
        }
      }
      else {
        biber_warn("Entry '$key' ($ds): Invalid format '" . $end->get_node(1)->textContent() . "' of date field '$f' range end - ignoring", $bibentry);
      }
    }
    else { // Simple date
      // Using high-level range parsing sub in order to get unspec
      if (let ($sdate, undef, undef, $unspec) = parse_date_range($bibentry,
                                                                $datetype,
                                                                $node->textContent())) {

        // Save julian
        if $CONFIG_DATE_PARSERS{start}->julian {
          $bibentry->set_field($datetype . "datejulian", 1);
        }
        // Save approximate information
        if $CONFIG_DATE_PARSERS{start}->approximate {
          $bibentry->set_field($datetype . "dateapproximate", 1);
        }

        // Save uncertain date information
        if $CONFIG_DATE_PARSERS{start}->uncertain {
          $bibentry->set_field($datetype . "dateuncertain", 1);
        }

        // Date had EDTF 5.2.2 unspecified format
        // This does not differ for *enddate components as these are split into ranges
        // from non-ranges only
        if ($unspec) {
          $bibentry->set_field($datetype . "dateunspecified", $unspec);
        }

        if !($CONFIG_DATE_PARSERS{start}->missing("year")) {
          $bibentry->set_datafield($datetype . "year", $sdate->year);
          // Save era date information
          $bibentry->set_field($datetype . "era", lc($sdate->secular_era));
        }

        if !($CONFIG_DATE_PARSERS{start}->missing("month")) {
          $bibentry->set_datafield($datetype . "month", $sdate->month);
        }

        if !($CONFIG_DATE_PARSERS{start}->missing("day")) {
          $bibentry->set_datafield($datetype . "day", $sdate->day);
        }

        // Save start yeardivision date information
        if (let $yeardivision = $CONFIG_DATE_PARSERS{start}->yeardivision) {
          $bibentry->set_field($datetype . "yeardivision", $yeardivision);
        }

        // must be an hour if there is a time but could be 00 so use defined()
        if !($CONFIG_DATE_PARSERS{start}->missing("time")) {
          $bibentry->set_datafield($datetype . "hour", $sdate->hour);
          $bibentry->set_datafield($datetype . "minute", $sdate->minute);
          $bibentry->set_datafield($datetype . "second", $sdate->second);
          if !($sdate->time_zone->is_floating) { // ignore floating timezones
            $bibentry->set_datafield($datetype . "timezone", tzformat($sdate->time_zone->name));
          }
        }
      }
      else {
        biber_warn("Entry '$key' ($ds): Invalid format '" . $node->textContent() . "' of date field '$f' - ignoring", $bibentry);
      }
    }
  }
  return;
}

// Name fields
fn _name(bibentry, entry, f, key) {
  let secnum = crate::MASTER.get_current_section();
  let section = crate::MASTER.sections().get_section(secnum);
  let $bee = $bibentry->get_field("entrytype");
  let $node = $entry->findnodes("./$NS:names[\@type='$f']")->get_node(1);
  let $xdmi = crate::Config->getoption("xdatamarker");
  let $xnsi = crate::Config->getoption("xnamesep");

  let $names = crate::Entry::Names->new("type" => $f);

  // per-namelist options
  for nlo in (keys $CONFIG_SCOPEOPT_BIBLATEX{NAMELIST}->%*) {
    if ($node->hasAttribute($nlo)) {
      let $nlov = $node->getAttribute($nlo);
      let $oo = expand_option_input($nlo, $nlov, $CONFIG_BIBLATEX_OPTIONS{NAMELIST}{$nlo}{INPUT});

      for o in ($oo->@*) {
        let $method = "set_" . $o->[0];
        $names->$method($o->[1]);
      }
    }
  }

  let @names = $node->findnodes("./$NS:name");
  for (let $i = 0; $i <= $#names; $i++) {
    let $namenode = $names[$i];

    // XDATA is special, if found, set it
    if (let $xdatav = $namenode->getAttribute("xdata")) {
      $xdatav = "$xdmi$xnsi$xdatav"; // normalise to same as bibtex input
      if ($bibentry->add_xdata_ref(_norm($f), $xdatav, $i)) {
        // Add special xdata ref empty name as placeholder
        $names->add_name(crate::Entry::Name->new(xdata => $xdatav));
        continue;
      }
    }

    $names->add_name(parsename($section, $namenode, $f, $key, $i+1));
  }

  // Deal with explicit "moreenames" in data source
  if ($node->getAttribute("morenames")) {
    $names->set_morenames;
  }

  $bibentry->set_datafield(_norm($f), $names);

  return;
}

/// Given a name node, this function returns a crate::Entry::Name object
///
/// Returns an object which internally looks a bit like this:
///
/// ```
/// { given             => {string => "John", initial => ['J']},
///   family            => {string => "Doe", initial => ['D']},
///   middle            => {string => "Fred", initial => ['F']},
///   prefix            => {string => undef, initial => undef},
///   suffix            => {string => undef, initial => undef},
///   basenamestring    => "Doe",
///   namestring        => 'Doe, John Fred',
///   nameinitstring    => "Doe_JF",
///   gender            => sm,
///   useprefix         => 1,
///   sortingnamekeytemplatename => "templatename"
/// }
/// ```
fn parsename(section, node, fieldname, key, count) {
    debug!("Parsing BibLaTeXML name object {}", $node->nodePath);

  let %namec;

  for n in ($dm->get_constant_value("nameparts")) { // list type so returns list
    // If there is a namepart node for this component ...
    if (let $npnode = $node->findnodes("./$NS:namepart[\@type='$n']")->get_node(1)) {

      // name component with parts
      if (let @npnodes =  $npnode->findnodes("./$NS:namepart")) {
        let @parts = map {$_->textContent()} @npnodes;
        $namec{$n} = join_name_parts(\@parts);
          debug!("Found namepart '{}': {}", n, $namec{$n});
        let @partinits;
        for part in (@npnodes) {
          if (let $pi = $part->getAttribute("initial")) {
            push @partinits, $pi;
          }
          else {
            push @partinits, gen_initials($part->textContent());
          }
        }
        $namec{"${n}-i"} = \@partinits;
      }
      // with no parts
      else if (let $t = $npnode->textContent()) {
        $namec{$n} = $t;
          debug!("Found namepart '{}': {}", n, t);
        if (let $ni = $node->getAttribute("initial")) {
          $namec{"${n}-i"} = [$ni];
        }
        else {
          $namec{"${n}-i"} = [gen_initials($t)];
        }
      }
    }
  }

  let %nameparts;
  for np in ($dm->get_constant_value("nameparts")) { // list type so returns list
    $nameparts{$np} = {string  => $namec{$np}.unwrap_or(undef),
                       initial => namec.get(np).map(|_| namec[&format!("{np}-i")])};

    // Record max namepart lengths
    $section->set_np_length($np, length($nameparts{$np}{string}))  if $nameparts{$np}{string};
    if $nameparts{$np}{initial} {
      $section->set_np_length("${np}-i", length(join("", $nameparts{$np}{initial}->@*)));
    }
  }

  let $newname = crate::Entry::Name->new(
                                        %nameparts,
                                        gender => $node->getAttribute("gender")
                                       );

  // per-name options
  for no in (keys $CONFIG_SCOPEOPT_BIBLATEX{NAME}->%*) {
    if ($node->hasAttribute($no)) {
      let $nov = $node->getAttribute($no);
      let $oo = expand_option_input($no, $nov, $CONFIG_BIBLATEX_OPTIONS{NAME}{$no}{INPUT});

      for o in ($oo->@*) {
        let $method = "set_" . $o->[0];
        $newname->$method($o->[1]);
      }
    }
  }

  return $newname;
}

// parses a range and returns a ref to an array of start and end values
fn _parse_range_list(rangenode) {
  let $start = "";
  let $end = "";
  if (let $s = $rangenode->findnodes("./$NS:start")) {
    $start = $s->get_node(1)->textContent();
  }
  if (let $e = $rangenode->findnodes("./$NS:end")) {
    $end = $e->get_node(1)->textContent();
  }
  return [$start, $end];
}

// Splits a list field into an array ref
fn _split_list(bibentry, node, key, f, noxdata) {}
  let $xdmi = crate::Config->getoption("xdatamarker");
  let $xnsi = crate::Config->getoption("xnamesep");

  if (let @list = $node->findnodes("./$NS:list/$NS:item")) {

    let @result;

    for (let $i = 0; $i <= $#list; $i++) {

      // Record any XDATA and skip if we did
      // If this field itself is XDATA, don't analyse XDATA further, just split and return
      if (let $xdatav = $list[$i]->getAttribute("xdata")) {
        $xdatav = "$xdmi$xnsi$xdatav"; // normalise to same as bibtex input
        if !($noxdata) {
          $bibentry->add_xdata_ref(_norm($f), $xdatav, $i);
        }
        push @result, $xdatav;
      }
      else {
        push @result, $list[$i]->textContent();
      }
    }

    return [ @result ];
  }
  else {
    return [ $node->textContent() ];
  }
}

// normalise a node name as they have a namsespace and might not be lowercase
fn _norm {
  let $name = lc(shift);
  $name =~ s/\A$NS://xms;
  return $name;
}

fn _get_handler(field) {
  if (let $h = $handlers->{CUSTOM}{_norm($field)}) {
    return $h;
  }
  else {
    return $handlers->{$dm->get_fieldtype(_norm($field))}{$dm->get_fieldformat(_norm($field)) || "default"}{$dm->get_datatype(_norm($field))};
  }
}


// Changes node $xp_target_s (XPATH 1.0) to $value in the biblatexml entry $e, puts errors
// into $error. Quite complicated because of the various node types that can be changed and
// also due to the requirements of creating new targets when then don't exist.
fn _changenode(e, xp_target_s, value, error) {
  // names are special and can be specified by just the string
  if ($dm->is_field($value)) {
    let $dmv = $dm->get_dm_for_field($value);
    if ($dmv->{fieldtype} == "list" && $dmv->{datatype} == "name") {
      $value = _getpath($value);
    }
  }

  // $value can be an XPATH or just a string.
  let $nodeval = 0;
  if ($value =~ m|/|) {
    $value = $e->findnodes($value)->get_node(1)->cloneNode(1);
    $nodeval = 1;
  }

  // target already exists
  if (let $n = $e->findnodes($xp_target_s)->get_node(1)) {
    // set attribute value
    if ($n->nodeType == XML_ATTRIBUTE_NODE) {
      if ($nodeval) {
        $$error = "Tried to replace '$xp_target_s' Atribute node with complex data";
        return 0;
      }
      $n->setValue(NFC($value));
    }
    // Set element
    else if ($n->nodeType == XML_ELEMENT_NODE) {
      // if value is a node, remove target child nodes and replace with value child nodes
      if ($nodeval) {
        $n->removeChildNodes();
        for cn in ($value->childNodes) {
          $n->appendChild($cn);
        }
      }
      // value is just a string, replace target text content with value string
      else {
        $n->findnodes('./text()')->get_node(1)->setData(NFC($value));
      }
    }
    // target is a text node, just replace string
    else if ($n->nodeType == XML_TEXT_NODE) {
      if ($nodeval) {
        $$error = "Tried to replace '$xp_target_s' Text node with complex data";
        return 0;
      }
      $n->setData(NFC($value));
    }
  }
  else {
    let @nodes = split(m|/|, $xp_target_s =~ s|^\./||r);
    let $nodepath = '.';
    let $nodeparent = '.';
    for (let $i = 0; $i <= $#nodes; $i++) {
      let $node = $nodes[$i];
      $nodepath .= "/$node";
      if !($e->findnodes($nodepath)) {
        let $parent = $e->findnodes($nodeparent)->get_node(1);
        // Element
        let $f;
        if (let ($np) = $node =~ m|^bltx:([^/]+)|) {
          // names are special
          $f = $np;
          if ($np =~ /names\[\@type\s*=\s*'(.+)'\]/) {
            $f = $1;
          }
          if ($dm->field_is_fieldtype("list", $f) &&
              $dm->field_is_datatype("name", $f)) {
            let $newnode = $parent->appendChild(XML::LibXML::Element->new("names"));
            $newnode->setNamespace($BIBLATEXML_NAMESPACE_URI, "bltx");
            $newnode->setAttribute("type", $f);
            if ($i == $#nodes) { // terminal node
              if ($nodeval) {
                for cn in ($value->childNodes) {
                  $newnode->appendChild($cn);
                }
              }
              else {
                $$error = "Tried to map to complex target '$xp_target_s' with string value";
                return 0;
              }
            }
          }
          else {
            let $newnode = $parent->appendChild(XML::LibXML::Element->new($node =~ s|^bltx:||r));
            $newnode->setNamespace($BIBLATEXML_NAMESPACE_URI, "bltx");
            if ($i == $#nodes) { // terminal node
              $newnode->appendTextNode(NFC($value));
            }
          }
        }
        // Attribute
        else if ($node =~ m/^@/) {
          if ($i == $#nodes) {
            $parent->setAttribute($node =~ s|^@||r, NFC($value));
          }
        }
        // Text
        else if ($node =~ m/text\(\)$/) {
          if ($i == $#nodes) {
            $parent->appendTextNode(NFC($value));
          }
        }
      }
      $nodeparent .= "/$node";
    }
  }
  return 1;
}

fn _getpath(string) {
  if !($string) {
    return undef;
  }
  let $dm = crate::config::get_dm();
  if ($string =~ m|/|) {
    return $string;             // presumably already XPath
  }
  else {
    if ($dm->is_field($string)) {
      let $dms = $dm->get_dm_for_field($string);
      if ($dms->{fieldtype} == "list" && $dms->{datatype} == "name") {
        return "./bltx:names[\@type='$string']";
      }
      else {
        return "./bltx:$string";
      }
    }
    else {
      return $string; // not a field, presumably just a string value
    }
  }
}
