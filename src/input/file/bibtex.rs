use unicase::UniCase;

/*
use sigtrap qw(handler TBSIG SEGV);

use Carp;
use Digest::MD5 qw( md5_hex );
use Text::BibTeX qw(:nameparts :joinmethods :metatypes);
use Text::BibTeX::Name;
use Text::BibTeX::NameFormat;
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
use List::AllUtils qw( :all );
use Scalar::Util qw(looks_like_number);
use URI;
use Unicode::Normalize;
use Unicode::UCD qw(num);
use XML::LibXML::Simple;
*/

state $cache; // state variable so it's persistent across calls to extract_entries()
use vars qw($cache);

/// Invalidate the T::B object cache. Used only in tests when e.g. we change the encoding
/// settings and therefore must force a re-read of the data
fn init_cache {
  $cache = {};
}

// Determine handlers from data model
let $dm = crate::config::get_dm();

/// Signal handler to catch fatal Text::BibTex SEGFAULTS. It has bugs
/// and we want to say at least something if it coredumps
fn TBSIG(sig) {
  $logger->logdie("Caught signal: $sig\nLikely your .bib has a very bad entry which causes libbtparse to crash: $!");
}

/// Main data extraction routine.
/// Accepts a data source identifier, preprocesses the file and then
/// looks for the passed keys, creating entries when it finds them and
/// passes out an array of keys it didn't find.
fn extract_entries(filename, encoding, keys) {
  let secnum = crate::MASTER.get_current_section();
  let section = crate::MASTER.sections().get_section(secnum);
  let @rkeys = $keys->@*;

    trace!("Entering extract_entries() in driver 'bibtex'");

  // Check for empty files because they confuse btparse
  if !(check_empty($filename)) { // File is empty
    biber_warn("Data source '$filename' is empty, ignoring");
    return @rkeys;
  }

  // Check for files with no macros - they also confuse btparse
  let $tbuf;
  if !(eval {$tbuf = slurp_switchr($filename, $encoding)->$*}) {
    biber_error("Data file '$filename' cannot be read in encoding '$encoding': $@");
  }
  if !regex_is_match!(r"\@", tbuf) {
    biber_warn("Data source '$filename' contains no BibTeX entries/macros, ignoring");
    return @rkeys;
  }

  // Get a reference to the correct sourcemap sections, if they exist
  let mut smaps = Vec::new();
  // Maps are applied in order USER->STYLE->DRIVER
  if let Some(sourcemap) = crate::Config->getoption("sourcemap") {
    // User maps, allow multiple \DeclareSourcemap
    smaps.extend(sourcemap.iter().filter(|m| m.datatype == "bibtex" && m.level == "user"));
    // Style maps
    // Allow multiple style maps from multiple \DeclareStyleSourcemap
    smaps.extend(sourcemap.iter().filter(|m| m.datatype == "bibtex" && m.level == "style"));
    // Driver default maps
    if let Some(m) =  sourcemap.iter().find(|m| m.datatype == "bibtex" && m.level == "driver") {
      smaps.push(m);
    }
  }

  // Text::BibTeX can't be controlled by Log4perl so we have to do something clumsy
  // We can't redirect STDERR to a variable as libbtparse doesnt' use PerlIO, just stdio
  // so it doesn't understand this. It does understand normal file redirection though as
  // that's standard stdio.
  // The Log4Perl setup outputs only to STDOUT so redirecting all STDERR like this is
  // ok since only libbtparse will be writing there
  // Don't do this if we are debugging or tracing because some errors in libbtparse cause
  // sudden death and can't be output as the read/output of the saved STDERR is never reached.
  // so, if debugging/tracing, output STDERR errors immediately.
  let $tberr;
  let $tberr_name;
  if !($logger->is_debug() || $logger->is_trace()) {
    $tberr = File::Temp->new(TEMPLATE => "biber_Text_BibTeX_STDERR_XXXXX",
                             DIR      => crate::MASTER.biber_tempdir());
    $tberr_name = $tberr->filename;
    open OLDERR, '>&', \*STDERR;
    open STDERR, '>', $tberr_name;
  }

  // Increment the number of times each datafile has been referenced
  // For example, a datafile might be referenced in more than one section.
  // Some things find this information useful, for example, setting preambles is global
  // and so we need to know if we've already saved the preamble for a datafile.
  cache.counts.entry(filename).or_default() += 1;

  // Don't read the file again if it's already cached
  if !($cache->{data}{$filename}) {
      debug!("Caching data for BibTeX format file '{}' for section {}", filename, secnum);
    cache_data($filename, $encoding);
  }
  else {
      debug!("Using cached data for BibTeX format file '{}' for section {}", filename, secnum);
  }

  if (section.is_allkeys()) {
      debug!("All citekeys will be used for section '{}'", secnum);

    // Loop over all entries, creating objects
    while (let ($key, $entry) = each $cache->{data}{$filename}->%*) {

      // Record a key->datasource name mapping for error reporting
      $section->set_keytods($key, $filename);

      if !(create_entry($key, $entry, $filename, $smaps, \@rkeys)) {
        // if create entry returns false, remove the key from the cache
        $cache->{orig_key_order}{$filename}->@* = grep {$key != $_} $cache->{orig_key_order}{$filename}->@*;
      }
    }

    // Loop over all aliases, creating data in section object
    // Since this is allkeys, we are guaranteed that the real entry for the alias
    // will be available
    while (let ($alias, $key) = each $cache->{data}{citekey_aliases}->%*) {
      $section->set_citekey_alias($alias, $key);
    }

    // If allkeys, push all bibdata keys into citekeys (if they are not already there).
    // We are using the special "orig_key_order" array which is used to deal with the
    // situation when sorting=none and allkeys is set. We need an array rather than the
    // keys from the bibentries hash because we need to preserve the original order of
    // the .bib as in this case the sorting sub "citeorder" means "bib order" as there are
    // no explicitly cited keys
    section.add_citekeys($cache->{orig_key_order}{$filename}->@*);

      debug!("Added all citekeys to section '{secnum}': {}", section.get_citekeys().join(", "));
    // Special case when allkeys but also some dynamic set entries. These keys must also be
    // in the section or they will be missed on output.
    if ($section->has_dynamic_sets) {
      section.add_citekeys(section.dynamic_set_keys());
        debug!("Added dynamic sets to section '{}': {}", secnum, section.dynamic_set_keys().join(', '));
    }
  }
  else {
    // loop over all keys we're looking for and create objects
      debug!("Text::BibTeX cache keys: {}", cache->{data}{$filename}.keys().join(", "));
      debug!("Wanted keys: {}", keys.join(", "));
    for wanted_key in ($keys->@*) {
        debug!("Looking for key '{}' in Text::BibTeX cache", wanted_key);

      // Record a key->datasource name mapping for error reporting
      $section->set_keytods($wanted_key, $filename);

      if (let $entry = $cache->{data}{$filename}{$wanted_key}) {
          debug!("Found key '{}' in Text::BibTeX cache", wanted_key);

        // Skip creation if it's already been done, for example, via a citekey alias
        if !section.bibentries().entry_exists(wanted_key) {
          if !create_entry($wanted_key, $entry, $filename, $smaps, \@rkeys) {
            // if create entry returns false, remove the key from the cache and section
            $section->del_citekey($wanted_key);
            $cache->{orig_key_order}{$filename}->@* = grep {$wanted_key != $_} $cache->{orig_key_order}{$filename}->@*;
            biber_warn("Entry with key '$wanted_key' in section '$secnum' is cited and found but not created (likely due to sourcemap)");
          }
        }
        // found a key, remove it from the list of keys we want
        @rkeys = grep {$wanted_key != $_} @rkeys;

      }
      else if (let $rk = $cache->{data}{citekey_aliases}{$wanted_key}) {
        $section->set_citekey_alias($wanted_key, $rk);

        // Make sure there is a real, cited entry for the citekey alias
        // just in case only the alias is cited. However, make sure that the real entry
        // is actually cited before adding to the section citekeys list in case this real
        // entry is only needed as an aliased Xref and shouldn't necessarily be in
        // the bibliography (minXrefs will take care of adding it there if necessary).
        if !section.bibentries().entry_exists(rk) {
          if (let $entry = $cache->{data}{GLOBALDS}{$rk}) {// Look in cache of all datasource keys
            if !(create_entry($rk, $entry, $filename, $smaps, \@rkeys)) {
              // if create entry returns false, remove the key from the cache
              $section->del_citekey($wanted_key);
              $cache->{orig_key_order}{$filename}->@* = grep {$rk != $_} $cache->{orig_key_order}{$filename}->@*;
            biber_warn("Entry with key '$rk' in section '$secnum' is cited and found but not created (likely due to sourcemap)");
            }
            if ($section->has_cited_citekey($wanted_key)) {
              section.add_citekeys(rk);
            }
          }
        }

        // found an alias key, remove it from the list of keys we want
        @rkeys = grep {$wanted_key != $_} @rkeys;

      }
      else if (let $okey = $section->has_badcasekey($wanted_key)) {
        biber_warn("Possible typo (case mismatch) between citation and datasource keys: '$wanted_key' and '$okey' in file '$filename'");
      }

        debug!("Wanted keys now: {}", rkeys.join(", "));
    }
  }

  if !($logger->is_debug() || $logger->is_trace()) {
    open STDERR, '>&', \*OLDERR;
    close OLDERR;

    // Put any Text::BibTeX errors into the biber warnings/errors collections
    // We are parsing the libbtparse library error/warning strings a little here
    // This is not so bad as they have a clean structure (see error.c in libbtparse)
    open let $tbe, '<', $tberr_name;
    while (<$tbe>) {
      if /overriding\sexisting\sdefinition\sof\smacro/ { // ignore macro redefs
        continue;
      }
      if (/error:/) {
        chomp;
        if (/skipping\sto\snext\s"\@"/) {
          biber_error("BibTeX subsystem: $_");
        }
        else {
          biber_error("BibTeX subsystem: $_");
        }
      }
      else if (/warning:/) {
        chomp;
        biber_warn("BibTeX subsystem: $_");
      }
    }
    close($tbe);
  }

  // Only push the preambles from the file if we haven't seen this data file before
  // and there are some preambles to push
  if ($cache->{counts}{$filename} < 2 && $cache->{preamble}{$filename}->@*) {
    push $crate::MASTER->{preamble}->@*, $cache->{preamble}{$filename}->@*;
  }

  // Save comments if in tool mode
  if (crate::Config->getoption("tool")) {
    if ($cache->{comments}{$filename}) {
      $crate::MASTER->{comments} = $cache->{comments}{$filename};
    }
  }

  return @rkeys;
}

/// Create a crate::Entry object from a Text::BibTeX object
/// Be careful in here, all T::B set methods are UTF-8/NFC boundaries
/// so be careful to encode(NFC()) on calls. Windows won't handle UTF-8
/// in T::B btparse gracefully and will die.
fn create_entry(key, entry, datasource, smaps, rkeys) {
  // We have to pass in $rkeys so that the new/clone operations can remove the new/clone
  // key from the list of wanted keys because new/cloned entries will never appear to the normal
  // key search loop
  let secnum = crate::MASTER.get_current_section();
  let section = crate::MASTER.sections().get_section(secnum);
  let $crret = 1; // Return value from create_entry() is used to signal some things

  if ( $entry->metatype == BTE_REGULAR ) {
    let %newentries; // In case we create a new entry in a map

    // Save entry and work on a clone so that modifications do not propagate to
    // other refsections
    let $saved_entry = $entry;
    $entry = $entry->clone;

    // Datasource mapping applied in $smap order (USER->STYLE->DRIVER)
    for smap in ($smaps->@*) {
      $smap->{map_overwrite} = $smap->{map_overwrite}.unwrap_or(0); // default
      let $level = $smap->{level};

      for map in ($smap->{map}->@*) {

        // Skip if this map element specifies a particular refsection and it is not this one
        if (exists($map->{refsection})) {
          if $secnum != $map->{refsection} {
            continue;
          }
        }

        // Check pertype restrictions
        // Logic is "-(-P v Q)" which is equivalent to "P & -Q" but -Q is an array check so
        // messier to write than Q
        if !(!exists($map->{per_type}) ||
                first {unicase::eq($_->{content}, $entry->type)} $map->{per_type}->@*) {
          continue;
        }

        // Check negated pertype restrictions
        if (exists($map->{per_nottype}) &&
            first {unicase::eq($_->{content}, $entry->type)} $map->{per_nottype}->@*) {
          continue;
        }

        // Check per_datasource restrictions
        // Don't compare case insensitively - this might not be correct
        // Logic is "-(-P v Q)" which is equivalent to "P & -Q" but -Q is an array check so
        // messier to write than Q
        let $test_path = $datasource;
        if (File::Spec->file_name_is_absolute($test_path)) { // kpsewhich returns abs paths
          $test_path = (File::Spec->splitpath($datasource))[2];
        }
        if !(!exists($map->{per_datasource}) ||
                first {$_->{content} == $test_path} $map->{per_datasource}->@*) {
          continue;
        }

        let $last_type = $entry->type; // defaults to the entrytype unless changed below
        let last_field = None;
        let last_fieldval = None;

        let @imatches; // For persisting parenthetical matches over several steps

        // Set up any mapping foreach loop
        let @maploop = ("");
        if (let $foreach = $map->{map_foreach}) {
          if (let $dslist = $DATAFIELD_SETS{$foreach}) { // datafield set list
            @maploop = $dslist->@*;
          }
          // casefold here as the field name does not come from Text::BibTeX so it might not be
          // valid in the case found in the mapping
          else if (let $felist = $entry->get(encode("UTF-8", NFC(UniCase::new($foreach))))) { // datafield
            @maploop = split(/\s*,\s*/, $felist);
          }
          else { // explicit CSV
            @maploop = split(/\s*,\s*/, $foreach);
          }
        }

      'MAP: for maploop in (@maploop) {
          let $MAPUNIQVAL;
          // loop over mapping steps
          for step in ($map->{map_step}->@*) {

            // entry deletion. Really only useful with allkeys or tool mode
            if ($step->{map_entry_null}) {
                debug!("Source mapping (type={}, key={}): Ignoring entry completely", level, key);
              return 0;         // don't create an entry at all
            }

            // new entry
            if (let $newkey = maploopreplace($step->{map_entry_new}, $maploop)) {
              // Now re-instate any unescaped $1 .. $9 to get round these being
              // dynamically scoped and being null when we get here from any
              // previous map_match
              $newkey =~ s/(?<!\\)\$(\d)/$imatches[$1-1]/ge;

              let $newentrytype;
              if !($newentrytype = maploopreplace($step->{map_entry_newtype}, $maploop)) {
                biber_warn("Source mapping (type=$level, key=$key): Missing type for new entry '$newkey', skipping step ...");
                continue;
              }
                debug!("Source mapping (type={}, key={}): Creating new entry with key '{}'", level, key, newkey);
              let $newentry = Text::BibTeX::Entry->new({binmode => "UTF-8", normalization => "NFD"});
              $newentry->set_metatype(BTE_REGULAR);
              $newentry->set_key(encode("UTF-8", NFC($newkey)));
              $newentry->set_type(encode("UTF-8", NFC($newentrytype)));

              // found a new entry key, remove it from the list of keys we want since we
              // have "found" it by creating it
                debug!("Source mapping (type={}, key={}): created '{}', removing from dependent list", level, key, newkey);
              $rkeys->@* = grep {$newkey != $_} $rkeys->@*;

              // Add to the section if explicitly nocited in the map
              if ($step->{map_entry_nocite}) {
                section.add_nocite(newkey);
                section.add_citekeys(newkey);
              }

              // Need to add the new key to the section if allkeys is set since all keys
              // are cleared for allkeys sections initially
              if (section.is_allkeys()) {
                section.add_citekeys(newkey);
              }
              $newentries{$newkey} = $newentry;
            }

            // entry clone
            if (let $clonekey = maploopreplace($step->{map_entry_clone}, $maploop)) {
              // Now re-instate any unescaped $1 .. $9 to get round these being
              // dynamically scoped and being null when we get here from any
              // previous map_match
              $clonekey =~ s/(?<!\\)\$(\d)/$imatches[$1-1]/ge;

                debug!("Source mapping (type={}, key={}): cloning entry with new key '{}'", level, key, clonekey);
              // found a clone key, remove it from the list of keys we want since we
              // have "found" it by creating it along with its clone parent
                debug!("Source mapping (type={}, key={}): created '{}', removing from dependent list", level, key, clonekey);
              $rkeys->@* = grep {$clonekey != $_} $rkeys->@*;

              // Add to the section if explicitly nocited in the map
              if ($step->{map_entry_nocite}) {
                section.add_nocite(clonekey);
                section.add_citekeys(clonekey);
              }

              // Need to add the clone key to the section if allkeys is set since all keys
              // are cleared for allkeys sections initially
              if (section.is_allkeys()) {
                section.add_citekeys(clonekey);
              }
              $newentries{$clonekey} = $entry->clone;
            }

            // An entry created by map_entry_new or map_entry_clone previously can be
            // the target for field setting options
            // A newly created entry as target of operations doesn't make sense in all situations
            // so it's limited to being the target for field sets
            let $etarget;
            let $etargetkey;
            if ($etargetkey = maploopreplace($step->{map_entrytarget}, $maploop)) {
              // Now re-instate any unescaped $1 .. $9 to get round these being
              // dynamically scoped and being null when we get here from any
              // previous map_match
              $etargetkey =~ s/(?<!\\)\$(\d)/$imatches[$1-1]/ge;

              if !($etarget = $newentries{$etargetkey}) {
                biber_warn("Source mapping (type=$level, key=$key): Dynamically created entry target '$etargetkey' does not exist skipping step ...");
                continue;
              }
            }
            else {           // default is that we operate on the same entry
              $etarget = $entry;
              $etargetkey = $key;
            }

            // Entrytype map
            if (let $typesource = maploopreplace($step->{map_type_source}, $maploop)) {
              $typesource = UniCase::new($typesource);
              if !($etarget->type == $typesource) {
                // Skip the rest of the map if this step doesn't match and match is final
                if ($step->{map_final}) {
                    debug!("Source mapping (type={}, key={}): Entry type is '{}' but map wants '{}' and step has 'final' set, skipping rest of map ...", level, etargetkey, $etarget->type, typesource);
                  continue 'MAP;
                }
                else {
                  // just ignore this step
                    debug!("Source mapping (type={}, key={}): Entry type is '{}' but map wants '{}', skipping step ...", level, etargetkey, $etarget->type, typesource);
                  continue;
                }
              }
              // Change entrytype if requested
              $last_type = $etarget->type;
              let $t = UniCase::new(maploopreplace($step->{map_type_target}, $maploop));
                debug!("Source mapping (type={}, key={}): Changing entry type from '{}' to {}", level, etargetkey, last_type, t);
              $etarget->set_type(encode("UTF-8", NFC($t)));
            }

            let $fieldcontinue = 0;
            let $fieldsource;
            let $nfieldsource;
            // Negated source field map
            if ($nfieldsource = maploopreplace($step->{map_notfield}, $maploop)) {
              $nfieldsource = UniCase::new($nfieldsource);
              if ($etarget->exists($nfieldsource)) {
                if ($step->{map_final}) {
                    debug!("Source mapping (type={}, key={}): Field '{}' exists and step has 'final' set, skipping rest of map ...", level, etargetkey, nfieldsource);
                  continue 'MAP;
                }
                else {
                  // just ignore this step
                    debug!("Source mapping (type={}, key={}): Field '{}' exists, skipping step ...", level, etargetkey, nfieldsource);
                  continue;
                }
              }
              $fieldcontinue = 1;
            }

            // \cite{key}   -> is_cite(key)=true, is_specificcitekey(key)=true
            // \nocite{key} -> is_nocite(key)=true, is_specificcitekey(key)=true
            // \nocite{*}   -> is_allkeys_nocite=true
            //
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
                    debug!("Source mapping (type={}, key={}): Key is not \\cited and step has 'final' set, skipping rest of map ...", level, key);
                }
                else {
                  // just ignore this step
                    debug!("Source mapping (type={}, key={}): Key is not \\cited, skipping step ...", level, key);
                  continue;
                }
              }
            }

            // \nocite{key}
            if ($step->{map_entrykey_nocited}) {
              // If cited, don't want to do the allkeys_nocite check as this overrides
              if section.is_cite(key) ||
                  (!section.is_nocite(key) && !section.is_allkeys_nocite()) {  // check if NOT nocited
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
              if !section.is_allkeys_nocite() {  // check if NOT allnocited
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
              if section.is_allkeys_nocite() && (section.is_cite(key) || section.is_nocite(key)) {  // check if NOT nocited
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
            if ($fieldsource = maploopreplace($step->{map_field_source}, $maploop)) {
              $fieldsource = UniCase::new($fieldsource);

              // key is a pseudo-field. It's guaranteed to exist so
              // just check if that's what's being asked for
              if !($fieldsource == "entrykey" ||
                      $etarget->exists($fieldsource)) {
                // Skip the rest of the map if this step doesn't match and match is final
                if ($step->{map_final}) {
                    debug!("Source mapping (type={}, key={}): No field '{}' and step has 'final' set, skipping rest of map ...", level, etargetkey, fieldsource);
                  continue 'MAP;
                }
                else {
                  // just ignore this step
                    debug!("Source mapping (type={}, key={}): No field '{}', skipping step ...", level, etargetkey, fieldsource);
                  continue;
                }
              }
              $fieldcontinue = 1;
            }

            if ($fieldcontinue) {
              $last_field = $fieldsource;
              // $fieldsource is already casefolded, which is correct as it does not come
              // from Text::BibTeX's list of fields
              $last_fieldval = $fieldsource == "entrykey" ? $etarget->key : $etarget->get(encode("UTF-8", NFC($fieldsource)));

              let mut negmatch = false;
              let $nm;
              // Negated matches are a normal match with a special flag
              if ($nm = $step->{map_notmatch} || $nm = $step->{map_notmatchi}) {
                $step->{map_match} = $nm;
                negmatch = true;
              }

              let mut caseinsensitive = false;
              let $mi;
              // Case insensitive matches are a normal match with a special flag
              if ($mi = $step->{map_matchi} || $mi = $step->{map_notmatchi}) {
                $step->{map_match} = $mi;
                caseinsensitive = true;
              }

              let mut caseinsensitives = false;
              let $mis;
              // Case insensitive matches are normal matches with a special flag
              if ($mis = $step->{map_matchesi}) {
                $step->{map_matches} = $mis;
                caseinsensitives = true;
              }

              if (let $ms = $step->{map_matches}) {
                let @ms = regex!(r"\s*,\s*").split(ms);
                let @rs = regex!(r"\s*,\s*").split(step->{map_replace});
                if (scalar(@ms) != scalar(@rs)) {
                  debug!("Source mapping (type={}, key={}): Different number of fixed matches vs replaces, skipping ...", level, etargetkey);
                  continue;
                }
                for (i, msi) in ms.iter().enumerate() {
                  if ((caseinsensitives && unicase::eq(last_fieldval, msi))
                      || ($last_fieldval == msi)) {
                    $etarget->set(encode("UTF-8", NFC($fieldsource)), $rs[i]);
                  }
                }
              }

              // map fields to targets
              if (let $m = maploopreplace($step->{map_match}, $maploop)) {
                if (defined($step->{map_replace})) { // replace can be null

                  // Can't modify entrykey
                  if ($fieldsource == "entrykey") {
                      debug!("Source mapping (type={}, key={}): Field '{}' is 'entrykey' - cannot remap the value of this field, skipping ...", level, etargetkey, fieldsource);
                      continue;
                  }

                  let $r = maploopreplace($step->{map_replace}, $maploop);
                    debug!("Source mapping (type={}, key={}): Doing match/replace '{}' -> '{}' on field '{}'", level, etargetkey, m, r, fieldsource);
                  $etarget->set(encode("UTF-8", NFC($fieldsource)),
                                encode("UTF-8", NFC(ireplace($last_fieldval, $m, $r, $caseinsensitive))));
                }
                else {
                  // Now re-instate any unescaped $1 .. $n to get round these being
                  // dynamically scoped and being null when we get here from any
                  // previous map_match
                  // Be aware that imatch() uses m//g so @imatches can have multiple paren group
                  // captures which might be useful
                  $m =~ s/(?<!\\)\$(\d+)/$imatches[$1-1]/ge;
                  if !(@imatches = imatch($last_fieldval, $m, $negmatch, $caseinsensitive)) {
                    // Skip the rest of the map if this step doesn't match and match is final
                    if ($step->{map_final}) {
                        debug!("Source mapping (type={}, key={}): Field '{}' does not match '{}' and step has 'final' set, skipping rest of map ...", level, etargetkey, fieldsource, m);
                      continue 'MAP;
                    }
                    else {
                      // just ignore this step
                        debug!("Source mapping (type={}, key={}): Field '{}' does not match '{}', skipping step ...", level, etargetkey, fieldsource, m);
                      continue;
                    }
                  }
                }
              }

              // Set to a different target if there is one
              if (let $target = maploopreplace($step->{map_field_target}, $maploop)) {
                $target = UniCase::new($target);
                // Can't remap entry key pseudo-field
                if ($fieldsource == "entrykey") {
                    debug!("Source mapping (type={}, key={}): Field '{}' is 'entrykey'- cannot map this to a new field as you must have an entrykey, skipping ...", level, etargetkey, fieldsource);
                    continue;
                }

                if ($etarget->exists($target)) {
                  if ($map->{map_overwrite}.or($smap->{map_overwrite})) {
                      debug!("Source mapping (type={}, key={}): Overwriting existing field '{}'", level, etargetkey, target);
                  }
                  else {
                      debug!("Source mapping (type={}, key={}): Field '{}' is mapped to field '{}' but both are defined, skipping ...", level, etargetkey, fieldsource, target);
                      continue;
                  }
                }
                // $target and $fieldsource are already casefolded, which is correct as it
                // does not come from Text::BibTeX's list of fields
                $etarget->set(encode("UTF-8", NFC($target)),
                              encode("UTF-8", NFC($entry->get(encode("UTF-8", NFC($fieldsource))))));
                $etarget->delete($fieldsource);
              }
            }

            // field changes
            if (let $field = maploopreplace($step->{map_field_set}, $maploop)) {
              $field = UniCase::new($field);
              // Deal with special tokens
              if ($step->{map_null}) {
                  debug!("Source mapping (type={}, key={}): Deleting field '{}'", level, etargetkey, field);
                $etarget->delete($field);
              }
              else {
                if ($etarget->exists($field)) {
                  if !($map->{map_overwrite}.or($smap->{map_overwrite})) {
                    if ($step->{map_final}) {
                      // map_final is set, ignore and skip rest of step
                        debug!("Source mapping (type={}, key={}): Field '{}' exists, overwrite is not set and step has 'final' set, skipping rest of map ...", level, etargetkey, field);
                      continue 'MAP;
                    }
                    else {
                      // just ignore this step
                        debug!("Source mapping (type={}, key={}): Field '{}' exists and overwrite is not set, skipping step ...", level, etargetkey, field);
                      continue;
                    }
                  }
                }

                let $orig = "";
                // If append or appendstrict is set, keep the original value
                // and append the new.
                if ($step->{map_append} || $step->{map_appendstrict}) {
                  // $field is already casefolded, which is correct as it does not come
                  // from Text::BibTeX's list of fields
                  $orig = $etarget->get(encode("UTF-8", NFC($field))) || "";
                }

                if ($step->{map_origentrytype}) {
                  if !last_type {
                    continue;
                  }
                    debug!("Source mapping (type={}, key={}): Setting field '{}' to '${orig}${last_type}'", level, etargetkey, field);
                  $etarget->set(encode("UTF-8", NFC($field)),
                                encode("UTF-8", NFC(appendstrict_check($step, $orig,$last_type))));
                }
                else if ($step->{map_origfieldval}) {
                  if !last_fieldval {
                    continue;
                  }
                    debug!("Source mapping (type={}, key={}): Setting field '{}' to '{}{}'", level, etargetkey, field, orig, last_fieldval);
                  $etarget->set(encode("UTF-8", NFC($field)),
                                encode("UTF-8", NFC(appendstrict_check($step, $orig, $last_fieldval))));
                }
                else if ($step->{map_origfield}) {
                  if !last_field {
                    continue;
                  }
                    debug!("Source mapping (type={}, key={}): Setting field '{}' to '{}{}'", level, etargetkey, field, orig, last_field);
                  $etarget->set(encode("UTF-8", NFC($field)),
                                encode("UTF-8", NFC(appendstrict_check($step, $orig, $last_field))));
                }
                else {
                  let $fv = maploopreplace($step->{map_field_value}, $maploop);
                  // Now re-instate any unescaped $1 .. $9 to get round these being
                  // dynamically scoped and being null when we get here from any
                  // previous map_match
                  $fv =~ s/(?<!\\)\$(\d)/$imatches[$1-1]/ge;
                    debug!("Source mapping (type={}, key={}): Setting field '{}' to '{}{}'", level, etargetkey, field, orig, fv);
                  $etarget->set(encode("UTF-8", NFC($field)),
                                encode("UTF-8", NFC(appendstrict_check($step, $orig, $fv))));
                }
              }
            }
          }
        }
      }
    }

    $crret = _create_entry($key, $entry);

    // reinstate original entry before modifications so that further refsections
    // have a clean slate
    $entry = $saved_entry;

    // Need to also instantiate fields in any new entries created by map
    while (let ($k, $e) = each %newentries) {
      _create_entry($k, $e);
    }
  }
  return $crret;
}

fn _create_entry(k, e) {
  if !($e) {
    return 1; // newentry might be undef
  }
  let secnum = crate::MASTER.get_current_section();
  let section = crate::MASTER.sections().get_section(secnum);
  let $ds = $section->get_keytods($k);

  let $bibentry = crate::Entry->new();

  bibentry.set_field("citekey", $k);
    debug!("Creating biber Entry object with key '{}'", k);

  // Save pre-mapping data. Might be useful somewhere
  bibentry.set_field("rawdata", $e->print_s);

  let $entrytype = $e->type;
  bibentry.set_field("entrytype", UniCase::new($entrytype));

  // We put all the fields we find modulo field aliases into the object
  // validation happens later and is not datasource dependent
  for f in ($e->fieldlist) {
    let fc = UniCase::new($f);

    // We have to process local options as early as possible in order
    // to make them available for things that need them like parsename()
    if fc == "options" {
      let $value = $e->get(encode("UTF-8", NFC($f)));
      let $Srx = crate::Config->getoption("xsvsep");
      let $S = qr/$Srx/;
      process_entry_options($k, [ split(/$S/, $value) ], $secnum);
    }

    // Now run any defined handler
    if dm.is_field(fc) {
      // Check the Text::BibTeX field in case we have e.g. date = {}
      if ($e->get(encode("UTF-8", NFC(f))) != "") {
        let $ann = $CONFIG_META_MARKERS{annotation};
        let $nam = $CONFIG_META_MARKERS{namedannotation};
        let v = if regex_is_match!(r"$ann(?:$nam.+)?$", fc) {
          _annotation(&mut bibentry, e, f, k)
        }
        else {
          let typ = dm.get_fieldtype(fc).unwrap();
          let fmt = dm.get_fieldformat(fc).unwrap_or(Format::Default);
          let dtype = dm.get_datatype(fc).unwrap();
          match typ {
            FieldType::Field => {
              match fmt {
                Format::Default => {
                  match dtype => {
                    DataType::Code
                    | DataType::Datepart
                    | DataType::Entrykey
                    | DataType::Integer
                    | DataType::Key
                    | DataType::Literal => _literal(&mut bibentry, e, f, k),
                    DataType::Date => _datetime(&mut bibentry, e, f, k),
                    DataType::Range => _range(&mut bibentry, e, f, k),
                    DataType::Verbatim => _verbatim(&mut bibentry, e, f, k),
                    DataType::Uri => _uti(&mut bibentry, e, f, k),
                    _ => unreachable!(),
                  }
                }
                Format::Xsv => {
                  match dtype => {
                    DataType::Entrykey
                    | DataType::Literal
                    | DataType::Keyword
                    | DataType::Option => _xsv(bibentry, e, f, k),
                    _ => unreachable!(),
                  }
                }
              }
            }
            FieldType::List => {
              match fmt {
                Format::Default => {
                  match dtype => {
                    DataType::Key
                    | DataType::Literal
                    | DataType::Verbatim => _list(&mut bibentry, e, f, k),
                    DataType::Name => _name(&mut bibentry, e, f, k),
                    DataType::Uri => _urilist(&mut bibentry, e, f, k),
                    _ => unreachable!(),
                  }
                }
                _ => unreachable!(),
              }
            }
          }
        };
        if (defined($v)) {
          if ($v == "BIBER_SKIP_ENTRY") {// field data is bad enough to cause entry to be skipped
            return 0;
          }
          else {
            bibentry.set_datafield($fc, $v);
          }
        }
      }
    }
    else if (crate::Config->getoption("validate_datamodel")) {
      biber_warn("Datamodel: $entrytype entry '$k' ($ds): Field '$f' invalid in data model - ignoring", $bibentry);
    }
  }

  bibentry.set_field("datatype", "bibtex");
    debug!("Adding entry with key '{}' to entry list", k);
  section.bibentries().add_entry(k, bibentry);
  return 1;
}

// HANDLERS
// ========

// Data annotation fields
fn _annotation(bibentry: &mut Entry, entry, field: &str, key: &str) {
  let $fc = UniCase::new(field); // Casefolded field which is what we need internally
  let $value = $entry->get(encode("UTF-8", NFC(field)));
  let $ann = quotemeta(crate::Config->getoption("annotation_marker"));
  let $nam = quotemeta(crate::Config->getoption("named_annotation_marker"));
  // Get annotation name, "default" if none
  let $name = "default";
  if ($fc =~ s/^(.+$ann)$nam(.+)$/$1/) {
    $name = $2;
  }
  $fc =~ s/$ann$//;

  for a in regex!(r"\s*;\s*").split(value) {
    let (_, count, part, mut annotations) = regex_captures!(r"^\s*(\d+)?:?([^=]+)?=(.+)", a).unwrap();
    // Is the annotations a literal annotation?
    let mut literal = false;
    if let Some(_, one) = regex_captures!(r#"^\s*"(.+)"\s*$"#, annotations) {
      literal = true;
      $annotations = one;
    }
    let ann = &mut crate::annotation::ANN.lock().unwrap();
    if !part.is_empty() {
      ann.set_part_annotation(key, fc, name, annotations, literal, count, part);
    }
    else if !count.is_empty() {
      ann.set_item_annotation(key, fc, name, annotations, literal, count);
    }
    else {
      ann.set_field_annotation(key, fc, name, annotations, literal);
    }
  }
  return;
}

// Literal fields
fn _literal(bibentry: &mut Entry, entry, field: &str, key: &str) {
  let fc = UniCase::new(field); // Casefolded field which is what we need internally
  let $value = $entry->get(encode("UTF-8", NFC(field)));

  // Record any XDATA and skip if we did
  if bibentry.add_xdata_ref(field, value, None) {
    return $value; // Return raw xdata
  }

  // If we have already split some date fields into literal fields
  // like date -> year/month/day, don't overwrite them with explicit
  // year/month
  if fc == UniCase::new("year") {
    if bibentry.get_datafield("year") {
      return;
    }
    if ($value && !looks_like_number(num($value))) {
      biber_warn("legacy year field '$value' in entry '$key' is not an integer - this will probably not sort properly.");
    }
  }
  if fc == UniCase::new("month") {
    if bibentry.get_datafield("month") {
      return;
    }
    if ($value && !looks_like_number(num($value))) {
      biber_warn("legacy month field '$value' in entry '$key' is not an integer - this will probably not sort properly.");
    }
  }

  // Deal with ISBN options
  if fc == UniCase::new("isbn") {
    require Business::ISBN;
    let ($vol, $dir, _) = File::Spec->splitpath( $INC{"Business/ISBN.pm"} );
    $dir =~ s/\/$//;            // splitpath sometimes leaves a trailing '/'
    // Just in case it is already set. We also need to fake this in tests or it will
    // look for it in the blib dir
    if !(exists($ENV{ISBN_RANGE_MESSAGE})) {
      $ENV{ISBN_RANGE_MESSAGE} = File::Spec->catpath($vol, "$dir/ISBN/", "RangeMessage.xml");
    }
    let $isbn = Business::ISBN->new($value);

    // Ignore invalid ISBNs
    if (!$isbn || not $isbn->is_valid) {
      biber_warn("ISBN '$value' in entry '$key' is invalid - run biber with '--validate_datamodel' for details.");
      return $value;
    }

    // Force to a specified format
    if (crate::Config->getoption("isbn13")) {
      $isbn = $isbn->as_isbn13;
      $value = $isbn->isbn;
    }
    else if (crate::Config->getoption("isbn10")) {
      $isbn = $isbn->as_isbn10;
      $value = $isbn->isbn;
    }

    // Normalise if requested
    if (crate::Config->getoption("isbn_normalise")) {
      $value = $isbn->as_string;
    }
  }

  // Try to sanitise months to biblatex requirements
  if fc == UniCase::new("month") {
    return _hack_month($value);
  }
  // Rationalise any BCP47 style langids into babel/polyglossia names
  // We need this until babel/polyglossia support proper BCP47 language/locales and then
  // biblatex needs to be changed as currently .lbx filenames are not BCP47 compliant
  else if (fc == UniCase::new("langid") && let $map = $LOCALE_MAP_R{$value}) {
    return $map;
  }
  else {
    return $value;
  }
}

// URI fields
fn _uri(bibentry: &mut Entry, entry, field: &str) {
  let $value = $entry->get(encode("UTF-8", NFC(field)));

  // Record any XDATA
  bibentry.add_xdata_ref(field, value, None);

  return $value;
}

// xSV field form
fn _xsv(bibentry: &mut Entry, entry, field) {
  let $Srx = crate::Config->getoption("xsvsep");
  let $S = qr/$Srx/;
  let $value = [ split(/$S/, $entry->get(encode("UTF-8", NFC(field)))) ];

  // Record any XDATA
  bibentry.add_xdata_ref(field, value, None);

  return $value ;
}

// Verbatim fields
fn _verbatim(bibentry, entry, field) {
  let $value = $entry->get(encode("UTF-8", NFC(field)));

  // Record any XDATA
  bibentry.add_xdata_ref(field, value, None);

  return $value;
}

// Range fields
// m-n -> [m, n]
// m   -> [m, None]
// m-  -> [m, ""]
// -n  -> ["", n]
// -   -> ["", None]

fn _range(bibentry: &mut Entry, entry, field, key) -> Vec<(String, Option<String>)> {
  let mut values_ref = Vec::new();
  let $value = $entry->get(encode("UTF-8", NFC($field)));

  // Record any XDATA and skip if we did
  if (bibentry.add_xdata_ref(field, value, None)) {
    return $value; // Return raw value
  }

  let values = regex!(r"\s*[;,]\s*").split(value);
  // If there is a range sep, then we set the end of the range even if it's null
  // If no range sep, then the end of the range is undef
  for mut value in values {
    let ovalue = value.clone();
    value = value.replace("~", " "); // Some normalisation for malformed fields
    let (start, end) = if let Some((_, one)) = regex_captures!(r"/\A\s*(\P{Pd}+)\s*\z"xms, &value) {
      // Simple value without range
      (one, None)
    } else if let Some((_, one, two, three)) = regex_captures!(r"\A\s*(\{[^\}]+\}|[^\p{Pd} ]+)\s*(\p{Pd}+)\s*(\{[^\}]+\}|\P{Pd}*)\s*\z"xms, &value) {
      (one, (!two.is_empty()).then_some(three))
    } else if let Some((_, one, two, three)) = regex_captures!(r"\A\s*(.+)(\p{Pd}{2,})(.+)\s*\z"xms, &value) {
      // M-1--M-4
      (one, (!two.is_empty()).then_some(three))
    } else if let Some((_, one, two, three)) = regex_captures!(r"\A\s*(.+)(\p{Pd}+)(.+)\s*\z"xms, &value) {
      // blah M-1
      (one, (!two.is_empty()).then_some(three))
    } else {
      panic!()
    };
    let start = regex_replace!(r"\A\{([^\}]+)\}\z", start, |_, one: &str| one.to_string());
    let end = end.skip_empty().map(|end| regex_replace!(r"\A\{([^\}]+)\}\z", end, |_, one: &str| one.to_string()));
    if !start.is_empty() {
      values_ref.push(($start || "", $end));
    } else {
      biber_warn("Range field '$field' in entry '$key' is malformed, falling back to literal", $bibentry);
      values_ref.push((ovalue, None));
    }
  }
  values_ref
}

// Names
fn _name(bibentry, entry, field, key) {
  let fc = UniCase::new($field); // Casefolded field which is what we need internally
  let secnum = crate::MASTER.get_current_section();
  let section = crate::MASTER.sections().get_section(secnum);
  let $value = $entry->get(encode("UTF-8", NFC(field)));
  let $xnamesep = crate::Config->getoption("xnamesep");
  let bee = bibentry.get_field("entrytype");

  let $names = crate::Entry::Names->new("type" => $fc);

  let @tmp = Text::BibTeX::split_list(NFC($value),// Unicode NFC boundary
                                     crate::Config->getoption("namesep"),
                                     None,
                                     None,
                                     None,
                                     {binmode => "UTF-8", normalization => "NFD"});

  for (i, name) in tmp.iter().enumerate() {
    // Record any XDATA and skip if we did
    if bibentry.add_xdata_ref(field, name, Some(i)) {
      // Add special xdata ref empty name as placeholder
      names.add_name(crate::Entry::Name->new(xdata => $name));
      continue;
    }

    // per-namelist options
    if ($name =~ m/^(\S+)\s*$xnamesep\s*(\S+)?$/) {
      let $nlo = ($1).to_lowercase();
      let $nlov = $2.unwrap_or(1); // bare options are just boolean numerals
      if CONFIG_OPT_SCOPE_BIBLATEX.contains_pair(&nlo, "NAMELIST") {
        let $oo = expand_option_input($nlo, $nlov, $CONFIG_BIBLATEX_OPTIONS{NAMELIST}{$nlo}{INPUT});

        for o in ($oo->@*) {
          let $method = "set_" . $o->[0];
          $names->$method($o->[1]);
        }
        continue;
      }
    }

    // Consecutive "and" causes Text::BibTeX::Name to segfault
    if !($name) {
      biber_warn("Name in key '$key' is empty (probably consecutive 'and'): skipping entry '$key'", bibentry);
      $section->del_citekey($key);
      return "BIBER_SKIP_ENTRY";
    }

    let $nps = dm.get_constant_value("nameparts").join("|");
    let $no;

    // extended name format
    let $xnamesep = crate::Config->getoption("xnamesep");
    if ($name =~ m/(?:$nps)\s*$xnamesep/ && !crate::Config->getoption("noxname")) {
      // Skip names that don't parse for some reason
      // uniquename defaults to 'false' just in case we are in tool mode otherwise
      // there are spurious uninitialised warnings

      if !($no = parsename_x(section, name, fc, key)) {
        continue;
      }
    }
    else { // Normal bibtex name format
      // Check for malformed names in names which aren't completely escaped
      // Too many commas
      if !(name.starts_with('{') && name.ends_with('}')) { // Ignore these tests for escaped names
        if name.matches(',').count() > 1 {
          biber_error("Name \"$name\" has too many commas, skipping entry '$key'", true);
          section.del_citekey(key);
          return "BIBER_SKIP_ENTRY";
        }

        // Consecutive commas cause Text::BibTeX::Name to segfault
        if name.contains(",,") {
          biber_error("Name \"$name\" is malformed (consecutive commas): skipping entry '$key'", true);
          section.del_citekey(key);
          return "BIBER_SKIP_ENTRY";
        }
      }

      // Skip names that don't parse for some reason
      // unique name defaults to 0 just in case we are in tool mode otherwise there are spurious
      // uninitialised warnings
      if !($no = parsename(section, name, fc)) {
        continue;
      }
    }

    // Deal with implied "et al" in data source
    if no.get_rawstring().to_lowercase() == crate::Config->getoption("others_string") {
      names.set_morenames();
    }
    else {
      if $no {
        names.add_name(no);
      }
    }
  }

  // Don't set if there were no valid names due to special errors above
  return if names.count() { $names } else { None };
}

// Dates
fn _datetime(bibentry: &mut Entry, entry, field: &str, key: &str) {
  let $datetype = $field =~ s/date\z//xmsr;
  let $date = $entry->get(encode("UTF-8", NFC($field)));
  let secnum = crate::MASTER.get_current_section();
  let section = crate::MASTER.sections().get_section(secnum);
  let $ds = $section->get_keytods($key);
  let $bee = bibentry.get_field("entrytype");

  let (sdate, edate, sep, unspec) = parse_date_range(bibentry, datetype, date);

  // Date had unspecified format
  // This does not differ for *enddate components as these are split into ranges
  // from non-ranges only
  if ($unspec) {
    bibentry.set_field(format!("{datetype}dateunspecified"), $unspec);
  }

  if (defined($sdate)) { // Start date was successfully parsed
    if ($sdate) { // Start date is an object not "0"
      // Did this entry get its datepart fields from splitting an IS08601 date field?
      bibentry.set_field(format!("{datetype}datesplit"), 1);

      // Some warnings for overwriting YEAR and MONTH from DATE
      if (sdate.year &&
          (format!("{datetype}year") == "year") && // ???
          entry.get("year") &&
          sdate.year != entry.get("year")) {
        biber_warn("Overwriting field 'year' with year value from field 'date' for entry '$key'", $bibentry);
      }
      if (!$CONFIG_DATE_PARSERS{start}->missing("month") &&
          (format!("{datetype}month") == "month") &&
          entry.get("month") &&
          sdate.month != entry.get("month")) {
        biber_warn("Overwriting field 'month' with month value from field 'date' for entry '$key'", $bibentry);
      }

      // Save julian
      if $CONFIG_DATE_PARSERS{start}->julian {
        bibentry.set_field(format!("{datetype}datejulian"), 1);
      }
      if $CONFIG_DATE_PARSERS{end}->julian {
        bibentry.set_field(format!("{datetype}enddatejulian"), 1);
      }

      // Save approximate information
      if $CONFIG_DATE_PARSERS{start}->approximate {
        bibentry.set_field(format!("{datetype}dateapproximate"), 1);
      }
      if $CONFIG_DATE_PARSERS{end}->approximate {
        bibentry.set_field(format!("{datetype}enddateapproximate"), 1);
      }

      // Save uncertain date information
      if $CONFIG_DATE_PARSERS{start}->uncertain {
        bibentry.set_field(format!("{datetype}dateuncertain"), 1);
      }
      if $CONFIG_DATE_PARSERS{end}->uncertain {
        bibentry.set_field(format!("{datetype}enddateuncertain"), 1);
      }

      // Save start yeardivision date information
      if (let $yeardivision = $CONFIG_DATE_PARSERS{start}->yeardivision) {
        bibentry.set_field(format!("{datetype}yeardivision"), $yeardivision);
      }

      if !($CONFIG_DATE_PARSERS{start}->missing("year")) {
        bibentry.set_datafield(format!("{datetype}year"),
                                 $CONFIG_DATE_PARSERS{start}->resolvescript(sdate.year));
        // Save era date information
        bibentry.set_field(format!("{datetype}era"), sdate.secular_era.to_lowercase());
      }

      if !($CONFIG_DATE_PARSERS{start}->missing("month")) {
        bibentry.set_datafield(format!("{datetype}month"),
                               $CONFIG_DATE_PARSERS{start}->resolvescript(sdate.month));
      }

      if !($CONFIG_DATE_PARSERS{start}->missing("day")) {
        bibentry.set_datafield(format!("{datetype}day"),
                               $CONFIG_DATE_PARSERS{start}->resolvescript(sdate.day));
      }

      // time
      if !($CONFIG_DATE_PARSERS{start}->missing("time")) {
        bibentry.set_datafield(format!("{datetype}hour"),
                                 $CONFIG_DATE_PARSERS{start}->resolvescript(sdate.hour));
        bibentry.set_datafield(format!("{datetype}minute"),
                                 $CONFIG_DATE_PARSERS{start}->resolvescript(sdate.minute));
        bibentry.set_datafield(format!("{datetype}second"),
                                 $CONFIG_DATE_PARSERS{start}->resolvescript(sdate.second));
        if !(sdate.time_zone.is_floating()) { // ignore floating timezones
          bibentry.set_datafield(format!("{datetype}timezone"), tzformat(sdate.time_zone.name));
        }
      }
    }
    else { // open ended range - startdate is defined but empty
      bibentry.set_datafield(format!("{datetype}year"), "");
    }

    // End date can be missing
    if ($sep) {
      if (defined($edate)) { // End date was successfully parsed
        if ($edate) { // End date is an object not "0"
          // Did this entry get its datepart fields from splitting an ISO8601-2 date field?
          bibentry.set_field(format!("{datetype}datesplit"), 1);

          if !($CONFIG_DATE_PARSERS{end}->missing("year")) {
            bibentry.set_datafield(format!("{datetype}endyear"),
                                     $CONFIG_DATE_PARSERS{end}->resolvescript(edate.year));
            // Save era date information
            bibentry.set_field(format!("{datetype}endera"), edate.secular_era.to_lowercase());
          }

          if !($CONFIG_DATE_PARSERS{end}->missing("month")) {
            bibentry.set_datafield(format!("{datetype}endmonth"),
                                   $CONFIG_DATE_PARSERS{end}->resolvescript(edate.month));
          }

          if !($CONFIG_DATE_PARSERS{end}->missing("day")) {
            bibentry.set_datafield(format!("{datetype}endday"),
                                   $CONFIG_DATE_PARSERS{end}->resolvescript(edate.day));
          }

          // Save end yeardivision date information
          if (let $yeardivision = $CONFIG_DATE_PARSERS{end}->yeardivision) {
            bibentry.set_field(format!("{datetype}endyeardivision"), yeardivision);
            bibentry.set_field(format!("{datetype}endseaason"), yeardivision); // legacy
          }

          // must be an hour if there is a time but could be 00 so use defined()
          if !($CONFIG_DATE_PARSERS{end}->missing("time")) {
            bibentry.set_datafield(format!("{datetype}endhour"),
                                     $CONFIG_DATE_PARSERS{end}->resolvescript(edate.hour));
            bibentry.set_datafield(format!("{datetype}endminute"),
                                    $CONFIG_DATE_PARSERS{end}->resolvescript(edate.minute));
            bibentry.set_datafield(format!("{datetype}endsecond"),
                                     $CONFIG_DATE_PARSERS{end}->resolvescript(edate.second));
            if !(edate.time_zone.is_floating()) { // ignore floating timezones
              bibentry.set_datafield(format!("{datetype}endtimezone"), tzformat(edate.time_zone.name));
            }
          }
        }
        else { // open ended range - enddate is defined but empty
          bibentry.set_datafield(format!("{datetype}endyear"), "");
        }
      }
      else {
        biber_warn("$bee entry '$key' ($ds): Invalid format '$date' of end date field '$field' - ignoring", $bibentry);
      }
    }
  }
  else {
    biber_warn("$bee entry '$key' ($ds): Invalid format '$date' of date field '$field' - ignoring", $bibentry);
  }
  return;
}

// Bibtex list fields with listsep separator
fn _list(bibentry: &mut Entry, entry, field: &str) {
  let $value = $entry->get(encode("UTF-8", NFC($field)));

  let @tmp = Text::BibTeX::split_list(NFC($value),// Unicode NFC boundary
                                     crate::Config->getoption("listsep"),
                                     None,
                                     None,
                                     None,
                                     {binmode => "UTF-8", normalization => "NFD"});
  @tmp = map { (remove_outer($_))[1] } @tmp;
  let mut result = Vec::new();

  for (i, e) in tmp.iter().enumerate() {
    // Record any XDATA and skip if we did
    bibentry.add_xdata_ref(field, e, Some(i));

    result.push(e);
  }

  return [ @result ];
}

// Bibtex uri lists
fn _urilist(bibentry: &mut Entry, entry, field: &str) {
  let $value = entry.get(encode("UTF-8", NFC(field)));

  // Unicode NFC boundary (passing to external library)
  let @tmp = Text::BibTeX::split_list(NFC($value),
                                     crate::Config->getoption("listsep"),
                                     None,
                                     None,
                                     None,
                                     {binmode => "UTF-8", normalization => "NFD"});
  let result = Vec::new();

  for (i, e) in tmp.iter().enumerate() {
    // Record any XDATA and skip if we did
    bibentry.add_xdata_ref(field, e, Some(i));

    result.push(e);
  }

  result

}

/// Caches file data into T::B objects indexed by the original
/// datasource key, decoded into UTF8
fn cache_data(filename, encoding) {
  let secnum = crate::MASTER.get_current_section();
  let section = crate::MASTER.sections().get_section(secnum);

  // Initialise this
  $cache->{preamble}{$filename} = [];

  // Convert/decode file
  let $pfilename = preprocess_file($filename, $encoding);

  let $bib = Text::BibTeX::File->new();
  $bib->open($pfilename, {binmode => "UTF-8", normalization => "NFD"}) || biber_error("Cannot create Text::BibTeX::File object from $pfilename: $!");

  // Log that we found a data file
  info!("Found BibTeX data source '{}'", filename);

  while ( let $entry = Text::BibTeX::Entry->new($bib) ) {
    if ( $entry->metatype == BTE_PREAMBLE ) {
      cache->{preamble}{$filename}.push(entry->value);
      continue;
    }

    // Save comments for output in tool mode unless comment stripping is requested
    if ( $entry->metatype == BTE_COMMENT ) {
      if (crate::Config->getoption("tool") && !crate::Config->getoption("strip_comments") ) {
        cache->{comments}{$filename}.push(process_comment($entry->value));
      }
      continue;
    }

    // Record macros in T::B so we can output then properly in tool mode
    if ($entry->metatype == BTE_MACRODEF) {
      for f in ($entry->fieldlist) {
        $RSTRINGS{$entry->get($f)} = $f;
      }
      continue;
    }

    // Ignore misc BibTeX entry types we don't care about
    if ( $entry->metatype == BTE_UNKNOWN ) {
      continue;
    }

    // If an entry has no key, ignore it and warn
    if !($entry->key) {
      biber_warn("Invalid or undefined BibTeX entry key in file '$pfilename', skipping ...");
      continue;
    }

    // Text::BibTeX >= 0.46 passes through all citekey bits, thus allowing UTF-8 keys
    let $key = $entry->key;

    // Check if this key has already been registered as a citekey alias, if
    // so, the key takes priority and we delete the alias
    if (exists($cache->{data}{citekey_aliases}{$key})) {
      biber_warn("Citekey alias '$key' is also a real entry key, skipping ...");
      delete($cache->{data}{citekey_aliases}{$key});
    }

    // Any secondary keys?
    // We can't do this with a driver dispatch for the IDS field as this needs
    // an entry object creating first and the whole point of aliases is that
    // there is no entry object
    if (let $ids = $entry->get("ids")) {
      let $Srx = crate::Config->getoption("xsvsep");
      let $S = qr/$Srx/;
      for id in Regex::new(format!(r"{S}")).unwrap().split(ids) {

        // Skip aliases which are this very key (deep recursion ...)
        if id == key {
          biber_warn("BAD RECURSION! Entry alias '$id' is identical to the entry key, skipping ...");
          continue;
        }

        // Skip aliases which are also real entry keys
        if section.has_everykey(id) {
          biber_warn("Entry alias '$id' is also a real entry key, skipping ...");
          continue;
        }

        // Warn on conflicting aliases
        if (exists($cache->{data}{citekey_aliases}{$id})) {
          let $otherid = $cache->{data}{citekey_aliases}{$id};
          if ($otherid != $key) {
            biber_warn("Entry alias '$id' already has an alias '$otherid', skipping ...");
          }
        }
        else {
          $cache->{data}{citekey_aliases}{$id} = $key;
            debug!("Entry alias '{}' is an alias for citekey '{}'", id, key);
        }
      }
    }

    // If we've already seen a case variant, warn
    // This is case mismatch test of datasource entries with other datasource entries
    if (let $okey = $section->has_badcasekey($key)) {
      biber_warn("Possible typo (case mismatch) between datasource keys: '$key' and '$okey' in file '$filename'");
    }

    // If we've already seen this key in a datasource, ignore it and warn unless user wants
    // duplicates
    if ($section->has_everykey($key) && !crate::Config->getoption("noskipduplicates")) {
      biber_warn("Duplicate entry key: '$key' in file '$filename', skipping ...");
      continue;
    }
    else {
      if ($section->has_everykey($key)) {
        biber_warn("Duplicate entry key: '$key' in file '$filename'");
      }
      $section->add_everykey($key);
    }

    // Bad entry
    if !($entry->parse_ok) {
      biber_warn("Entry $key does not parse correctly");
      continue;
    }

    // Cache the entry so we don't have to read the file again on next pass.
    // Two reasons - So we avoid T::B macro redef warnings and speed
    // Create a global "all datasources" cache too as this is useful in places
    $cache->{data}{GLOBALDS}{$key} = $cache->{data}{$filename}{$key} = $entry;
    // We do this as otherwise we have no way of determining the original .bib entry order
    // We need this in order to do sorting=none + allkeys because in this case, there is no
    // "citeorder" because nothing is explicitly cited and so "citeorder" means .bib order
    push $cache->{orig_key_order}{$filename}->@*, $key;
      debug!("Cached Text::BibTeX entry for key '{}' from BibTeX file '{}'", key, filename);
  }

  $bib->close; // If we don't do this, we can't unlink the temp file on Windows
  return;
}


/// Convert file to UTF-8 and potentially decode LaTeX macros to UTF-8
fn preprocess_file(filename, benc) {
  // Put the utf8 encoded file into the global biber tempdir
  // We have to do this in case we can't write to the location of the
  // .bib file
  let $td = crate::MASTER.biber_tempdir();
  let (_, _, fn_) = File::Spec->splitpath($filename);

  // The filename that Text::BibTeX actually opens cannot be UTF-8 on Windows as there is no
  // way to do this with the correct Win32::Unicode:File calls and so we normalise to a hash
  // of the name so that it will work cross-platform.
  let $fnh = md5_hex(encode_utf8(NFC(fn_)));
  let $ufilename = File::Spec->catfile($td->dirname, "${fnh}_$$.utf8");
  debug!("File '{}' is converted to UTF8 as '{}'", fn_, ufilename);

  // We read the file in the bib encoding and then output to UTF-8, even if it was already UTF-8,
  // just in case there was a BOM so we can delete it as it makes T::B complain
  // Might fail due to encountering characters invalid in the encoding so trap and die gracefully
  if ($benc == "ascii") {
    info!("Reading ascii input as UTF-8");
    $benc = "UTF-8";
  }
  let $buf;
  if !(eval{$buf = NFD(slurp_switchr($filename, $benc)->$*)}) {// Unicode NFD boundary
    biber_error("Data file '$filename' cannot be read in encoding '$benc': $@");
  }

  // strip UTF-8 BOM if it exists - this just makes T::B complain about junk characters
  buf = regex_replace!(r"\A\u{feff}", &buf);

  // Normalise line breaks because libbtparse can't handle things like CR only
  // in some circumstances
  buf = regex_replace_all!(r"\R", &buf, "\n");

  slurp_switchw($ufilename, $buf);// Unicode NFC boundary

  let $lbuf = parse_decode($ufilename);

    trace!("Buffer after decoding -> '{}'", lbuf);

  slurp_switchw($ufilename, $lbuf);// Unicode NFC boundary

  return $ufilename;
}

/// Partially parse the .bib datasource and latex_decode the data contents.
/// We do this because latex_decoding the entire buffer is difficult since
/// such decoding is regexp based and since braces are used to protect data in
/// .bib files, it makes it hard to do some parsing.
fn parse_decode(ufilename: &Path) -> String {
  let $dmh = crate::config::get_dm_helpers();
  let mut lbuf = String::new();

  let $bib = Text::BibTeX::File->new();
  $bib->open($ufilename, {binmode => "UTF-8", normalization => "NFD"}) || biber_error("Cannot create Text::BibTeX::File object from $ufilename: $!");

  info!("LaTeX decoding ...");

  while ( let $entry = Text::BibTeX::Entry->new($bib) ) {
    match entry->metatype {
      BTE_REGULAR => {
        lbuf.push_str(&format!("@{}{{{},\n", entry->type, entry->key));
        for f in ($entry->fieldlist) {
          let mut fv = $entry->get(encode("UTF-8", NFC($f))); // NFC boundary: $f is "output" to Text::BibTeX

          // Don't decode verbatim fields
          if !dmh.verbs.iter().any(|v| unicase::eq(f, v)) {
            fv = crate::LaTeX::Recode::latex_decode(fv);
          }
          lbuf.push_str(&format!("  {f} = {{{fv}}},\n"));
        }
        lbuf.push_str("\n}\n\n");
      }
      BTE_PREAMBLE => {
        lbuf.push_str("@PREAMBLE{\"");
        lbuf.push_str(entry->value);
        lbuf.push_str("\"}\n");
      }
      BTE_COMMENT => {
        lbuf.push_str("@COMMENT{");
        lbuf.push_str(entry->value);
        lbuf.push_str("}\n");
      }
      BTE_MACRODEF => {
        lbuf.push_str("@STRING{");
        for f in ($entry->fieldlist) {
          lbuf.push_str(&format!("{f} = {{{}}}", entry.get(encode("UTF-8", NFC(f)))));
        }
        lbuf.push_str("}\n");
      }
      _ => {
        lbuf.push_str(&crate::LaTeX::Recode::latex_decode($entry->print_s));
      }
    }
  }

  // (Re-)define the old BibTeX month macros to what biblatex wants unless user stops this
  if !(crate::Config->getoption("nostdmacros")) {
    for (mon, monn) in &MONTHS {
      Text::BibTeX::add_macro_text(mon, monn);
    }
  }

  $bib->close;

  lbuf
}

/// Given a name string, this function returns a crate::Entry::Name object
/// with all parts of the name resolved according to the BibTeX conventions.
///
/// `parsename('John Doe', "author", "key")`
/// returns an object which internally looks a bit like this:
///
/// ```
/// { given          => {string => "John", initial => ['J']},
///   family         => {string => "Doe", initial => ['D']},
///   prefix         => {string => None, initial => None},
///   suffix         => {string => None, initial => None},
///   id             => 32RS0Wuj0P,
///   strip          => {"given"  => 0,
///                      "family" => 0,
///                      "prefix" => 0,
///                      "suffix" => 0}
///   }
/// ```
fn parsename(section, namestr, fieldname) {
  // First sanitise the namestring due to Text::BibTeX::Name limitations on whitespace
  $namestr =~ s/\A\s*|\s*\z//xms; // leading and trailing whitespace
  // Collapse internal whitespace and escaped spaces like in "Christina A. L.\ Thiele"
  $namestr =~ s/\s+|\\\s/ /g;
  $namestr =~ s/\A\{\{+([^\{\}]+)\}+\}\z/{$1}/xms; // Allow only one enveloping set of braces

  // If requested, try to correct broken initials with no space between them.
  // This can slightly mess up some other names like {{U.K. Government}} etc.
  // btparse can't do this so we do it before name parsing
  if crate::Config->getoption("fixinits") {
    $namestr =~ s/(\w)\.(\w)/$1. $2/g;
  }

  let %namec;
  let $name = Text::BibTeX::Name->new({binmode => "UTF-8", normalization => "NFD"}, NFC($namestr));

  // Formats so we can get BibTeX compatible nbsp inserted
  let $l_f = Text::BibTeX::NameFormat->new('l', 0);
  let $f_f = Text::BibTeX::NameFormat->new('f', 0);
  let $p_f = Text::BibTeX::NameFormat->new('v', 0);
  let $s_f = Text::BibTeX::NameFormat->new('j', 0);
  $l_f->set_options(BTN_LAST,  0, BTJ_MAYTIE, BTJ_NOTHING);
  $f_f->set_options(BTN_FIRST, 0, BTJ_MAYTIE, BTJ_NOTHING);
  $p_f->set_options(BTN_VON,   0, BTJ_MAYTIE, BTJ_NOTHING);
  $s_f->set_options(BTN_JR,    0, BTJ_MAYTIE, BTJ_NOTHING);

  // Generate name parts
  $namec{family} = $name->format($l_f);
  $namec{given}  = $name->format($f_f);
  $namec{prefix} = $name->format($p_f);
  $namec{suffix} = $name->format($s_f);

  // Not using Text::BibTeX for initials generation as it can't handle combining
  // chars and diacritics in general

  // basic bibtex names have a fixed data model
  for np in ("prefix", "family", "given", "suffix") {
    if ($namec{$np}) {
      ($namec{"${np}-strippedflag"}, $namec{"${np}-stripped"}) = remove_outer($namec{$np});

      // Protect spaces inside {} when splitting to produce initials
      let $part = $namec{$np};
      if ($namec{"${np}-strippedflag"}) {
        $part = regex!(r"\s+").replace_all($namec{$np}, "_");
      }

      // strip noinit
      $part = strip_noinit($part);

      // split on spaces/tilde outside of brace block
      $namec{"${np}-i"} = [gen_initials(regex!(r"[\h~]+(?![^{]*\})").split(part))];
    }
  }

  let %nameparts;
  let $strip;
  for np in ["prefix", "family", "given", "suffix"] {
    $nameparts{$np} = {string  => $namec{"${np}-stripped"}.unwrap_or(None),
                       initial => if $namec{$np} { $namec{"${np}-i"} } else { None }
                      };
    $strip->{$np} = $namec{"${np}-strippedflag"};

    // Record max namepart lengths
    $section->set_np_length($np, length($nameparts{$np}{string})) if $nameparts{$np}{string};
    $section->set_np_length("${np}-i", length(join("", $nameparts{$np}{initial}->@*))) if $nameparts{$np}{initial};
  }

  // The "strip" entry tells us which of the name parts had outer braces
  // stripped during processing so we can add them back when printing the
  // .bbl so as to maintain maximum BibTeX compatibility
  return  crate::Entry::Name->new(
                                  %nameparts,
                                  strip => $strip
                                 );
}

/// Given a name string in extended format, this function returns a crate::Entry::Name object
/// with all parts of the name resolved according to the BibTeX conventions.
///
/// `parsename_x('given=John, family=Doe')`
/// returns an object which internally looks a bit like this:
///
/// ```
/// { given          => {string => "John", initial => ['J']},
///   family         => {string => "Doe", initial => ['D']},
///   prefix         => {string => None, initial => None},
///   suffix         => {string => None, initial => None},
///   id             => 32RS0Wuj0P,
///   sortingnamekeytemplatename => 'template name',
/// }
/// ```
fn parsename_x(section: &mut Section, namestr: &str, fieldname: &str, key: &str) -> Name {
  let $xnamesep = crate::Config->getoption("xnamesep");
  let %nps = map {$_ => 1} $dm->get_constant_value("nameparts");

  let %namec;
  let %pernameopts;
  for np in (split_xsv($namestr)) {// Can have x inside records so use Text::CSV
    let ($npn, $npv) = $np =~ m/^(.+)\s*$xnamesep\s*(.+)$/x;
    let npn = npn.to_lowercase();

    // per-name options
    if CONFIG_OPT_SCOPE_BIBLATEX.contains_pair(npn, "NAME") {
      let $oo = expand_option_input(npn, $npv, $CONFIG_BIBLATEX_OPTIONS{NAME}{$npn}{INPUT});

      for o in ($oo->@*) {
        $pernameopts{$o->[0]} = $o->[1];
      }
      continue;
    }

    if !($nps{regex!(r"-i$").replace(npn, "")}) {
      biber_warn("Invalid namepart '$npn' found in extended name format name '$fieldname' in entry '$key', ignoring");
      continue;
    }

    if regex_is_match!(r"-i$", npn) {
      $namec{$npn} = _split_initials($npv);
    }
    else {
      // Don't tie according to bibtex rules if the namepart is protected with braces
      if (has_outer($npv)) {
        $namec{$npn} = $npv;
      }
      else {
        $namec{$npn} = join_name_parts(regex!(r"\s+").split(npv));
      }
    }
  }

  for np in nps.keys() {
    if (exists($namec{$np})) {
      // Generate any stripped information
      (let $s, $namec{$np}) = remove_outer($namec{$np});

      // Protect spaces inside {} when splitting to produce intials
      let $part = $namec{$np};
      if ($s) {
        $part = regex!(r"\s+/_").replace_all(namec{$np}, "");
      }

      // strip noinit
      $part = strip_noinit($part);

      // Generate any initials which are missing
      if (!exists($namec{&format!("{np}-i")})) {
        $namec{"${np}-i"} = [gen_initials(regex!(r"[\s~]+").split(part))];
      }
    }
  }

  let %nameparts;
  for np in nps.keys() {
    $nameparts{$np} = {string  => $namec{$np}.unwrap_or(None),
                       initial => if exists($namec{$np}) { $namec{&format!("{np}-i")} } else { None }
                      };

    // Record max namepart lengths
    $section->set_np_length($np, length($nameparts{$np}{string}))  if $nameparts{$np}{string};
    $section->set_np_length("${np}-i", length(join("", $nameparts{$np}{initial}->@*)))  if $nameparts{$np}{initial};
  }

  // The "strip" entry tells us which of the name parts had outer braces
  // stripped during processing so we can add them back when printing the
  // .bbl so as to maintain maximum BibTeX compatibility
  return  crate::Entry::Name->new(%nameparts, %pernameopts);
}

// Routine to try to hack month into the right biblatex format
// Especially since we support remote .bibs which we potentially have no control over
let %months = (
              "jan" => "1",
              "feb" => "2",
              "mar" => "3",
              "apr" => "4",
              "may" => "5",
              "jun" => "6",
              "jul" => "7",
              "aug" => "8",
              "sep" => "9",
              "oct" => "10",
              "nov" => "11",
              "dec" => "12"
             );

fn _hack_month(in_month: &str) -> String {
  if let Some(_, m) = regex_captures!(r"\A\s*((?:jan|feb|mar|apr|may|jun|jul|aug|sep|oct|nov|dec).*)\s*\z"i, in_month) {
    months.get(m.graphemes(true).take(3).collect::<String>().to_lowercase()).unwrap().into()
  } else {
    in_month.into()
  }
}

// "ab{cd}e" -> [a,b,cd,e]
fn _split_initials(npv) -> Vec<String> {
  let mut npv = Vec::new();
  let mut ci = false;
  let mut acc = String::new();

  for c in regex!(r"\b{gcb}".split(npv) {
    // entering compound initial
    if c == "{" {
      ci = true;
    }
    // exiting compound initial, push accumulator and reset
    else if c == "}" {
      ci = false;
      npv.push(acc);
      acc = String::new();
    }
    else {
      if ci {
        acc.push_str(c);
      }
      else {
        npv.push(c);
      }
    }
  }
  npv
}
