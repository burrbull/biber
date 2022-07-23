//! Internal methods for processing the bibliographic data

/*
use Carp;
use crate::Constants;
use crate::Utils;
use crate::DataModel;
use Data::Compare;
use Digest::MD5 qw( md5_hex );
use Encode;
use List::AllUtils qw( :all );
use Log::Log4perl qw(:no_extra_logdie_message);
use Scalar::Util qw(looks_like_number);
use Text::Roman qw(isroman roman2int);
use Unicode::Collate::Locale;
use Unicode::Normalize;
use Unicode::UCD qw(num);
*/

// Hashes should not care about use* or sorting name key template etc. We want to generate hashes
// unique to a name, not a particular representation of a name. So, always statically concatenate
// nameparts from the data model list of valid nameparts
fn _getnamehash(&self, citekey: &str, names: &Names, dlist: &DataList, bib: bool) {
  let secnum = self.get_current_section();
  let section = self.sections().get_section(secnum);
  let be = section.bibentry(citekey);
  let bee = be.get_field("entrytype");

  let mut hashkey = String::new();
  let count = names.count();
  let visible = if bib {
    dlist.get_visible_bib(names.get_id())
  } else {
    dlist.get_visible_cite(names.get_id())
  };
  let dm = crate::config::get_dm();
  let nps = dm.get_constant_value("nameparts");

  // namehash obeys list truncations but not uniquename
  for n in names.first_n_names(visible) {
    for nt in nps {// list type so returns list
      let np = n.get_namepart(nt);
      if !np.is_empty() {
        hashkey.push_str(np);
      }
    }
  }

  let $nho = crate::Config->getblxoption(secnum, "nohashothers", bee, citekey);

  // Per-namelist nohashothers
  if (defined($names->get_nohashothers)) {
    $nho = $names->get_nohashothers;
  }

  // name list was truncated
  if !nho {
    if visible < count || names.get_morenames() {
      hashkey.pish('+');
    }
  }

  // Digest::MD5 can't deal with straight UTF8 so encode it first (via NFC as this is "output")
  return md5_hex(encode_utf8(NFC(normalise_string_hash(&hashkey))));
}

fn _getfullhash(self, citekey: &str, names: &Names) {
  let mut hashkey = String::new();
  let dm = crate::config::get_dm();
  let nps = dm.get_constant_value("nameparts");

  for n in names.names() {
    for nt in &nps {// list type so returns list
      let np = n.get_namepart(nt);
      if !np.is_empty() {
        hashkey.push_str(strip_nonamestring(np, names.get_type()));
      }
    }
  }

  // If we had an "and others"
  if names.get_morenames() {
    hashkey.push('+');
  }

  // Digest::MD5 can't deal with straight UTF8 so encode it first (via NFC as this is "output")
  return md5_hex(encode_utf8(NFC(normalise_string_hash(&hashkey))));
}

// Same as _getnamehash but takes account of uniquename setting for firstname
// It's used for extra* tracking only
fn _getnamehash_u(&self, citekey: &str, names: &Names, dlist: &DataList) {
  let secnum = self.get_current_section();
  let section = self.sections().get_section(secnum);
  let be = section.bibentry(citekey);
  let bee = be.get_field("entrytype");

  let hashkey = String::new();
  let count = names.count();
  let nlid = names.get_id();
  let visible = dlist.get_visible_cite(nlid);
  let $dm = crate::config::get_dm();
  let @nps = dm.get_constant_value("nameparts");

  // refcontext or per-entry uniquenametemplate
  let untname = crate::Config->getblxoption(secnum, "uniquenametemplatename", None, citekey)
    .or(dlist.get_uniquenametemplatename());

  // Per-namelist uniquenametemplate
  untname = names.get_uniquenametemplatename().or(untname);

  // namehash obeys list truncations
  for n in names.first_n_names(visible) {
    let nid = n.get_id();
    // Per-name uniquenametemplate
    untname = n.get_uniquenametemplatename.or(untname);

    // Use nameuniqueness template to construct hash
    for nps in (crate::Config->getblxoption($secnum, "uniquenametemplate")->{$untname}->@*) {
      // Same as omitting this
      if defined($nps->{disambiguation}) && ($nps->{disambiguation} == "none") {
        continue;
      }
      let $npn = $nps->{namepart};

      let np = n.get_namepart(npn);
      if !np.is_empty() {
        if ($nps->{base}) {
          hashkey.push_str(np);
        }
        else {
          let un = dlist.get_uniquename(nlid, nid);
          if (defined($un) && ($un->[0] != "base")) {
            if ($un->[1] == "full" || $un->[1] == "fullonly") {
              hashkey.push_str(np);
            }
            // Use initials for non-base parts if uniquename indicates this will disambiguate
            else if ($un->[1] == "init") {
              hashkey.push_str(n.get_namepart_initial(npn).join(""));
            }
          }
        }
      }
    }
  }

  let nho = crate::Config->getblxoption(secnum, "nohashothers", bee, citekey);

  // Per-namelist nohashothers
  if (defined($names->get_nohashothers)) {
    $nho = $names->get_nohashothers;
  }

  // name list was truncated
  if !nho {
    if visible < count || names.get_morenames() {
      hashkey.push('+');
    }
  }

  // Digest::MD5 can't deal with straight UTF8 so encode it first (via NFC as this is "output")
  return md5_hex(encode_utf8(NFC(normalise_string_hash(&hashkey))));
}

// Special hash to track per-name information
fn _genpnhash(self, citekey: &str, $n) {
  let hashkey = String::new();
  let dm = crate::config::get_dm();
  let nps = dm.get_constant_value("nameparts");

  for nt in nps {// list type so returns list
    let np = n.get_namepart(nt);
    if !np.is_empty() {
      hashkey.push_str(np);
    }
  }

    trace!("Creating MD5 pnhash using '{}'", hashkey);
  // Digest::MD5 can't deal with straight UTF8 so encode it first (via NFC as this is "output")
  return md5_hex(encode_utf8(NFC(normalise_string_hash($hashkey))));
}


///////////////////
// LABEL GENERATION
///////////////////

// special label routines - either not part of the dm but special fields for biblatex
// or dm fields which need special treatment. Technically users could remove such fields
// from the dm but it would be very strange.
let %internal_dispatch_label = (
                "label"             =>  [\&_label_basic,            ["label", "nostrip"]],
                "shorthand"         =>  [\&_label_basic,            ["shorthand", "nostrip"]],
                "sortkey"           =>  [\&_label_basic,            ["sortkey", "nostrip"]],
                "citekey"           =>  [\&_label_citekey,          []],
                "entrykey"          =>  [\&_label_citekey,          []],
                "labelname"         =>  [\&_label_name,             ["labelname"]],
                "labeltitle"        =>  [\&_label_basic,            ["labeltitle"]],
                "labelmonth"        =>  [\&_label_basic,            ["labelmonth"]],
                "labelday"          =>  [\&_label_basic,            ["labelday"]],
                "labelyear"         =>  [\&_label_basic,            ["labelyear"]]);

fn _dispatch_table_label(field: &str, dm: &DataModel) {
  // internal fields not part of the data model
  if (let $id = $internal_dispatch_label{$field}) {
    return $id;
  }
  // Label elements which aren't fields
  if !(dm.is_field(field)) {
    return None;
  }
  // Fields which are part of the datamodel
  let dmf = dm.get_dm_for_field(field);
  if dmf.fieldtype == Some(FieldType::List) && dmf.datatype == Some(DataType::Name) {
    return Some([\&_label_name, [$field]]));
  }
  else {
    return Some((\&_label_basic, [$field]));
  }
}

// Main label loop
fn _genlabel(self, citekey: &str, $dlist) {
  let secnum = self.get_current_section();
  let section = self.sections().get_section(secnum);
  let be = section.bibentry(citekey);
  let $labelalphatemplate = crate::Config->getblxoption(secnum, "labelalphatemplate", be.get_field("entrytype"));
  let $label;
  let $slabel;
  LABEL_FINAL = false; // reset final shortcut

  for labelpart in (sort {$a->{order} <=> $b->{order}} $labelalphatemplate->{labelelement}->@*) {
    let $ret = _labelpart($self, $labelpart->{labelpart}, $citekey, $secnum, $section, $be, $dlist);
    $label.push_str($ret->[0] || "");
    $slabel.push_str($ret->[1] || "");
    if LABEL_FINAL {
      break;
    }
  }

  return [ $label, $slabel ];
}

// Disjunctive set of label parts
fn _labelpart(self, $labelpart, $citekey, $secnum, $section, $be, $dlist) {
  let $bee = be.get_field("entrytype");
  let $dm = crate::config::get_dm();
  let $maxan = crate::Config->getblxoption($secnum, "maxalphanames", $bee, $citekey);
  let $minan = crate::Config->getblxoption($secnum, "minalphanames", $bee, $citekey);
  let $lp;
  let $slp;

  for part in ($labelpart->@*) {
    // Implement defaults not set by biblatex itself
    if part.substring_fixed_threshold.is_none() {
      part.substring_fixed_threshold = Some(1);
    }

    // Deal with various tests
    // ifnames only uses this label template part if the list it is applied to is a certain
    // length
    if (let $inc = part.ifnames) {
      let f = part.content;
      // resolve labelname
      if f == "labelname" {
        f = be.get_labelname_info().unwrap_or("");
      }
      if dm.get_fields_of_type(FieldType::List, &[DataType::Name], None).contains(f) {
        let name = be.get_field(f)
        if !name {
          continue;// just in case there is no labelname etc.
        }
        let total_names = name.count();
        let visible_names = if (total_names > maxan) {
          minan
        }
        else {
          total_names
        };

        // Deal with ifnames
        if regex_is_match!(r"^\d+$", inc) {// just a number
          if visible_names != inc {
            continue;
          }
        }
        else {// a range
          let incr = parse_range_alt(inc);
          if !defined(incr.0) {// range -x
            if visible_names > incr.1 {
              continue;
            }
          }
          else if !defined(incr.1) {// range x-
            if visible_names < incr.0 {
              continue;
            }
          }
          else {// range x-y
            if !(visible_names >= incr.0 && visible_names <= incr.1) {
              continue;
            }
          }
        }
      }
    }
    let $ret = _dispatch_label(self, part, citekey, secnum, section, be, dlist);
    lp.push_str(ret.0);
    slp.push_str(ret.1);

    // We use the first one to return something
    if ($ret->[0]) {
      if $part->{final} {
        LABEL_FINAL = true;
      }
      break;
    }
  }

  return [ $lp, $slp ];
}


// Main label dispatch method
fn _dispatch_label(&mut self, $part, citekey: &str, secnum: u32, $section, $be, $dlist) {
  let code_ref;
  let code_args_ref;
  let $lp;
  let $slp;
  let dm = crate::config::get_dm();


  // real label field
  if let Some((d0, d1)) = _dispatch_table_label(part.content, dm) {
    code_ref = d0;
    code_args_ref = d1;
  } else { // if the field is not found in the dispatch table, assume it's a literal string
    code_ref = \&_label_literal;
    code_args_ref = [part.content];
  }
  return &code_ref(self, citekey, secnum, section, be, code_args_ref, part, dlist);
}


//////////////////////////
// Label dispatch routines
//////////////////////////

fn _label_citekey(&mut self, citekey: &str, secnum: u32, $section, $be, $args, $labelattrs, dlist: &DataList) {
  let $k = _process_label_attributes(self, citekey, dlist, [[citekey, None]], $labelattrs, $args->[0]);
  return [$k, unescape_label($k)];
}

fn _label_basic(&mut self, citekey: &str, secnum: u32, $section, $be, $args, $labelattrs, dlist: &DataList) {
  let $e = $args->[0];

  let $f;
  if ($args->[1] &&
      $args->[1] == "nostrip") {
    $f = be.get_field(e);
  }
  else {
    $f = normalise_string_label(be.get_field(e));
  }
  if ($f) {
    let $b = _process_label_attributes(self, citekey, dlist, [[$f, None]], $labelattrs, $e);
    return [$b, unescape_label($b)];
  }
  else {
    return ["", ""];
  }
}

// literal string - don't post-process this, there is no point
fn _label_literal(self, _citekey: &str, _secnum: u32, _section, $be, $args, $labelattrs) {
  let $string = $args->[0];
  return [escape_label(unescape_label($string)), unescape_label($string)];
}

// names
fn _label_name(self, citekey: &str, secnum: u32, _section, be: &Entry, $args, $labelattrs, dlist: &DataList) {
  let $bee = be.get_field("entrytype");
  let $useprefix = crate::Config->getblxoption(secnum, "useprefix", bee, citekey);
  let $alphaothers = crate::Config->getblxoption(None, "alphaothers", bee);
  let $sortalphaothers = crate::Config->getblxoption(None, "sortalphaothers", $bee);

  // Get the labelalphanametemplate name or this list context
  let lantname = dlist.get_labelalphanametemplatename();

  // Override with any entry-specific information
  $lantname = crate::Config->getblxoption($secnum, "labelalphanametemplatename", None, citekey).unwrap_or($lantname);

  // Shortcut - if there is no labelname, don't do anything
  if be.get_labelname_info().is_none() {
    return ["",""];
  }

  let $namename = $args->[0];
  let $acc = "";// Must initialise to empty string as we need to return a string
  // This contains sortalphaothers instead of alphaothers, if defined
  // This is needed in cases where alphaothers is something like
  // '\textasteriskcentered' which would mess up sorting.
  let $sortacc;

  // Careful to extract the information we need about the real name behind labelname
  // as we need this to set the use* options below.
  let realname = if namename == "labelname" {
    be.get_labelname_info().unwrap()
  } else {
    namename
  };

  let names = be.get_field(realname);

  // Account for labelname set to short* when testing use* options
  let lnameopt = if realname.len() > 5 && realname.starts_with("short") {
    realname[5..].to_string()
  } else {
    realname.clone()
  };

  if (crate::Config->getblxoption(secnum, format!("use{lnameopt}"), bee, citekey) && names) {

    // namelist scope labelalphanametemplate
    if let Some(val) = names.get_labelalphanametemplatename() {
      lantname = val;
    }

    // namelist scope useprefix
    if let Some(pref) = names.get_useprefix() {
      $useprefix = pref;
    }

    let numnames = names.count();
    let visibility = dlist.get_visible_alpha(names.get_id());

    // Use name range override, if any
    let $nr_start;
    let $nr_end;
    if (exists($labelattrs->{names})) {
      let $nr = parse_range($labelattrs->{names});
      $nr_start = $nr->[0];
      $nr_end = $nr->[1];

      if (defined($nr_end) &&
          $nr_end == '+') {// minalphanames cap marker
        $nr_end = $visibility;
      }
      else if (!defined($nr_end) ||
          $nr_end > $numnames) { // cap at numnames, of course
        $nr_end = $numnames;
      }
    }
    else {
      $nr_start = 1;
      $nr_end = $visibility; // Else use bib visibility
    }

      trace!("{}/numnames={}/visibility={}/nr_start={}/nr_end={}", realname, numnames, visibility, nr_start, nr_end);

    let $parts;
    let $opts;

    for name in names.names() {
      // name scope labelalphanametemplate
      if let Some(val) = name.get_labelalphanametemplatename() {
        lantname = val;
      }

      // name scope useprefix
      if let Some(pref) = name.get_useprefix() {
        $useprefix = pref;
      }

      // In future, perhaps there will be a need for more namepart use* options and
      // therefore $opts will come from somewhere else
      $opts->{useprefix} = $useprefix;

      // Now extract the template to use from the global hash of templates
      let $lnat = crate::Config->getblxoption(None, "labelalphanametemplate")->{$lantname};

      let $preacc; // arrayref accumulator for "pre" nameparts
      let $mainacc; // arrayref accumulator for main non "pre" nameparts
      let $mpns; // arrayref accumulator for main non "pre" namepart names
      let $preopts; // arrayref accumulator for "pre" namepart options
      let $mainopts; // arrayref accumulator for main non "pre" namepart options
      for lnp in ($lnat->@*) {
        let $npn = $lnp->{namepart};
        let $np;

        if ($np = $name->get_namepart($npn)) {
          if ($lnp->{use}) { // only ever defined as 1
            if !($opts->{format!("use{npn}")}) {
              continue;
            }
          }

          if ($lnp->{pre}) {
            push $preacc->@*,
              [normalise_string_label($np),
               {substring_width => $lnp->{substring_width},
                substring_side => $lnp->{substring_side},
                substring_compound => $lnp->{substring_compound}}];
          }
          else {
            mpns.push(npn);
            push $mainacc->@*,
              [normalise_string_label($np),
               {substring_width => $lnp->{substring_width},
                substring_side => $lnp->{substring_side},
                substring_compound => $lnp->{substring_compound}}];
          }
        }
      }

      push $parts->{pre}{strings}->@*, $preacc;
      push $parts->{main}{strings}->@*, $mainacc;
      push $parts->{main}{partnames}->@*, $mpns;
    }

    // Loop over names in range
    for i in nr_start-1..nr_end {
      // Deal with pre options
      for fieldinfo in ($parts->{pre}{strings}[$i]->@*) {
        let $np = $fieldinfo->[0];
        let $npo = $fieldinfo->[1];
        let $w = npo.substring_width.unwrap_or(1);
        if (npo.substring_compound) {
          let mut tmpstring = String::new();
          // Splitting on tilde too as libbtparse inserts these into compound prefices
          for part in regex!(r"[\s\p{Dash}~]+").split(np) {
            tmpstring.extend(part.graphemes(true).take(w));
          }
          acc.push_str(tmpstring);
        } else {
          acc.extend(np.graphemes(true).take(w))
        }
      }

      acc.push_str(_process_label_attributes(self,
                                        citekey,
                                        dlist,
                                        $parts->{main}{strings}[i],
                                        $labelattrs,
                                        $realname,
                                        $parts->{main}{partnames}[i],
                                        $i)
      );

      // put in names sep, if any
      if (let $nsep = $labelattrs->{namessep}) {
        if i != nr_end - 1 {
          acc.push_str(nsep);
        }
      }
    }

    $sortacc = $acc;

    // Add alphaothers if name list is truncated unless noalphaothers is specified
    if !($labelattrs->{noalphaothers}) {
      if numnames > nr_end || names.get_morenames() {
        acc.push_str(alphaothers.unwrap_or("")); // alphaothers can be undef
        sortacc.push_str(sortalphaothers.unwrap_or("")); // sortalphaothers can be undef
      }
    }
    return [$acc, unescape_label($sortacc)];
  }
  else {
    return ["", ""];
  }
}

// Label generation utilities

// Modify label string according to some attributes
// We use different caches for the "v" and "l" schemes because they have a different format
// internally and interfere with each other between resets in prepare() otherwise

// Complicated due to various label disambiguation schemes and also due to dealing with
// name fields
fn _process_label_attributes(&mut self, citekey: &str, dlist: &DataList, $fieldstrings, $labelattrs, $field, $nameparts, $index) {
  if !($labelattrs) {
    return fieldstrings.iter().map(|s| s[0]).join("");
  }
  let $rfield_string;
  let secnum = self.get_current_section();
  let section = self.sections().get_section(secnum);
  let @citekeys = $section.get_citekeys();
  let nindex = citekeys.iter().position(|&k| k == citekey);

  for fieldinfo in ($fieldstrings->@*) {
    let $field_string = $fieldinfo->[0];
    let $namepartopts = $fieldinfo->[1];

    if (defined($labelattrs->{substring_width})) {
      // dynamically disambiguated width (individual name disambiguation)
      if ($labelattrs->{substring_width} =~ /v/ && $field) {
        // Use the cache if there is one
        if (let $lcache = $section->get_labelcache_v($field)) {
            debug!("Using label disambiguation cache (name) for '{}' in section {}", field, secnum);
          // Use the global index override if set (substring_width =~ /f/)
          $field_string = ${$lcache->{$field_string}{data}}[$lcache->{globalindices}{$field_string} || $lcache->{$field_string}{index}];
        }
        else {
          // This contains a mapping of strings to substrings of increasing lengths
          let %substr_cache = ();
          let $lcache = {};

          // Get the indices of each field (or namepart) we are dealing with
          let %indices;
          for key in &citekeys {
            if (let $f = section.bibentry(key).get_field(field)) {
              if ($nameparts) { // name field
                let nlid = f.get_id();
                for n in f.first_n_names(dlist.get_visible_alpha(nlid)) {
                  // Do strip/nosort here as that's what we also do to the field contents
                  // we will use to look up in this hash later
                  $indices{normalise_string_label(nameparts.iter().map(|np| n.get_namepart(np)).join(""), field)} = n.get_index();
                }
              }
              else {
                $indices{$f} = 0;
              }
            }
          }

          // This ends up as a flat list due to array interpolation
          let @strings = uniq keys %indices;
          // Look to the index of the longest string or the explicit max width if set
          let $maxlen = $labelattrs->{substring_width_max} || strings.iter().map(|s| s.graphemes(true).count()).max();
          for i in 1..=maxlen {
            for map in (map { let $s = Unicode::GCString->new($_)->substr(0, $i)->as_string; $substr_cache{$s}++; [$_, $s] } @strings) {
              // We construct a list of all substrings, up to the length of the longest string
              // or substring_width_max. Then we save the index of the list element which is
              // the minimal disambiguation if it's not yet defined
              push $lcache->{$map->[0]}{data}->@*, $map->[1];
              $lcache->{$map->[0]}{nameindex} = $indices{$map->[0]};
              if (!exists($lcache->{$map->[0]}{index}) &&
                  ($substr_cache{$map->[1]} == 1 || $i == $maxlen)) {
                // -1 to make it into a clean array index
                $lcache->{$map->[0]}{index} = Unicode::GCString->new($map->[1])->length - 1;
              }
            }
          }
          // We want to use a string width for all strings equal to the longest one needed
          // to disambiguate this list. We do this by saving an override for the minimal
          // disambiguation length per index
          if ($labelattrs->{substring_width} =~ /f/) {
            // Get the uniqueness indices of all of the strings and strip out those
            // which don't occur at least substring_fixed_threshold times

            let $is;
            for v in (values %$lcache) {
              $is->{$v->{nameindex}}{$v->{index}}++;
            }

            // Now set a new global index for the name part index which is the maximum of those
            // occuring above a certain threshold
            for s in lcache.keys() {
              for ind in is.keys() {
                if $indices{$s} != $ind {
                  continue;
                }
                $lcache->{globalindices}{$s} = max grep {$is->{$ind}{$_} >= $labelattrs->{substring_fixed_threshold} } keys $is->{$ind}->%*;
              }
            }
          }

          // Use the global index override if set (substring_width =~ /f/)
          $field_string = ${$lcache->{$field_string}{data}}[$lcache->{globalindices}{$field_string} || $lcache->{$field_string}{index}];
            trace!("Label disambiguation cache for '{field}' {}in section {secnum}:\n {lcache:?}", (if !nameparts.is_empty() { format!("({}) ", nameparts.join(",")) } else { "" }));
          section.set_labelcache_v(field, lcache);
        }
      }
      // dynamically disambiguated width (list disambiguation)
      else if ($labelattrs->{substring_width} =~ /l/ && $field) {
        // Use the cache if there is one
        if (let $lcache = $section->get_labelcache_l($field)) {
            debug!("Using label disambiguation cache (list) for '{}' in section {}", field, secnum);
          $field_string = $lcache->{data}[$nindex][$index];

        }
        else {
          // This retains the structure of the entries for the "l" list disambiguation
          // Have to be careful if field "$f" is not set for all entries
          let $strings = [map {let $f = section.bibentry($_).get_field(field);
                              $f ? ($nameparts ? [map {let $n = $_;join("", map {$n->get_namepart($_)} $nameparts->@*)} $f->first_n_names(dlist.get_visible_alpha(f.get_id()))->@*] : [$f]) : [""] }
                         @citekeys];
          let $lcache = _label_listdisambiguation($strings);

          $field_string = $lcache->{data}[$nindex][$index];

            trace!("Label disambiguation (list) cache for '{field}' {}in section {secnum}:\n {lcache:?}", (if !nameparts.is_empty() { format!("({}) ", nameparts.join(",")) } else { "" }));
          section.set_labelcache_l(field, lcache);
        }
      }
      // static substring width
      else {
        let mut subs_offset = 0;
        let default_substring_width = 1;
        let default_substring_side = Side::Left;
        let padchar = labelattrs.pad_char;
        let mut subs_side = labelattrs.substring_side.unwrap_or(default_substring_side);
        let mut subs_width = labelattrs.substring_width.unwrap_or(default_substring_width);

        // Override subs width with namepart specific setting, if it exists
        if ($nameparts) {
          if (let $w = namepartopts.substring_width) {
            subs_width = w;
          }
          if (let $s = namepartopts.substring_side) {
            subs_side = s;
          }
        }

        // Set offset depending on subs side
        if subs_side == Side::Right {
          subs_offset = 0 - subs_width;
        }

        // Get map of regexps to not count against string width and record their place in the
        // string
        let $nolabelwcs = crate::Config->getoption("nolabelwidthcount");
        let $nolabelwcis;
        if ($nolabelwcs) {
          $nolabelwcis = match_indices([map {$_->{value}} $nolabelwcs->@*], $field_string);
          trace!("Saved indices for nolabelwidthcount: {}", Data::Dump::pp($nolabelwcis));
          // Then remove the nolabelwidthcount chars for now
          for nolabelwc in ($nolabelwcs->@*) {
            let $nlwcopt = $nolabelwc->{value};
            let $re = qr/$nlwcopt/;
            // remove nolabelwidthcount items
            field_string = regex_xms(&format!(r"{re}")).replace_all(&field_string, "");
          }
        }

        // If desired, do the substring on all parts of compound names
        // (with internal spaces or hyphens)
        if ($nameparts && $namepartopts->{substring_compound}) {
          let mut tmpstring = String::new();
          for part in regex!(r"[\s\p{Dash}]+").split(&field_string) {
            tmpstring.extend(part.graphemes(true).skip(subs_offset).take(subs_width));
          }
          field_string = tmpstring;
        }
        else {
          field_string = field_string.graphemes(true).skip(subs_offset).take(subs_width).collect();
        }
        // Padding
        if ($padchar) {
          padchar = unescape_label(padchar);
          let pad_side = labelattrs.pad_side.unwrap_or(Side::Right);
          let paddiff = subs_width - field_string.graphemes(true).count();
          if paddiff > 0 {
            match pad_side {
              Side::Right => field_string.push_str(padchar.repeat(paddiff)),
              Side::Left => field_string = format!("{}{field_string}", padchar.repeat(paddiff)),
            }
          }
          field_string = escape_label(&field_string);
        }

        // Now reinstate any nolabelwidthcount regexps
        if ($nolabelwcis) {
          let $gc_string = Unicode::GCString->new($field_string);
          for nolabelwci in ($nolabelwcis->@*) {
            // Don't put back anything at positions which are no longer in the string
            if (nolabelwci[1] + 1 <= $gc_string->length) {
              $gc_string->substr(nolabelwci[1], 0, nolabelwci[0]);
            }
          }
          $field_string = $gc_string->as_string;
        }
      }
    }
    rfield_string.push_str(field_string);
  }

  // Case changes
  if labelattrs.uppercase && labelattrs.lowercase {
    // do nothing if both are set, for sanity
  } else if labelattrs.uppercase {
    rfield_string = rfield_string.to_uppercase();
  } else if labelattrs.lowercase {
    rfield_string = rfield_string.to_lowercase();
  }

  rfield_string
}

// This turns a list of label strings:
// [
//  ["Agassi", "Chang",   "Laver", "bob"],
//  ["Agassi", "Chang",   "Laver"],
//  ["Agassi", "Chang",   "Laver"],
//  ["Agassi", "Connors", "Lendl"],
//  ["Agassi", "Courier", "Laver"],
//  ["Borg",   "Connors", "Edberg"],
//  ["Borg",   "Connors", "Emerson"],
//  ["Becker", "Connors", "Emerson"],
//  ["Becker"]
//  ["Zoo", "Xaa"],
//  ["Zoo", "Xaa"],
//  ["Zaa"],
//  ["Abc", "Abc", "Abc"],
//  ["Abc", "Abc", "Abc"],
//  ["Abc", "Abc", "Abc"]
// ]
//
//
// into a disambiguated list of substrings:
//
// { data => [
//            ["A",  "C",  "L",  "b"],
//            ["A",  "Ch", "L"      ],
//            ["A",  "Ch", "L"      ],
//            ["A",  "Co", "L"      ],
//            ["A",  "C",  "L"      ],
//            ["B",  "C",  "Ed"     ],
//            ["Bo", "C",  "E"      ],
//            ["B",  "C",  "E"      ],
//            ["B"                  ]
//            ["Z"   "X"            ]
//            ["Z"   "X"            ]
//            ["Z"                  ]
//            ["A",  "A",  "A"      ]
//            ["A",  "A",  "A"      ]
//            ["A",  "A",  "A"      ]
//           ],
// }
//

fn _label_listdisambiguation(strings) {
  // Cache map says which index are we substr'ing to for each name.
  // Starting default is first char from each
  let $cache->{substr_map} = [map {[map {1} $_->@*]} $strings->@*];
  let lcache->{data} = vec![None; strings.len()];

  // First flag any duplicates so we can shortcut setting these later
  let mut dups = Vec::new();
  for (i, s) in strings.iter().enumerate() {
    dups.push(s.join(""));
  }

  _do_substr($lcache, $cache, $strings);

  // loop until the entire disambiguation cache is filled.
  while (grep { !defined } $lcache->{data}->@*) {
    _check_counts($lcache, $cache);
    for ambiguous_indices in ($cache->{ambiguity}->@*) {
      let $ambiguous_strings = [$strings->@[$ambiguous_indices->@*]]; // slice
      // We work on the first in an ambiguous set
      // We have to find the first name which is not the same as another name in the
      // same position as we can't disambiguate on the basis of an identical name. For example:
      // [
      //   [ "Smith", "Jones" ]
      //   [ "Smith", "Janes" ]
      // ]
      //
      // Here there is no point trying more characters in "Smith" as it won't help

      // Special case: If all lists in an ambiguity set are identical, like
      //
      // [
      //  [ "Smith", "Jones" ],
      //  [ "Smith", "Jones" ],
      // ]
      //
      // Then we can shortcut and take a 1-char substring only
      // if all name lists in the ambiguous list are in fact the same
      if (all {Compare($ambiguous_strings->[0], $_)} $ambiguous_strings->@*) {
        $lcache->{data}[$ambiguous_indices->[0]] =  [map {Unicode::GCString->new($_)->substr(0,1)->as_string} $ambiguous_strings->[0]->@*];
      }
      else {
        // Get disambiguating list position information
        _gen_first_disambiguating_name_map($cache, $ambiguous_strings, $ambiguous_indices);

        // Then increment appropriate substr map
        $cache->{substr_map}[$ambiguous_indices->[0]][$cache->{name_map}[$ambiguous_indices->[0]]]++;
      }

      // Rebuild the cache and loop
      _do_substr($lcache, $cache, $strings);
    }
  }

  return $lcache;
}

// Take substrings of name lists according to a map and save the results
fn _do_substr(lcache, cache, strings) {
  delete($cache->{keys});
  for (i, row) in strings.iter().enumerate() {
    if lcache->{data}[i].is_some() { // ignore names already disambiguated
      continue;
    }
    let mut s = Vec::new();
    for (j, rowj) in row.iter().enumerate() {
      s.push(Unicode::GCString->new(rowj)->substr(0 ,$cache->{substr_map}[i][j])->as_string);
    }
    let js = s.join("");
    $cache->{keys}{$js}{index} = $i; // index of the last seen $js key - useless for count >1
    push $cache->{keys}{$js}{indices}->@*, $i;
    $cache->{keys}{$js}{count}++;
    $cache->{keys}{$js}{strings} = \@s;
  }
}

// Push finished disambiguation into results and save still ambiguous labels for loop
fn _check_counts(lcache, cache) {
  delete($cache->{ambiguity});
  for key in (keys $cache->{keys}->%*) {
    if ($cache->{keys}{$key}{count} > 1) {
      push $cache->{ambiguity}->@*, $cache->{keys}{$key}{indices};
    }
    else {
      $lcache->{data}[$cache->{keys}{$key}{index}] = $cache->{keys}{$key}{strings};
    }
  }
}

// Find the index of the first name in $array->[0] which doesn't
// occur in any other of $array in the same position. This must be the name
// which disambiguates.

// [
//  ["Agassi", "Chang",   "Laver"],
//  ["Agassi", "Chang",   "Laver"],
//  ["Agassi", "Connors", "Lendl"],
//  ["Agassi", "Courier", "Laver"],
//  ["Agassi", "Courier", "Lendl"],
// ]

// results in

// $cache->{name_map} = [ 1, 1, 1, 1, 2 ]
fn _gen_first_disambiguating_name_map(cache, array, indices) {
  for (i, arrayi) in array.iter().enumerate() {
    let @check_array = $array->@*;
    splice(@check_array, $i, 1);
    // Remove duplicates from the check array otherwise the duplicate makes generating the
    // name disambiguation index fail because there is a same name in every position
    @check_array = grep {not Compare(arrayi, $_)} @check_array;
    // all ambiguous must be same length (otherwise they wouldn't be ambiguous)
    for j in 0..array[0].len() {
      // if no other name equal to this one in same place, this is the index of the name
      // to use for disambiguation
      if !(grep {arrayi[j] == $_} map {$_->[$j]} @check_array) {
        $cache->{name_map}[$indices->[$i]] = $j;
        break;
      }
    }
  }
}

//////////
// Sorting
//////////

// None of these can be used to generate sorting information otherwise there
// would be a circular dependency:

// sortinit
// sortinithash
// extradate
// extratitle
// extratitleyear
// extraalpha

let $sorting_sep = ',';

// special sorting routines - not part of the dm but special fields for biblatex
let %internal_dispatch_sorting = (
                                 "editoratype"     =>  [\&_sort_editort,       ["editoratype"]],
                                 "editorbtype"     =>  [\&_sort_editort,       ["editorbtype"]],
                                 "editorctype"     =>  [\&_sort_editort,       ["editorctype"]],
                                 "citeorder"       =>  [\&_sort_citeorder,     []],
                                 "citecount"       =>  [\&_sort_citecount,     []],
                                 "intciteorder"    =>  [\&_sort_intciteorder,  []],
                                 "labelalpha"      =>  [\&_sort_labelalpha,    []],
                                 "labelname"       =>  [\&_sort_labelname,     []],
                                 "labeltitle"      =>  [\&_sort_labeltitle,    []],
                                 "labelyear"       =>  [\&_sort_labeldate,     ["year"]],
                                 "labelmonth"      =>  [\&_sort_labeldate,     ["month"]],
                                 "labelday"        =>  [\&_sort_labeldate,     ["day"]],
                                 "presort"         =>  [\&_sort_presort,       []],
                                 "sortname"        =>  [\&_sort_sortname,      []],
                                 "entrytype"       =>  [\&_sort_entrytype,     []],
                                 "entrykey"        =>  [\&_sort_entrykey,      []]);

// The value is an array pointer, first element is a code pointer, second is
// a pointer to extra arguments to the code. This is to make code re-use possible
// so the sorting can share code for similar things.
fn _dispatch_table_sorting(field, dm) {
  // internal fields not part of the data model
  if (let $id = $internal_dispatch_sorting{$field}) {
    return $id;
  }
  // Sorting elements which aren't fields
  if !dm.is_field(field) {
    return None;
  }
  // Fields which are part of the datamodel
  let dmf = dm.get_dm_for_field(field);
  if let (Some(fieldtype), Some(datatype)) = (dmf.fieldtype, dmf.datatype) {
    match (fieldtype, datatype) {
      (FieldType::List, DataType::Name) => {
        return [\&_sort_name, [$field]];
      }
      (_, DataType::Verbatim | DataType::Uri) => { // NOTE: Should be FieldType::Field?
        return [\&_sort_verbatim, [$field]];
      }
      (FieldType::Field, DataType::Literal) => {
        return [\&_sort_literal, [$field]];
      }
      (FieldType::Field, DataType::Integer | DataType::Datepart) => {
        return [\&_sort_integer, [$field]];
      }
      (FieldType::List, DataType::Literal | DataType::Key) => {
        return [\&_sort_list, [$field]];
      }
      (FieldType::List, DataType::Verbatim | DataType::Uri) => {
        return [\&_sort_list_verbatim, [$field]];
      }
      (FieldType::Field, DataType::Key) => {
        return [\&_sort_literal, [$field]];
      }
      _ => {}
    }
  }
}

// Main sorting dispatch method
fn _dispatch_sorting(self, sortfield: &str, citekey: &str, secnum: u32, section: &Section, $be, $dlist, $sortelementattributes) {
  let $code_ref;
  let $code_args_ref;
  let $dm = crate::config::get_dm();

  // If this field is excluded from sorting for this entrytype, then skip it and return
  if (let $se = crate::Config->getblxoption(None, "sortexclusion", be.get_field("entrytype"))) {
    if ($se->{$sortfield}) {
      return "";
    }
  }
  // If this field is excluded from sorting for all entrytypes, then include it if it's
  // explicitly included
  if (let $se = crate::Config->getblxoption(None, "sortexclusion", '*')) {
    if ($se->{$sortfield}) {
      if (let $si = crate::Config->getblxoption(None, "sortinclusion", be.get_field("entrytype"))) {
        if !($si->{$sortfield}) {
          return "";
        }
      }
      else {
        return "";
      }
    }
  }

  // if the field is a literal string, use it
  if ($sortelementattributes->{literal}) {
    $code_ref = \&_sort_string;
    $code_args_ref = [$sortfield];
  }
  // real sorting field
  else if (let $d = _dispatch_table_sorting($sortfield, $dm)) {
    $code_ref = $d->[0];
    $code_args_ref  = $d->[1];
  }
  else { // Unknown field
    biber_warn("Field '$sortfield' in sorting template is not a sortable field");
    return None;
  }

  return &{$code_ref}($self, $citekey, $secnum, $section, $be, $dlist, $sortelementattributes, $code_args_ref);
}

// Conjunctive set of sorting sets
fn _generatesortinfo(self, citekey: &str, $dlist) {
  let $sortingtemplate = $dlist->get_sortingtemplate;
  let secnum = self.get_current_section();
  let section = self.sections().get_section(secnum);
  let be = section.bibentry(citekey);
  let $sortobj;
  let $szero = 0;

  $BIBER_SORT_NULL = 0;
  $BIBER_SORT_FINAL = "";
  $BIBER_SUPPRESS_FINAL = 1;

  for sortset in ($sortingtemplate->{spec}->@*) {
    let $s = $self->_sortset(sortset, citekey, secnum, section, be, dlist);

    // Did we get a real zero? This messes up tests below unless we are careful
    // Don't try and make this more implicit, it is too subtle a problem
    if ($s == "BIBERZERO") {
      $szero = 1;
      $s = 0;
    }

    // We have already found a "final" item so if this item returns null,
    // copy in the "final" item string as it's the master key for this entry now
    // (but suppress this when the final item is found so that entries without
    //  the final item don't always sort before entries with the final item)
    // This means that final items are always blank in all sort keys across all entries
    // and so have no impact until later sort items where the final item becomes the
    // sorting key for every subsequent sorting item.
    if (let $f = $BIBER_SORT_FINAL) {
      push $sortobj->@*, ($BIBER_SUPPRESS_FINAL ? "" : $f);
      $BIBER_SUPPRESS_FINAL = 0;
    }
    else {
      push $sortobj->@*, $s;
    }
  }

  // Record the information needed for sorting later
  // sortstring isn't actually used to sort, it's used to generate sortinit and
  // for debugging purposes
  let $ss = sortobj.join(&sorting_sep);
  $dlist->set_sortdata($citekey, [$ss, $sortobj]);
    debug!("Sorting object for key '{}' -> {}", citekey, Data::Dump::pp($sortobj));

  // Generate sortinit. Skip if there is no sortstring, which is possible in tests
  if ($ss || $szero) {
    // This must ignore the presort characters, naturally
    let $pre = crate::Config->getblxoption(secnum, "presort", be.get_field("entrytype"), citekey);

    // Strip off the prefix
    $ss =~ s/\A$pre$sorting_sep+//;
    let $init = Unicode::GCString->new(normalise_string($ss))->substr(0, 1)->as_string;
    $dlist->set_sortinitdata_for_key($citekey, $init);
  }
  return;
}

// Process sorting set
fn _sortset(self, $sortset, citekey: &str, secnum: u32, $section, $be, $dlist) {
  let $dm = crate::config::get_dm();
  for sortelement in ($sortset->@[1..$sortset->$#*]) {
    let ($sortelementname, $sortelementattributes) = %$sortelement;
    $BIBER_SORT_NULL = 0; // reset this per sortset
    let $out = self._dispatch_sorting(sortelementname, citekey, secnum, section, be, dlist, sortelementattributes);
    if ($out) { // sort returns something for this key
      if ($sortset->[0]{final}) {
        // If we encounter a "final" element, we return an empty sort
        // string and save the string so it can be copied into all further
        // fields as this is now the master sort key. We use an empty string
        // where we found it in order to preserve sort field order and so
        // that we sort correctly against all other entries without a value
        // for this "final" field
        $BIBER_SORT_FINAL = $out;
        break;
      }
      return $out;
    }
  }
  $BIBER_SORT_NULL = 1; // set null flag - need this to deal with some cases
  return "";
}

//////////////////////////////////////////////
// Sort dispatch routines
//////////////////////////////////////////////

fn _sort_citeorder(self, $citekey, $secnum, $section, $be, $dlist, $sortelementattributes) {
  // Allkeys and sorting=none means use bib order which is in orig_order_citekeys
  // However, someone might do:
  // \cite{b}\cite{a}
  // \nocite{*}
  // in the same section which means we need to use the order attribute for those
  // keys which have one (the \cited keys) and then an orig_order_citekey index based index
  // for the nocite ones.
  let $ko = crate::Config->get_keyorder($secnum, $citekey);// only for \cited keys
  if section.is_allkeys() {
    let $biborder = (crate::Config->get_keyorder_max($secnum) +
                    (first_index {$_ == $citekey} $section->get_orig_order_citekeys) + 1);
    let $allkeysorder = crate::Config->get_keyorder($secnum, '*');
    if (defined($ko) && defined($allkeysorder) && $allkeysorder < $ko) {
      return $biborder;
    }
    else {
      return $ko || $biborder;
    }
  }
  // otherwise, we need to take account of citations with simulataneous order like
  // \cite{key1, key2} so this tied sorting order can be further sorted with other fields
  // Note the fallback of "" - this is for auto-generated entries which are not cited
  // and so never have a keyorder entry
  else {
    return $ko || "";
  }
}

fn _sort_citecount(self, $citekey, $secnum, $section, $be, $dlist, $sortelementattributes) {
  return $section->get_citecount($citekey).unwrap_or("");
}

fn _sort_integer(self, $citekey, $secnum, $section, $be, $dlist, $sortelementattributes, $args) {
  let $dmtype = $args->[0]; // get int field type
  let bee = be.get_field("entrytype");
  if (let $field = be.get_field($dmtype)) {

    // Make an attempt to map roman numerals to integers for sorting unless suppressed
    if (isroman(NFKD(field)) &&
        !crate::Config->getblxoption(secnum, "noroman", be.get_field("entrytype"), citekey)) {
      $field = roman2int(NFKD(field));
    }

    // Use Unicode::UCD::num() to map Unicode numbers to integers if possible
    $field = num($field).unwrap_or($field);

    return _process_sort_attributes($field, $sortelementattributes);
  }
  else {
    return "";
  }
}

fn _sort_editort(self, $citekey, $secnum, $section, $be, $dlist, $sortelementattributes, $args) {
  let $edtypeclass = $args->[0]; // get editor type/class field
  if (crate::Config->getblxoption($secnum, "useeditor", be.get_field("entrytype"), citekey) &&
    be.get_field($edtypeclass)) {
    let $string = be.get_field($edtypeclass);
    return _translit($edtypeclass, $be, _process_sort_attributes($string, $sortelementattributes));
  }
  else {
    return "";
  }
}

fn _sort_entrykey(self, $citekey, $secnum, $section, $be, $dlist, $sortelementattributes) {
  return _process_sort_attributes($citekey, $sortelementattributes);
}

fn _sort_entrytype(self, $citekey, $secnum, $section, $be, $dlist, $sortelementattributes) {
  return _process_sort_attributes(be.get_field("entrytype"), $sortelementattributes);
}

fn _sort_intciteorder(self, $citekey, $secnum, $section, $be, $dlist, $sortelementattributes) {
  return Biber::Config->get_internal_keyorder($secnum, $citekey);
}

fn _sort_labelalpha(self, $citekey, $secnum, $section, $be, $dlist, $sortelementattributes, $args) {
  let string = dlist.get_entryfield(citekey, "sortlabelalpha").unwrap_or("");
  return _process_sort_attributes(string, sortelementattributes);
}

fn _sort_labelname(self, citekey, $secnum, $section, $be, $dlist, $sortelementattributes, $args) {
  // re-direct to the right sorting routine for the labelname
  if let Some(lni) = be.get_labelname_info().skip_empty() {
    // Don't process attributes as they will be processed in the real sub
    return self._dispatch_sorting(lni, citekey, secnum, section, be, dlist, sortelementattributes);
  }
  else {
    return "";
  }
}

fn _sort_labeltitle(self, citekey: &str, secnum: u32, section: &Section, be: &Entry, dlist, sortelementattributes, args) {
  // re-direct to the right sorting routine for the labeltitle
  if (let $lti = be.get_labeltitle_info()) {
    // Don't process attributes as they will be processed in the real sub
    return $self->_dispatch_sorting($lti, citekey, secnum, section, be, dlist, sortelementattributes);
  }
  else {
    return "";
  }
}

fn _sort_labeldate(self, citekey: &str, secnum: u32, section: &Section, be: &Entry, dlist, sortelementattributes, args) {
  no autovivification;
  let $ldc = $args->[0]; // labeldate component
  // re-direct to the right sorting routine for the labeldate component
  if let Some(LabelDateInfo::Field(field)) be.get_labeldate_info() {
    if (let $ldf = field{$ldc}) {
      // Don't process attributes as they will be processed in the real sub
      return self._dispatch_sorting(ldf, citekey, secnum, section, be, dlist, sortelementattributes);
    } else {
      ""
    }
  } else {
    ""
  }
}

// This is a meta-sub which uses the optional arguments to the dispatch code
// It's done to avoid having many repetitions of almost identical sorting code
fn _sort_list(self, citekey, secnum, section, be, dlist, sortelementattributes, args) {
  let $list = $args->[0]; // get list field
  if (be.get_field(list)) {
    let string = self._liststring(citekey, list);
    return _translit(list, be, _process_sort_attributes(string, sortelementattributes));
  }
  else {
    return "";
  }
}

// This is a meta-sub which uses the optional arguments to the dispatch code
// It's done to avoid having many repetitions of almost identical sorting code
fn _sort_list_verbatim(self, citekey: &str, secnum: u32, section: &Section, be: &Entry, dlist, sortelementattributes, args) {
  let $list = $args->[0]; // get list field
  if (be.get_field(list)) {
    let string = self._liststring(citekey, list, 1);
    return _process_sort_attributes(string, sortelementattributes);
  }
  else {
    return "";
  }
}

// This is a meta-sub which uses the optional arguments to the dispatch code
// It's done to avoid having many repetitions of almost identical sorting code
// for literal strings which need normalising
fn _sort_literal(self, citekey: &str, secnum: u32, section: &Section, be: &Entry, dlist, sortelementattributes, args) {
  let $literal = $args->[0]; // get actual field
  if (let $field = be.get_field(literal)) {
    let string = normalise_string_sort(field, literal);
    return _translit(literal, be, _process_sort_attributes(string, sortelementattributes));
  }
  else {
    return "";
  }
}

// This is a meta-sub which uses the optional arguments to the dispatch code
// It's done to avoid having many repetitions of almost identical sorting code
// for literal strings which need no normalising/translit. Nosort is still honoured.
fn _sort_verbatim(self, citekey: &str, secnum: u32, section: &Section, be: &Entry, dlist, sortelementattributes, args) {
  let $literal = $args->[0]; // get actual field
  if (let $field = be.get_field(literal)) {
    let string = strip_nosort(field, literal);
    return _process_sort_attributes(field, sortelementattributes);
  }
  else {
    return "";
  }
}

// This is a meta-sub which uses the optional arguments to the dispatch code
// It's done to avoid having many repetitions of almost identical sorting code
// for the editor roles
fn _sort_name(self, citekey: &str, secnum: u32, section: &Section, be: &Entry, dlist, sortelementattributes, args) {
  let $name = $args->[0]; // get name field name
  // If there is a biblatex option which controls the use of this name, check it
  if CONFIG_OPT_SCOPE_BIBLATEX.contains_left(&format!("use{name}")) &&
      !crate::Config->getblxoption(secnum, format!("use{name}"), be.get_field("entrytype"), citekey) {
    return "";
    }
  if (be.get_field(name)) {
    let string = self._namestring(citekey, name, dlist);
    return _translit(name, be, _process_sort_attributes(string, sortelementattributes));
  }
  else {
    return "";
  }
}

fn _sort_presort(self, citekey: &str, secnum: u32, section: &Section, be: &Entry, dlist, sortelementattributes) {
  let string = crate::Config->getblxoption(secnum, "presort", be.get_field("entrytype"), citekey);
  return _process_sort_attributes(string, sortelementattributes);
}

fn _sort_sortname(self, citekey: &str, secnum: u32, section: &Section, be: &Entry, dlist, sortelementattributes) {
  let $dm = crate::config::get_dm();

  // sortname is ignored if no use<name> option is defined - see biblatex manual
  if (be.get_field("sortname") &&
  dm.get_fields_of_type(FieldType::List, &[DataType::Name], None).iter().filter(|f| crate::Config->getblxoption(secnum, format!("use{f}"), be.get_field("entrytype"), citekey))) {
    let string = self._namestring(citekey, "sortname", dlist);
    return _translit("sortname", be, _process_sort_attributes(string, sortelementattributes));
  }
  else {
    return "";
  }
}

fn _sort_string(self, citekey: &str, secnum: u32, section: &Section, be: &Entry, dlist, sortelementattributes, args) {
  let $string = $args->[0]; // get literal string
  return _process_sort_attributes(string, sortelementattributes);
}

//========================================================
// Utility subs used elsewhere but relying on sorting code
//========================================================

fn _process_sort_attributes(field_string: &str, sortelementattributes) -> String {
  let mut field_string = field_string.to_string();
  if field_string == "0" {
    return "BIBERZERO".into(); // preserve real zeros
  }
  if !($sortelementattributes) {
    return field_string;
  }
  if !($field_string) {
    return field_string;
  }
  // process substring
  if sortelementattributes.substring_width.is_some() ||
      sortelementattributes.substring_side.is_some() {
    let mut subs_offset = 0;
    let default_substring_width = 4;
    let default_substring_side = Side::Left;
    let subs_width = sortelementattributes.substring_width.unwrap_or(default_substring_width);
    let subs_side = sortelementattributes.substring_side.unwrap_or(default_substring_side);
    if subs_side == Side::Right {
      subs_offset = 0 - subs_width;
    }
    field_string = field_string.graphemes(true).skip(subs_offset).take(subs_width).collect();
  }
  // Process padding
  if (sortelementattributes.pad_side.is_some() ||
      sortelementattributes.pad_width.is_some() ||
      sortelementattributes.pad_char.is_some()) {
    let default_pad_width = 4;
    let default_pad_side = Side::Left;
    let default_pad_char = '0';
    let pad_width = sortelementattributes.pad_width.unwrap_or(default_pad_width);
    let pad_side = sortelementattributes.pad_side.unwrap_or(default_pad_side);
    let pad_char = sortelementattributes.pad_char.unwrap_or(default_pad_char);
    let pad_length = pad_width - field_string.graphemes(true).count();
    if pad_length > 0 {
      match pad_side {
        Side::Left => field_string = format!("{}{field_string}", pad_char.repeat(pad_length)),
        Side::Right => field_string.push_str(pad_char.repeat(pad_length)),
      }
    }
  }
  field_string
}

// This is used to generate sorting string for names
fn _namestring(self, citekey: &str, field: &str, dlist: &DataList) {
  let secnum = self.get_current_section();
  let section = self.sections().get_section(secnum);
  let be = section.bibentry(citekey);
  let bee = be.get_field("entrytype");
  let names = be.get_field(field);
  let string = String::new();
  let count = names.count();
  let useprefix = crate::Config->getblxoption(secnum, "useprefix", bee, citekey);

  // Get the sorting name key template for this list context
  let snkname = dlist.get_sortingnamekeytemplatename();

  // Override with any entry-specific sorting name key template option
  let snkname = crate::Config->getblxoption(secnum, "sortingnamekeytemplatename", None, citekey).unwrap_or(snkname);

  // Override with any namelist scope sorting name key template option
  let mut snkname = names.get_sortingnamekeytemplatename().unwrap_or(snkname);

  // Get the sorting namekey template determined so far now that we are down to the name list
  // scope since we need the visibility type now and this doesn't mean anything below the name list
  // level anyway. We will select the final sorting namekey template below if there is an override
  // at the individual name level
  let $tmpsnk = crate::Config->getblxoption(None, "sortingnamekeytemplate")->{$snkname};
  // Now set visibility of the correct type. By default this is the standard
  // sorting visibility but can be citation visibility as the biblatex
  // "sortcites" option can require a different visibility for citations and
  // so we have to generate a separate sorting list for this case
  let visible = dlist.get_visible_sort(names.get_id());
  if let Some("cite") = tmpsnk.as_deref() {
    visible = dlist.get_visible_cite(names.get_id());
  }

  // Name list scope useprefix option
  if let Some(pref) = names.get_useprefix() {
    useprefix = pref;
  }

  let trunc = '\x{10FFFD}';  // sort string for "et al" truncated name

  for n in &names.first_n_names(visible) {

    // Name scope useprefix option
    if let Some(pref) = n.get_useprefix() {
      useprefix = pref;
    }

    // Override with any name scope sorting name key template option
    // This won't override the visibility type selection already taken from higher-level
    // sorting namekey templates since this option only applies at name list level and higher
    // anyway and this is individual name scope
    snkname = n.get_sortingnamekeytemplatename.unwrap_or(snkname);

    // Now get the actual sorting name key template
    let $snk = crate::Config->getblxoption(None, "sortingnamekeytemplate")->{$snkname};

    // Get the sorting name key specification and use it to construct a sorting key for each name
    let mut kpa = Vec::new();
    for kp in ($snk->{template}->@*) {
      let mut kps = String::new();
      for (i, np) in kp.iter().enumerate() {
        if ($np->{type} == "namepart") {
          let $namepart = $np->{value};
          let useopt = if exists($np->{use}) { Some(format!("use{namepart}")) } else { None };
          let $useoptval = crate::Config->getblxoption(secnum, useopt, bee, citekey);

          // useprefix can be name list or name local
          if let Some("useprefix") = useopt.as_deref() {
            $useoptval = map_boolean("useprefix", useprefix, "tonum");
          }

          if (let $npstring = n.get_namepart(namepart)) {
            // No use attribute conditionals or the attribute is specified and matches the option

            if (!$useopt ||
                ($useopt && $useoptval == $np->{use})) {

              let mut nps;
              // Do we only want initials for sorting?
              if ($np->{inits}) {
                let npistring = n.get_namepart_initial(namepart);

                // The namepart is padded to the longest namepart in the ref
                // section as this is the only way to make sorting work
                // properly The padding is spaces as this sorts before all
                // glyphs but it also of variable weight and ignorable in
                // DUCET so we have to set U::C to variable=>"non-ignorable"
                // as sorting default so that spaces are non-ignorable
                nps = normalise_string_sort(npistring.join(""), field);

                // Only pad the last namepart
                if i == kp.len()-1 {
                  nps = sprintf("%-*s", section.get_np_length("${namepart}-i"), nps);
                }
              } else {
                nps = normalise_string_sort(npstring, field);

                // Only pad the last namepart
                if i == kp.len()-1 {
                  nps = sprintf("%-*s", section.get_np_length(namepart), nps);
                }
              }
              kps.push_str(nps);
            }
          }
        }
        else if ($np->{type} == "literal") {
          kps.push_str(np->{value});
        }
      }
      // Now append the key part string if the string is not empty
      if !kps.is_empty() {
        string.push_str(kps);
      }
      kpa.push(kps);
    }
  }

  let $nso = crate::Config->getblxoption(secnum, "nosortothers", bee, citekey);

  // Per-namelist nosortothers
  if let Some(val) = names.get_nosortothers {
    $nso = val;
  }

  if !($nso) {
    if $visible < count {
      string.push(trunc); // name list was truncated
    }
  }

  return string;

}

fn _liststring(self, citekey: &str, field: &str, $verbatim) {
  let secnum = self.get_current_section();
  let section = self.sections().get_section(secnum);
  let be = section.bibentry(citekey);
  let bee = be.get_field("entrytype");
  let f = match be.get_field(field) { // _liststring is used in tests so there has to be
    None => return "",   // more error checking which will never be needed in normal use
    Some(f) => f,
  };
  let @items = $f->@*;
  let mut truncated = false;

  // These should be symbols which can't appear in lists and which sort before all alphanum
  // so that "Alan Smith" sorts after "Al Smith". This means, symbols which normalise_string_sort()
  // strips out. Unfortuately, this means using punctuation and these are by default variable
  // weight and ignorable in DUCET so we have to redefine these these symbols after loading DUCET
  // when sorting so that they are non-ignorable (see Biber.pm)
  let lsi    = '!';          // list separator, internal
  // Guaranteed to sort after everything else as it's the last legal Unicode code point
  let trunc = '\x{10FFFD}';  // sort string for truncated list

  // perform truncation according to options minitems, maxitems
  if ( items.len() > crate::Config->getblxoption($secnum, "maxitems", bee, citekey) ) {
    truncated = true;
    @items = splice(@items, 0, crate::Config->getblxoption($secnum, "minitems", bee, citekey) );
  }

  // separate the items by a string to give some structure
  // We strip nosort first otherwise normalise_string_sort damages diacritics
  // We strip each individual component instead of the whole thing so we can use
  // as name separators things which would otherwise be stripped. This way we
  // guarantee that the separators are never in names
  let s = if ($verbatim) { // no normalisation for verbatim/uri fields
    items.iter().map(|i| strip_nosort(i, field)).join(lsi)
  }
  else {
    items.iter().map(|i| normalise_string_sort(i, field)).join(lsi)
  };

  s = regex!(r"\s+\z"xms).replace(&s, "");
  if truncated {
    s.push(trunc);
  }
  s
}

// transliterate if requested
fn _translit(target, entry, string) {
  let $entrytype = entry.get_field("entrytype");
  if (let $translits = crate::Config->getblxoption(None, "translit", $entrytype)) {
    for tr in ($translits->@*) {
      // Translit is specific to particular langids
      if let Some(val) = tr.langids {
        let langid = entry.get_field("langid");
        if !langid {
          continue;
        }
        if !regex!(r"\s*,\s*").split(val).any(|v| unicase::eq(langid, v)) {
          continue;
        }
      }
      if (tr.target.to_lowercase() == '*' ||
          tr.target == $target ||
          DATAFIELD_SETS{tr.target}.contains(target)) {
        return call_transliterator($target, tr.from, tr.to, $string);
      }
    }
  }
  return $string;
}
