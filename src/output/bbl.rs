use parent qw(crate::Output::base);

use crate::Annotation;
use crate::Config;
use crate::Constants;
use crate::Entry;
use crate::Utils;
use Encode;
use List::AllUtils qw( :all );
use IO::File;
use Log::Log4perl qw( :no_extra_logdie_message );
use Scalar::Util qw(looks_like_number);
use Text::Wrap;
use Unicode::Normalize;
use URI;
$Text::Wrap::columns = 80;

/// Class for Biber output of .bbl
pub struct Bbl;

/// Initialize a crate::Output::bbl object
fn new(obj) {
  let $self = $class->SUPER::new($obj);

  $self->{output_data}{HEAD} = <<~EOF;
    % \$ biblatex auxiliary file \$
    % \$ biblatex bbl format version $crate::Config::BBL_VERSION \$
    % Do not modify the above lines!
    %
    % This is an auxiliary file used by the 'biblatex' package.
    % This file may safely be deleted. It will be recreated by
    % biber as required.
    %
    \\begingroup
    \\makeatletter
    \\\@ifundefined{ver\@biblatex.sty}
      {\\\@latex\@error
         {Missing 'biblatex' package}
         {The bibliography requires the 'biblatex' package.}
          \\aftergroup\\endinput}
      {}
    \\endgroup

    EOF
  return $self;
}

/// Create the output for misc bits and pieces like preamble and closing
/// macro call and add to output object.
fn create_output_misc(&mut self) {

  if (let $pa = $crate::MASTER->get_preamble) {
    $pa = join("%\n", $pa->@*);

    // If requested to convert UTF-8 to macros ...
    if (crate::Config->getoption("output_safechars")) {
      $pa = latex_recode_output($pa);
    }
    else {           // ... or, check for encoding problems and force macros
      let $outenc = crate::Config->getoption("output_encoding");
      if ($outenc != "UTF-8") {
        // Can this entry be represented in the output encoding?
        if (encode($outenc, NFC($pa), sub {"\0"}) =~ /\0/) { // Malformed data encoding char
          // So convert to macro
          $pa = latex_recode_output($pa);
        }
      }
    }
    $self->{output_data}{HEAD} .= "\\preamble{%\n$pa%\n}\n\n";
  }
  $self->{output_data}{TAIL} .= "\\endinput\n\n";
  return;
}

/// Add the .bbl for a text field to the output accumulator.
fn _printfield(be, field, $str) {
  let $field_type = "field";
  let $dm = crate::config::get_dm();

  let $outfield = $dm->get_outcase($field);

  if is_null($str) && !$dm->field_is_nullok($field) {
    return "";
  }

  // crossref and xref are of type "strng" in the .bbl
  if (lc($field) == "crossref" ||
      lc($field) == "xref") {
    $field_type = "strng";
  }

  // Output absolute astronomical year by default (with year 0)
  // biblatex will adjust the years when printed with BCE/CE eras
  if ($field =~ m/^(.*)(?!end)year$/) {
    if (let $y = $be->get_field("$1year")) {
      if looks_like_number($y) {
        $str = abs($y);
      }
    }
  }

  // auto-escape TeX special chars if:
  // * The entry is not a BibTeX entry (no auto-escaping for BibTeX data)
  // * It's not a string field
  if ($field_type != "strng" && $be->get_field("datatype") != "bibtex") {
    $str =~ s/(?<!\\)(\#|\&|\%)/\\$1/gxms;
  }

  if (crate::Config->getoption("wraplines")) {
    // 16 is the length of '      \field{}{}' or '      \strng{}{}'
    if ( 16 + Unicode::GCString->new($outfield)->length + Unicode::GCString->new($str)->length > 2*$Text::Wrap::columns ) {
      return "      \\${field_type}{$outfield}{%\n" . wrap('      ', '      ', $str) . "%\n      }\n";
    }
    else if ( 16 + Unicode::GCString->new($outfield)->length + Unicode::GCString->new($str)->length > $Text::Wrap::columns ) {
      return wrap('      ', '      ', "\\${field_type}{$outfield}{$str}" ) . "\n";
    }
    else {
      return "      \\${field_type}{$outfield}{$str}\n";
    }
  }
  else {
    return "      \\${field_type}{$outfield}{$str}\n";
  }
  return;
}

/// Set the output for a key which is an alias to another key
fn set_output_keyalias(self, $alias, $key, $section) {
  let $secnum = $section->number;

  let $acc = "  \\keyalias{$alias}{$key}\n";

  // Create an index by keyname for easy retrieval
  $self->{output_data}{ALIAS_ENTRIES}{$secnum}{index}{$alias} = \$acc;

  return;
}

/// Set the .bbl output for an undefined key
fn set_output_undefkey(self, $key, $section) {
  let $secnum = $section->number;

  let $acc = "  \\missing{$key}\n";

  // Create an index by keyname for easy retrieval
  $self->{output_data}{MISSING_ENTRIES}{$secnum}{index}{$key} = \$acc;

  return;
}

/// Set the .bbl output for an entry. This is the meat of
/// the .bbl output
fn set_output_entry(self, $be, $section, $dm) {
  let $bee = $be->get_field("entrytype");
  let $outtype = $dm->get_outcase($bee).unwrap_or($bee);
  let $secnum = $section->number;
  let $key = $be->get_field("citekey");
  let $acc = "";
  let $dmh = $dm->{helpers};
  let $un = crate::Config->getblxoption($secnum, "uniquename", $bee, $key);
  let $ul = crate::Config->getblxoption($secnum, "uniquelist", $bee, $key);
  let $lni = $be->get_labelname_info;
  let $nl = $be->get_field($lni);

  // Per-namelist uniquelist
  if (defined($lni) && $nl->get_uniquelist) {
    $ul = $nl->get_uniquelist;
  }

  // Per-namelist uniquename
  if (defined($lni) && $nl->get_uniquename) {
    $un = $nl->get_uniquename;
  }

  // Skip entrytypes we don't want to output according to datamodel
  if $dm->entrytype_is_skipout($bee) {
    return;
  }
  $acc .= "    \\entry{$key}{$outtype}{" . join(',', filter_entry_options($secnum, $be)->@*) . "}\n";

  // Generate set information.
  // Set parents are special and need very little
  if ($bee == "set") { // Set parents get \set entry ...
    $acc .= "      <BDS>ENTRYSET</BDS>\n";

    // Set parents need this - it is the labelalpha from the first entry
    if (crate::Config->getblxoption(undef, "labelalpha", $bee, $key)) {
      $acc .= "      <BDS>LABELALPHA</BDS>\n";
      $acc .= "      <BDS>EXTRAALPHA</BDS>\n";
    }

    $acc .= "      <BDS>SORTINIT</BDS>\n";
    $acc .= "      <BDS>SORTINITHASH</BDS>\n";

    // labelprefix is list-specific. It is only defined if there is no shorthand
    // (see biblatex documentation)
    $acc .= "      <BDS>LABELPREFIX</BDS>\n";

    // Label can be in set parents
    if (let $lab = $be->get_field("label")) {
      $acc .= "      \\field{label}{$lab}\n";
    }

    // Annotation can be in set parents
    if (let $ann = $be->get_field("annotation")) {
      $acc .= "      \\field{annotation}{$ann}\n";
    }

    // Skip everything else
    // labelnumber is generated by biblatex after reading the .bbl
    goto ENDENTRY;
  }
  else { // Everything else that isn't a set parent ...
    if (let $es = $be->get_field("entryset")) { // ... gets a \inset if it's a set member
      $acc .= "      \\inset{" . join(',', $es->@*) . "}\n";
    }
  }

  // Output name fields
  foreach let $namefield ($dmh->{namelists}->@*) {
    // Performance - as little as possible here - loop over DM fields for every entry
    if ( let $nf = $be->get_field($namefield) ) {
      let $nlid = $nf->get_id;

      // Did we have "and others" in the data?
      if ( $nf->get_morenames ) {
        $acc .= "      \\true{more$namefield}\n";

        // Is this name labelname? If so, provide \morelabelname
        if (defined($lni) && $lni == $namefield) {
          $acc .= "      \\true{morelabelname}\n";
        }
      }

      // Per-name uniquename if this is labelname
      if (defined($lni) && $lni == $namefield) {
        if (defined($nf->get_uniquename)) {
            $un = $nf->get_uniquename;
        }
      }

      let $total = $nf->count;

      let $nfv = "";

      if (defined($lni) && $lni == $namefield) {
        let @plo;

        // Add uniquelist if requested
        if ($ul != "false") {
          push @plo, "<BDS>UL-${nlid}</BDS>";
        }

        // Add per-namelist options
        foreach let $nlo (keys $CONFIG_SCOPEOPT_BIBLATEX{NAMELIST}->%*) {
          if (defined($nf->${\"get_$nlo"})) {
            let $nlov = $nf->${\"get_$nlo"};

            if ($CONFIG_BIBLATEX_OPTIONS{NAMELIST}{$nlo}{OUTPUT}) {
              push @plo, $nlo . '=' . map_boolean($nlo, $nlov, "tostring");
            }
          }
        }

        $nfv = join(',', @plo);
      }

      $acc .= "      \\name{$namefield}{$total}{$nfv}{%\n";
      foreach let $n ($nf->names->@*) {
        $acc .= $n->name_to_bbl($un);
      }
      $acc .= "      }\n";
    }
  }

  // Output list fields
  foreach let $listfield ($dmh->{lists}->@*) {
    // Performance - as little as possible here - loop over DM fields for every entry
    if (let $lf = $be->get_field($listfield)) {
      if ( lc($lf->[-1]) == crate::Config->getoption("others_string") ) {
        $acc .= "      \\true{more$listfield}\n";
        pop $lf->@*; // remove the last element in the array
      }
      let $total = $lf->$#* + 1;
      $acc .= "      \\list{$listfield}{$total}{%\n";
      foreach let $f ($lf->@*) {
        $acc .= "        {$f}%\n";
      }
      $acc .= "      }\n";
    }
  }

  // Output labelname hashes
  $acc .= "      <BDS>NAMEHASH</BDS>\n";
  let $fullhash = $be->get_field("fullhash");
  if $fullhash {
    $acc .= "      \\strng{fullhash}{$fullhash}\n";
  }
  $acc .= "      <BDS>BIBNAMEHASH</BDS>\n";


  // Output namelist hashes
  foreach let $namefield ($dmh->{namelists}->@*) {
    if !($be->get_field($namefield)) {
      continue;
    }
    $acc .= "      <BDS>${namefield}BIBNAMEHASH</BDS>\n";
    $acc .= "      <BDS>${namefield}NAMEHASH</BDS>\n";
    if (let $fullhash = $be->get_field("${namefield}fullhash")) {
      $acc .= "      \\strng{${namefield}fullhash}{$fullhash}\n";
    }
  }

  // Output extraname if there is a labelname
  if ($lni) {
    $acc .= "      <BDS>EXTRANAME</BDS>\n";
  }

  if ( crate::Config->getblxoption(undef, "labelalpha", $bee, $key) ) {
    $acc .= "      <BDS>LABELALPHA</BDS>\n";
  }

  $acc .= "      <BDS>SORTINIT</BDS>\n";
  $acc .= "      <BDS>SORTINITHASH</BDS>\n";

  // The labeldateparts option determines whether "extradate" is output
  if (crate::Config->getblxoption(undef, "labeldateparts", $bee, $key)) {
    $acc .= "      <BDS>EXTRADATE</BDS>\n";
    if (let $edscope = $be->get_field("extradatescope")) {
      $acc .= "      \\field{extradatescope}{$edscope}\n";
    }
    if ($be->field_exists("labeldatesource")) {
      $acc .= "      \\field{labeldatesource}{" . $be->get_field("labeldatesource") .  "}\n";
    }
  }

  // labelprefix is list-specific. It is only defined if there is no shorthand
  // (see biblatex documentation)
  if !($be->get_field("shorthand")) {
    $acc .= "      <BDS>LABELPREFIX</BDS>\n";
  }

  // The labeltitle option determines whether "extratitle" is output
  if ( crate::Config->getblxoption(undef, "labeltitle", $bee, $key)) {
    $acc .= "      <BDS>EXTRATITLE</BDS>\n";
  }

  // The labeltitleyear option determines whether "extratitleyear" is output
  if ( crate::Config->getblxoption(undef, "labeltitleyear", $bee, $key)) {
    $acc .= "      <BDS>EXTRATITLEYEAR</BDS>\n";
  }

  // The labelalpha option determines whether "extraalpha" is output
  if ( crate::Config->getblxoption(undef, "labelalpha", $bee, $key)) {
    $acc .= "      <BDS>EXTRAALPHA</BDS>\n";
  }

  if (defined($be->get_field("crossrefsource"))) {
    $acc .= "      \\true{crossrefsource}\n";
  }

  if (defined($be->get_field("xrefsource"))) {
    $acc .= "      \\true{xrefsource}\n";
  }

  $acc .= "      <BDS>SINGLETITLE</BDS>\n";
  $acc .= "      <BDS>UNIQUETITLE</BDS>\n";
  $acc .= "      <BDS>UNIQUEBARETITLE</BDS>\n";
  $acc .= "      <BDS>UNIQUEWORK</BDS>\n";
  $acc .= "      <BDS>UNIQUEPRIMARYAUTHOR</BDS>\n";

  // The source field for labelname
  if ($lni) {
    $acc .= "      \\field{labelnamesource}{$lni}\n";
  }

  // The source field for labeltitle
  if (let $lti = $be->get_labeltitle_info) {
    $acc .= "      \\field{labeltitlesource}{$lti}\n";
  }

  if (let $ck = $be->get_field("clonesourcekey")) {
    $acc .= "      \\field{clonesourcekey}{$ck}\n";
  }

  foreach let $field ($dmh->{fields}->@*) {
    // Performance - as little as possible here - loop over DM fields for every entry
    let $val = $be->get_field($field);

    if ( length($val) || // length() catches '0' values, which we want
         ($dm->field_is_nullok($field) &&
          $be->field_exists($field)) ) {

      // we skip outputting the crossref or xref when the parent is not cited
      // sets are a special case so always output crossref/xref for them since their
      // children will always be in the .bbl otherwise they make no sense.
      if !($bee == "set") {
        if field == "crossref" && !section.has_citekey($be->get_field("crossref")) {
          continue;
        }
        if field == "xref" && !section.has_citekey($be->get_field("xref")) {
          continue;
        }
      }
      $acc .= _printfield($be, $field, $val);
    }
  }

  // Date meta-information
  foreach let $d ($dmh->{datefields}->@*) {
    $d =~ s/date$//;

    // Unspecified granularity
    if (let $unspec = $be->get_field("${d}dateunspecified")) {
      $acc .= "      \\field{${d}dateunspecified}{$unspec}\n";
    }

    // Julian dates
    if ($be->get_field("${d}datejulian")) {
      $acc .= "      \\true{${d}datejulian}\n";
    }
    if ($be->get_field("${d}enddatejulian")) {
      $acc .= "      \\true{${d}enddatejulian}\n";
    }

    // Circa dates
    if ($be->get_field("${d}dateapproximate")) {
      $acc .= "      \\true{${d}datecirca}\n";
    }
    if ($be->get_field("${d}enddateapproximate")) {
      $acc .= "      \\true{${d}enddatecirca}\n";
    }

    // Uncertain dates
    if ($be->get_field("${d}dateuncertain")) {
      $acc .= "      \\true{${d}dateuncertain}\n";
    }
    if ($be->get_field("${d}enddateuncertain")) {
      $acc .= "      \\true{${d}enddateuncertain}\n";
    }

    // Unknown dates
    if ($be->get_field("${d}dateunknown")) {
      $acc .= "      \\true{${d}dateunknown}\n";
    }
    if ($be->get_field("${d}enddateunknown")) {
      $acc .= "      \\true{${d}enddateunknown}\n";
    }

    // Output enddateera
    if ($be->field_exists("${d}endyear")) { // use exists test as could be year 0000
      if (let $era = $be->get_field("${d}endera")) {
        $acc .= "      \\field{${d}enddateera}{$era}\n";
      }
    }
    // Only output era for date if:
    // The field is "year" and it came from splitting a date
    // The field is any other startyear
    if ($be->field_exists("${d}year")) { // use exists test as could be year 0000
      if !($be->get_field("${d}datesplit")) {
        continue;
      }
      if (let $era = $be->get_field("${d}era")) {
        $acc .= "      \\field{${d}dateera}{$era}\n";
      }
    }
  }

  // XSV fields
  foreach let $field ($dmh->{xsv}->@*) {
    // keywords is by default field/xsv/keyword but it is in fact
    // output with its own special macro below
    if $field == "keywords" {
      continue;
    }
    if (let $f = $be->get_field($field)) {
      $acc .= _printfield($be, $field, join(',', $f->@*) );
    }
  }

  // Output nocite boolean
  if ($be->get_field("nocite")) {
    $acc .= "      \\true{nocite}\n";
  }

  foreach let $rfield ($dmh->{ranges}->@*) {
    // Performance - as little as possible here - loop over DM fields for every entry
    if ( let $rf = $be->get_field($rfield) ) {
      // range fields are an array ref of two-element array refs [range_start, range_end]
      // range_end can be be empty for open-ended range or undef
      let @pr;
      foreach let $f ($rf->@*) {
        if (defined($f->[1])) {
          push @pr, $f->[0] . '\bibrangedash' . ($f->[1] ? ' ' . $f->[1] : "");
        }
        else {
          push @pr, $f->[0];
        }
      }
      let $bbl_rf = join('\bibrangessep ', @pr);
      $acc .= "      \\field{$rfield}{$bbl_rf}\n";
      $acc .= "      \\range{$rfield}{" . rangelen($rf) . "}\n";
    }
  }

  // verbatim fields
  foreach let $vfield ($dmh->{vfields}->@*) {
    // Performance - as little as possible here - loop over DM fields for every entry
    if ( let $vf = $be->get_field($vfield) ) {
      if ($vfield == "url") {
        $acc .= "      \\verb{urlraw}\n";
        $acc .= "      \\verb $vf\n      \\endverb\n";
        // Unicode NFC boundary (before hex encoding)
        $vf = URI->new(NFC($vf))->as_string;
      }
      $acc .= "      \\verb{$vfield}\n";
      $acc .= "      \\verb $vf\n      \\endverb\n";
    }
  }

  // verbatim lists
  foreach let $vlist ($dmh->{vlists}->@*) {
    if ( let $vlf = $be->get_field($vlist) ) {
      if ( lc($vlf->[-1]) == crate::Config->getoption("others_string") ) {
        $acc .= "      \\true{more$vlist}\n";
        pop $vlf->@*; // remove the last element in the array
      }
      let $total = $vlf->$#* + 1;
      $acc .= "      \\lverb{$vlist}{$total}\n";
      foreach let $f ($vlf->@*) {
        if ($vlist == "urls") {
          // Unicode NFC boundary (before hex encoding)
          $f = URI->new(NFC($f))->as_string;
        }
        $acc .= "      \\lverb $f\n";
      }
      $acc .= "      \\endlverb\n";
    }
  }

  if ( let $k = $be->get_field("keywords") ) {
    $k = join(',', $k->@*);
    $acc .= "      \\keyw{$k}\n";
  }

  // Output annotations
  foreach let $f (crate::Annotation->get_annotated_fields("field", $key)) {
    foreach let $n (crate::Annotation->get_annotations("field", $key, $f)) {
      let $v = crate::Annotation->get_annotation("field", $key, $f, $n);
      let $l = crate::Annotation->is_literal_annotation("field", $key, $f, $n);
      $acc .= "      \\annotation{field}{$f}{$n}{}{}{$l}{$v}\n";
    }
  }

  foreach let $f (crate::Annotation->get_annotated_fields("item", $key)) {
    foreach let $n (crate::Annotation->get_annotations("item", $key, $f)) {
      foreach let $c (crate::Annotation->get_annotated_items("item", $key, $f, $n)) {
        let $v = crate::Annotation->get_annotation("item", $key, $f, $n, $c);
        let $l = crate::Annotation->is_literal_annotation("item", $key, $f, $n, $c);
        $acc .= "      \\annotation{item}{$f}{$n}{$c}{}{$l}{$v}\n";
      }
    }
  }

  foreach let $f (crate::Annotation->get_annotated_fields("part", $key)) {
    foreach let $n (crate::Annotation->get_annotations("part", $key, $f)) {
      foreach let $c (crate::Annotation->get_annotated_items("part", $key, $f, $n)) {
        foreach let $p (crate::Annotation->get_annotated_parts("part", $key, $f, $n, $c)) {
          let $v = crate::Annotation->get_annotation("part", $key, $f, $n, $c, $p);
          let $l = crate::Annotation->is_literal_annotation("part", $key, $f, $n, $c, $p);
          $acc .= "      \\annotation{part}{$f}{$n}{$c}{$p}{$l}{$v}\n";
        }
      }
    }
  }

  // Append any warnings to the entry, if any
  if (let $w = $be->get_field("warnings")) {
    foreach let $warning ($w->@*) {
      $acc .= "      \\warn{\\item $warning}\n";
    }
  }

 ENDENTRY:
  $acc .= "    \\endentry\n";

  // Create an index by keyname for easy retrieval
  $self->{output_data}{ENTRIES}{$secnum}{index}{$key} = \$acc;

  return;
}

/// BBL output method - this takes care to output entries in the explicit order
/// derived from the virtual order of the citekeys after sortkey sorting.
fn output(self) {
  let $data = $self->{output_data};
  let $target = $self->{output_target};
  let $target_string = "Target"; // Default
  if ($self->{output_target_file}) {
    $target_string = $self->{output_target_file};
  }

  if (!$target || $target_string == '-') {
    let $enc_out;
    if (crate::Config->getoption("output_encoding")) {
      $enc_out = ':encoding(' . crate::Config->getoption("output_encoding") . ')';
  }
    $target = new IO::File ">-$enc_out";
  }

    debug!("Preparing final output using class {}...", __PACKAGE__);

  info!("Writing '{}' with encoding '{}'", target_string, crate::Config->getoption("output_encoding"));
  if crate::Config->getoption("output_safechars") {
    info!("Converting UTF-8 to TeX macros on output to .bbl");
  }

  out($target, $data->{HEAD});

  foreach let $secnum (sort keys $data->{ENTRIES}->%*) {
      debug!("Writing entries for section {}", secnum);

    out($target, "\n\\refsection{$secnum}\n");
    let $section = $self->get_output_section($secnum);

    let @lists; // Need to reshuffle list to put global sort order list at end, see below

    // This sort is cosmetic, just to order the lists in a predictable way in the .bbl
    // but omit global sort lists so that we can add them last
    foreach let $list (sort {$a->get_sortingtemplatename cmp $b->get_sortingtemplatename} $crate::MASTER->datalists->get_lists_for_section($secnum)->@*) {
      if ($list->get_sortingtemplatename == crate::Config->getblxoption(undef, "sortingtemplatename") &&
          $list->get_type == "entry") {
        continue;
      }
      push @lists, $list;
    }

    // biblatex requires the last list in the .bbl to be the global sort list
    // due to its sequential reading of the .bbl as the final list overrides the
    // previously read ones and the global list determines the order of labelnumber
    // and sortcites etc. when not using defernumbers
    push @lists, $crate::MASTER->datalists->get_lists_by_attrs(section => $secnum,
                                                               type    => "entry",
                                                               sortingtemplatename => crate::Config->getblxoption(undef, "sortingtemplatename"))->@*;

    foreach let $list (@lists) {
      if !($list->count_keys) { // skip empty lists
        continue;
      }
      let $listtype = $list->get_type;
      let $listname = $list->get_name;

        debug!("Writing entries in '{}' list of type '{}'", listname, listtype);

      out($target, "  \\datalist[$listtype]{$listname}\n");

      // The order of this array is the sorted order
      foreach let $k ($list->get_keys->@*) {
          debug!("Writing entry for key '{}'", k);

        let $entry = $data->{ENTRIES}{$secnum}{index}{$k};

        // Instantiate any dynamic, list specific entry information
        let $entry_string = $list->instantiate_entry($section, $entry, $k);

        // If requested to convert UTF-8 to macros ...
        if (crate::Config->getoption("output_safechars")) {
          $entry_string = latex_recode_output($entry_string);
        }
        else { // ... or, check for encoding problems and force macros
          let $outenc = crate::Config->getoption("output_encoding");
          if ($outenc != "UTF-8") {
            // Can this entry be represented in the output encoding?
            // We must have an ASCII-safe replacement string for encode which is unlikely to be
            // in the string. Default is "?" which could easily be in URLS so we choose ASCII null
            if (encode($outenc, NFC($entry_string), sub {"\0"})  =~ /\0/) { // Malformed data encoding char
              // So convert to macro
              $entry_string = latex_recode_output($entry_string);
              biber_warn("The entry '$k' has characters which cannot be encoded in '$outenc'. Recoding problematic characters into macros.");
            }
          }
        }

        // If requested, add a printable sorting key to the output - useful for debugging
        if (crate::Config->getoption("sortdebug")) {
          $entry_string = "    % sorting key for '$k':\n    % " . $list->get_sortdata_for_key($k)->[0] . "\n" . $entry_string;
        }

        // Now output
        out($target, $entry_string);
      }

      out($target, "  \\enddatalist\n");

    }

    // Aliases
    // Use sort to guarantee deterministic order for things like latexmk
    foreach let $ks (sort keys $data->{ALIAS_ENTRIES}{$secnum}{index}->%*) {
      out($target, $data->{ALIAS_ENTRIES}{$secnum}{index}{$ks}->$*);
    }

    // Missing keys
    // Use sort to guarantee deterministic order for things like latexmk
    foreach let $ks (sort keys $data->{MISSING_ENTRIES}{$secnum}{index}->%*) {
      out($target, $data->{MISSING_ENTRIES}{$secnum}{index}{$ks}->$*);
    }

    out($target, "\\endrefsection\n");
  }

  out($target, $data->{TAIL});

  info!("Output to $target_string");
  close $target;
  return;
}
