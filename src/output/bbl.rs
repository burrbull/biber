/*
use parent qw(crate::Output::base);

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
*/

/// Class for Biber output of .bbl
pub struct Bbl;

impl Bbl {
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
      let mut pa = pa.join("%\n");

      // If requested to convert UTF-8 to macros ...
      if (crate::Config->getoption("output_safechars")) {
        pa = latex_recode_output(pa);
      } else {           // ... or, check for encoding problems and force macros
        let outenc = crate::Config->getoption("output_encoding");
        if outenc != "UTF-8" {
          // Can this entry be represented in the output encoding?
          if (encode($outenc, NFC(pa), || {"\0"}) =~ /\0/) { // Malformed data encoding char
            // So convert to macro
            pa = latex_recode_output(pa);
          }
        }
      }
      self.output_data.HEAD.push_str(format!("\\preamble{{%\n{pa}%\n}}\n\n"));
    }
    self.output_data.TAIL.push_str("\\endinput\n\n");
    return;
  }

  /// Add the .bbl for a text field to the output accumulator.
  fn _printfield(be, field, $str) {
    let $field_type = "field";
    let $dm = crate::config::get_dm();

    let $outfield = dm.get_outcase(field);

    if is_null($str) && !dm.field_is_nullok(field) {
      return "";
    }

    // crossref and xref are of type "strng" in the .bbl
    if field.to_lowercase() == "crossref" || field.to_lowercase() == "xref" {
      $field_type = "strng";
    }

    // Output absolute astronomical year by default (with year 0)
    // biblatex will adjust the years when printed with BCE/CE eras
    if ($field =~ m/^(.*)(?!end)year$/) {
      if (let $y = be.get_field("$1year")) {
        if looks_like_number(y) {
          $str = abs(y);
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
  fn set_output_entry(self, be: Entry, section: Section, dm: DataModel) {
    let bee = be.get_field("entrytype");
    let outtype = dm.get_outcase(bee).unwrap_or(bee);
    let secnum = section.number();
    let key = be.get_field("citekey");
    let mut acc = String::new();
    let dmh = dm.helpers;
    let mut un = crate::Config->getblxoption(secnum, "uniquename", bee, key);
    let mut ul = crate::Config->getblxoption(secnum, "uniquelist", bee, key);
    let lni = be.get_labelname_info();
    let nl = be.get_field(lni);

    // Per-namelist uniquelist
    if lni.is_some() {
      if nl.get_uniquelist() {
        ul = nl.get_uniquelist();
      }
    }

    // Per-namelist uniquename
    if lni.is_some() {
      if nl.get_uniquename() {
        un = nl.get_uniquename();
      }
    }

    // Skip entrytypes we don't want to output according to datamodel
    if dm.entrytype_is_skipout(bee) {
      return;
    }
    acc.push_str(&format!("    \\entry{key}{outtype}{{{}}}\n", filter_entry_options(secnum, be).join(",")));

    // Generate set information.
    // Set parents are special and need very little
    if bee == "set" { // Set parents get \set entry ...
      acc.push_str("      <BDS>ENTRYSET</BDS>\n");

      // Set parents need this - it is the labelalpha from the first entry
      if (crate::Config->getblxoption(None, "labelalpha", bee, key)) {
        acc.push_str("      <BDS>LABELALPHA</BDS>\n");
        acc.push_str("      <BDS>EXTRAALPHA</BDS>\n");
      }

      acc.push_str("      <BDS>SORTINIT</BDS>\n");
      acc.push_str("      <BDS>SORTINITHASH</BDS>\n");

      // labelprefix is list-specific. It is only defined if there is no shorthand
      // (see biblatex documentation)
      acc.push_str("      <BDS>LABELPREFIX</BDS>\n");

      // Label can be in set parents
      if (let $lab = be.get_field("label")) {
        acc.push_str(&format!("      \\field{{label}}{lab}\n"));
      }

      // Annotation can be in set parents
      if (let $ann = be.get_field("annotation")) {
        acc.push_str(&format!("      \\field{{annotation}}{ann}\n"));
      }

      // Skip everything else
      // labelnumber is generated by biblatex after reading the .bbl
      goto 'ENDENTRY;
    }
    else { // Everything else that isn't a set parent ...
      if (let $es = be.get_field("entryset")) { // ... gets a \inset if it's a set member
        acc.push_str(&format!("      \\inset{{{}}}\n"), es.join(","));
      }
    }

    // Output name fields
    for namefield in ($dmh->{namelists}->@*) {
      // Performance - as little as possible here - loop over DM fields for every entry
      if ( let $nf = be.get_field(namefield) ) {
        let nlid = nf.get_id();

        // Did we have "and others" in the data?
        if nf.get_morenames() {
          acc.push_str(&format!("      \\true{{more{namefield}}}\n"));

          // Is this name labelname? If so, provide \morelabelname
          if lni.filter(|lni| lni == namefield).is_some() {
            acc.push_str("      \\true{morelabelname}\n");
          }
        }

        // Per-name uniquename if this is labelname
        if lni.filter(|lni| lni == namefield).is_some() {
          un = nf.get_uniquename().or(un);
        }

        let total = nf.count();

        let mut nfv = String::new();

        if lni.filter(|lni| lni == namefield).is_some() {
          let plo = Vec::new();

          // Add uniquelist if requested
          if ul != "false" {
            plo.push_str(&format!("<BDS>UL-{nlid}</BDS>"));
          }

          // Add per-namelist options
          for nlo in CONFIG_OPT_SCOPE_BIBLATEX.iter_by_right("NAMELIST") {
            if let Some(nlov) = nf.get(\"get_$nlo") {
              if ($CONFIG_BIBLATEX_OPTIONS{NAMELIST}{$nlo}{OUTPUT}) {
                plo.push_str(&format!("{nlo}={}", map_boolean(nlo, nlov, "tostring")));
              }
            }
          }

          nfv = plo.join(",");
        }

        acc.push_str(&format!("      \\name{namefield}{total}{nfv}{{%\n"));
        for n in nf.names() {
          acc.push_str(n.name_to_bbl(un));
        }
        acc.push_str("      }\n");
      }
    }

    // Output list fields
    for listfield in ($dmh->{lists}->@*) {
      // Performance - as little as possible here - loop over DM fields for every entry
      if (let $lf = $be->get_field($listfield)) {
        if lf.last().filter(|last| last.to_lowercase() == crate::Config->getoption("others_string")).is_some() {
          acc.push_str(&format!("      \\true{{more{listfield}}}\n"));
          lf.pop(); // remove the last element in the array
        }
        let total = lf.len();
        acc.push_str(&format!("      \\list{listfield}{total}{{%\n"));
        for f in &lf {
          acc.push_str(&format!("        {{{f}}}%\n"));
        }
        acc.push_str("      }\n");
      }
    }

    // Output labelname hashes
    acc.push_str("      <BDS>NAMEHASH</BDS>\n");
    if let Some(fullhash) = be.get_field("fullhash").skip_empty();
      acc.push_str(&format!("      \\strng{{fullhash}}{fullhash}\n"));
    }
    acc.push_str("      <BDS>BIBNAMEHASH</BDS>\n");


    // Output namelist hashes
    for namefield in ($dmh->{namelists}->@*) {
      if !(be.get_field(namefield)) {
        continue;
      }
      acc.push_str(&format!("      <BDS>{namefield}BIBNAMEHASH</BDS>\n"));
      acc.push_str(&format!("      <BDS>{namefield}NAMEHASH</BDS>\n"));
      if (let $fullhash = $be->get_field("${namefield}fullhash")) {
        acc.push_str(&format!("      \\strng{{{namefield}fullhash}}{fullhash}\n"));
      }
    }

    // Output extraname if there is a labelname
    if ($lni) {
      acc.push_str("      <BDS>EXTRANAME</BDS>\n");
    }

    if ( crate::Config->getblxoption(None, "labelalpha", bee, key) ) {
      acc.push_str("      <BDS>LABELALPHA</BDS>\n");
    }

    acc.push_str("      <BDS>SORTINIT</BDS>\n");
    acc.push_str("      <BDS>SORTINITHASH</BDS>\n");

    // The labeldateparts option determines whether "extradate" is output
    if (crate::Config->getblxoption(None, "labeldateparts", bee, key)) {
      acc.push_str("      <BDS>EXTRADATE</BDS>\n");
      if (let $edscope = be.get_field("extradatescope")) {
        acc.push_str(&format!("      \\field{{extradatescope}}{edscope}\n"));
      }
      if (be.field_exists("labeldatesource")) {
        acc.push_str(&format!("      \\field{{labeldatesource}}{{{}}}\n", be.get_field("labeldatesource")));
      }
    }

    // labelprefix is list-specific. It is only defined if there is no shorthand
    // (see biblatex documentation)
    if !(be.get_field("shorthand")) {
      acc.push_str("      <BDS>LABELPREFIX</BDS>\n");
    }

    // The labeltitle option determines whether "extratitle" is output
    if ( crate::Config->getblxoption(None, "labeltitle", bee, key)) {
      acc.push_str("      <BDS>EXTRATITLE</BDS>\n");
    }

    // The labeltitleyear option determines whether "extratitleyear" is output
    if ( crate::Config->getblxoption(None, "labeltitleyear", bee, key)) {
      acc.push_str("      <BDS>EXTRATITLEYEAR</BDS>\n");
    }

    // The labelalpha option determines whether "extraalpha" is output
    if ( crate::Config->getblxoption(None, "labelalpha", bee, key)) {
      acc.push_str("      <BDS>EXTRAALPHA</BDS>\n");
    }

    if be.get_field("crossrefsource").is_some() {
      acc.push_str("      \\true{crossrefsource}\n");
    }

    if be.get_field("xrefsource").is_some() {
      acc.push_str("      \\true{xrefsource}\n");
    }

    acc.push_str("      <BDS>SINGLETITLE</BDS>\n");
    acc.push_str("      <BDS>UNIQUETITLE</BDS>\n");
    acc.push_str("      <BDS>UNIQUEBARETITLE</BDS>\n");
    acc.push_str("      <BDS>UNIQUEWORK</BDS>\n");
    acc.push_str("      <BDS>UNIQUEPRIMARYAUTHOR</BDS>\n");

    // The source field for labelname
    if ($lni) {
      acc.push_str(&format!("      \\field{{labelnamesource}}{{{lni}}}\n"));
    }

    // The source field for labeltitle
    if (let $lti = be.get_labeltitle_info()) {
      acc.push_str(&format!("      \\field{{labeltitlesource}}{{{lti}}}\n"));
    }

    if (let $ck = be.get_field("clonesourcekey")) {
      acc.push_str(&format!("      \\field{{clonesourcekey}}{{{ck}}}\n"));
    }

    for field in ($dmh->{fields}->@*) {
      // Performance - as little as possible here - loop over DM fields for every entry
      let $val = be.get_field(field);

      if ( length($val) || // length() catches '0' values, which we want
          ($dm->field_is_nullok($field) && be.field_exists(field)) ) {

        // we skip outputting the crossref or xref when the parent is not cited
        // sets are a special case so always output crossref/xref for them since their
        // children will always be in the .bbl otherwise they make no sense.
        if bee != "set" {
          if field == "crossref" && !section.has_citekey(be.get_field("crossref")) {
            continue;
          }
          if field == "xref" && !section.has_citekey(be.get_field("xref")) {
            continue;
          }
        }
        acc.push_str(_printfield(be, field, val));
      }
    }

    // Date meta-information
    for mut d in ($dmh->{datefields}->@*) {
      if let Some(val) = d.strip_suffix("date") {
        d = val;
      }

      // Unspecified granularity
      if (let $unspec = be.get_field(&format!("{d}dateunspecified"))) {
        acc.push_str(&format!("      \\field{{{d}dateunspecified}}{{{unspec}}}\n"));
      }

      // Julian dates
      if (be.get_field(&format!("{d}datejulian"))) {
        acc.push_str(&format!("      \\true{{{d}datejulian}}\n"));
      }
      if (be.get_field(&format!("{d}enddatejulian"))) {
        acc.push_str(&format!("      \\true{{{d}enddatejulian}}\n"));
      }

      // Circa dates
      if (be.get_field(&format!("{d}dateapproximate"))) {
        acc.push_str(&format!("      \\true{{{d}datecirca}}\n"));
      }
      if ($be->get_field(&format!("{d}enddateapproximate"))) {
        acc.push_str(&format!("      \\true{{{d}enddatecirca}}\n"));
      }

      // Uncertain dates
      if (be.get_field(&format!("{d}dateuncertain"))) {
        acc.push_str(&format!("      \\true{{{d}dateuncertain}}\n"));
      }
      if (be.get_field(&format!("{d}enddateuncertain"))) {
        acc.push_str(&format!("      \\true{{{d}enddateuncertain}}\n"));
      }

      // Unknown dates
      if (be.get_field(&format!("{d}dateunknown"))) {
        acc.push_str(&format!("      \\true{{{d}dateunknown}}\n"));
      }
      if (be.get_field(&format!("{d}enddateunknown"))) {
        acc.push_str(&format!("      \\true{{{d}enddateunknown}}\n"));
      }

      // Output enddateera
      if (be.field_exists(&format!("{d}endyear"))) { // use exists test as could be year 0000
        if (let $era = be.get_field(&format!("{d}endera"))) {
          acc.push_str(&format!("      \\field{{{d}enddateera}}{{{era}}}\n"));
        }
      }
      // Only output era for date if:
      // The field is "year" and it came from splitting a date
      // The field is any other startyear
      if (be.field_exists(&format!("{d}year"))) { // use exists test as could be year 0000
        if !(be.get_field(&format!("{d}datesplit"))) {
          continue;
        }
        if (let $era = be.get_field(&format!("{d}era"))) {
          acc.push_str(&format!("      \\field{{{d}dateera}}{{{era}}}\n"));
        }
      }
    }

    // XSV fields
    for field in ($dmh->{xsv}->@*) {
      // keywords is by default field/xsv/keyword but it is in fact
      // output with its own special macro below
      if field == "keywords" {
        continue;
      }
      if (let $f = be.get_field(field)) {
        acc.push_str(_printfield(be, field, f.join(",") ));
      }
    }

    // Output nocite boolean
    if (be.get_field("nocite")) {
      acc.push_str("      \\true{nocite}\n");
    }

    for rfield in ($dmh->{ranges}->@*) {
      // Performance - as little as possible here - loop over DM fields for every entry
      if ( let $rf = be.get_field(rfield) ) {
        // range fields are an array ref of two-element array refs [range_start, range_end]
        // range_end can be be empty for open-ended range or undef
        let mut pr = Vec::new();
        for f in ($rf->@*) {
          if let Some(f1) = f.get(1) {
            pr.push(format!(r"{}\bibrangedash{}", f[0], if !f1.is_empty() { &format!(" {}" , f1) } else { "" }))
          }
          else {
            pr.push(f[0]);
          }
        }
        let $bbl_rf = pr.join(r"\bibrangessep ");
        acc.push_str(&format!("      \\field{{{rfield}}}{{{bbl_rf}}}\n"));
        acc.push_str(&format!("      \\range{{{rfield}}}{{{}}}\n", rangelen(rf)));
      }
    }

    // verbatim fields
    for vfield in ($dmh->{vfields}->@*) {
      // Performance - as little as possible here - loop over DM fields for every entry
      if ( let $vf = be.get_field(vfield) ) {
        if vfield == "url" {
          acc.push_str("      \\verb{urlraw}\n");
          acc.push_str(&format!("      \\verb {vf}\n      \\endverb\n"));
          // Unicode NFC boundary (before hex encoding)
          $vf = URI->new(NFC($vf))->as_string;
        }
        acc.push_str(&format!("      \\verb{{{vfield}}}\n"));
        acc.push_str(&format!("      \\verb {vf}\n      \\endverb\n"));
      }
    }

    // verbatim lists
    for vlist in ($dmh->{vlists}->@*) {
      if ( let $vlf = be.get_field(vlist) ) {
        if vlf.last().filter(|last| last.to_lowercase() == crate::Config->getoption("others_string")) {
          acc.push_str(&format!("      \\true{{more{vlist}}}\n"));
          vlf.pop(); // remove the last element in the array
        }
        let total = vlf.len();
        acc.push_str(&format!("      \\lverb{{{vlist}}}{{{total}}}\n"));
        for f in &vlf {
          if vlist == "urls" {
            // Unicode NFC boundary (before hex encoding)
            $f = URI->new(NFC($f))->as_string;
          }
          acc.push_str(&format!("      \\lverb {f}\n"));
        }
        acc.push_str("      \\endlverb\n");
      }
    }

    if ( let $k = be.get_field("keywords") ) {
      let k = k.join(",");
      acc.push_str(&format!("      \\keyw{{{k}}}\n"));
    }

    // Output annotations
    let ann = &crate::annotation::ANN.lock().unwrap();
    for f in (ann.get_annotated_fields(Scope::Field, key)) {
      for n in (ann.get_annotations(Scope::Field, key, f)) {
        let annot = ann.get_field_annotation(key, f, n).unwrap();
        let v = annot.value();
        let l = annot.is_literal();
        acc.push_str(&format!("      \\annotation{{field}}{{{f}}}{{{n}}}{{}}{{}}{{{l}}}{{{v}}}\n"));
      }
    }

    for f in (ann.get_annotated_fields(Scope::Item, key)) {
      for n in (ann.get_annotations(Scope::Item, key, f)) {
        for (c, annot) in (ann.get_annotated_items(key, f, n)) {
          let v = annot.value();
          let l = annot.is_literal();
          acc.push_str(&format!("      \\annotation{{item}}{{{f}}}{{{n}}}{{{c}}}{{}}{{{l}}}{{{v}}}\n"));
        }
      }
    }

    for f in (ann.get_annotated_fields(Scope::Part, key)) {
      for n in (ann.get_annotations(Scope::Part, key, f)) {
        for (c, p, annot) in (ann.get_annotated_parts(key, f, n)) {
          let v = annot.value();
          let l = annot.is_literal();
          acc.push_str(&format!("      \\annotation{{part}}{{{f}}}{{{n}}}{{{c}}}{{{p}}}{{{l}}}{{{v}}}\n"));
        }
      }
    }

    // Append any warnings to the entry, if any
    if (let $w = be.get_field("warnings")) {
      for warning in w {
        acc.push_str(&format!("      \\warn{{\\item {warning}}}\n"));
      }
    }

  'ENDENTRY:
    acc.push_str("    \\endentry\n");

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

    for secnum in (sort keys $data->{ENTRIES}->%*) {
        debug!("Writing entries for section {}", secnum);

      out($target, "\n\\refsection{$secnum}\n");
      let $section = $self->get_output_section($secnum);

      let mut lists = Vec::new(); // Need to reshuffle list to put global sort order list at end, see below

      // This sort is cosmetic, just to order the lists in a predictable way in the .bbl
      // but omit global sort lists so that we can add them last
      for list in (sort {a.get_sortingtemplatename() cmp b.get_sortingtemplatename()} $crate::MASTER->datalists->get_lists_for_section($secnum)->@*) {
        if (list.get_sortingtemplatename() == crate::Config->getblxoption(None, "sortingtemplatename") &&
            list.get_type() == "entry") {
          continue;
        }
        lists.push(list);
      }

      // biblatex requires the last list in the .bbl to be the global sort list
      // due to its sequential reading of the .bbl as the final list overrides the
      // previously read ones and the global list determines the order of labelnumber
      // and sortcites etc. when not using defernumbers
      lists.push($crate::MASTER->datalists->get_lists_by_attrs(section => secnum,
                                                                type    => "entry",
                                                                sortingtemplatename => crate::Config->getblxoption(None, "sortingtemplatename"))->@*);

      for list in &lists {
        if !($list->count_keys) { // skip empty lists
          continue;
        }
        let listtype = list.get_type();
        let listname = list.get_name();

          debug!("Writing entries in '{}' list of type '{}'", listname, listtype);

        out($target, "  \\datalist[$listtype]{$listname}\n");

        // The order of this array is the sorted order
        for k in list.get_keys() {
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
      for ks in (sort keys $data->{ALIAS_ENTRIES}{$secnum}{index}->%*) {
        out($target, $data->{ALIAS_ENTRIES}{$secnum}{index}{$ks}->$*);
      }

      // Missing keys
      // Use sort to guarantee deterministic order for things like latexmk
      for ks in (sort keys $data->{MISSING_ENTRIES}{$secnum}{index}->%*) {
        out($target, $data->{MISSING_ENTRIES}{$secnum}{index}{$ks}->$*);
      }

      out($target, "\\endrefsection\n");
    }

    out($target, $data->{TAIL});

    info!("Output to $target_string");
    close $target;
    return;
  }
}
