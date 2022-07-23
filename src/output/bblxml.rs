/*
use parent qw(crate::Output::base);

use crate::Config;
use crate::Constants;
use crate::Entry;
use crate::Utils;
use Encode;
use List::AllUtils qw( :all );
use IO::File;
use IO::String;
use Log::Log4perl qw( :no_extra_logdie_message );
use Scalar::Util qw(looks_like_number);
use XML::Writer;
use Unicode::Normalize;
*/

/// Class for Biber output of .bbl in XML format
pub struct BblXML;

impl BblXML {
  /// Initialize a crate::Output::bbxml object
  fn new(obj) {
    let $self;
    if (defined($obj) && ref($obj) == "HASH") {
      $self = bless $obj, $class;
    }
    else {
      $self = bless {}, $class;
    }

    return $self;
  }

  /// Set the output target file of a crate::Output::bblxml object
  /// A convenience around set_output_target so we can keep track of the
  /// filename
  fn set_output_target_file(self, $bblxmlfile, $init) {

    // we assume that the schema files are in the same dir as Biber.pm:
    (let $vol, let $biber_path, _) = File::Spec->splitpath( $INC{"Biber.pm"} );

    $self->{output_target_file} = $bblxmlfile;

    if ($init) {
      let $bblxml = "https://sourceforge.net/projects/biblatex/bblxml";
      $self->{xml_prefix} = $bblxml;

      let $schemafile;
      let $exts = DS_EXTENSIONS.values().join("|");
      if ($bblxmlfile =~ m/\.(?:$exts)$/) {
        $schemafile = $bblxmlfile =~ s/\.(?:$exts)$/.rng/r;
      }
      else {
        // in tests, there is no extension as we are using a temp file
        $schemafile = $bblxmlfile . '.rng';
      }

      let $of;
      if ($bblxmlfile == '-') {
        open($of, '>&:encoding(UTF-8)', STDOUT);
      }
      else {
        $of = IO::File->new($bblxmlfile, '>:encoding(UTF-8)');
      }
      $of->autoflush;             // Needed for running tests to string refs

      let $xml = XML::Writer->new(OUTPUT      => $of,
                                ENCODING   => "UTF-8",
                                DATA_MODE   => 1,
                                DATA_INDENT => crate::Config->getoption("output_indent"),
                                NAMESPACES  => 1,
                                UNSAFE      => 1,
                                PREFIX_MAP  => {$bblxml => "bbl"});
      $xml->xmlDecl();
      $xml->pi("xml-model", &format!(r#"href="{}" type="application/xml" schematypens="http://relaxng.org/ns/structure/1.0""#, schemafile));
      $xml->comment("Auto-generated by crate::Output::bblxml");
      $xml->startTag([$self->{xml_prefix}, "refsections"]);
      return $xml;
    }
    return;
  }

  /// Set the .bblxml output for an entry. This is the meat of
  /// the .bbl output
  fn set_output_entry(
    self,
    be: crate::Entry,
    section: crate::Section, // Section object the entry occurs in
    dm: crate::DataModel
  ) {
    let $bee = be.get_field("entrytype");
    let $dmh = crate::config::get_dm_helpers();
    let $acc = "";
    let $secnum = section.number();
    let $key = be.get_field("citekey");
    let $xml_prefix = "https://sourceforge.net/projects/biblatex/bblxml";
    let $un = crate::Config->getblxoption(secnum, "uniquename", bee, key);
    let $ul = crate::Config->getblxoption(secnum, "uniquelist", bee, key);
    let nl = be.get_field(be.get_labelname_info());
    let lni = be.get_labelname_info();

    // Per-namelist uniquelist
    if lni.is_some() {
      if nl.get_uniquelist() {
        ul = nl.get_uniquelist();
      }
    }

    // Per-namelist uniquename
    if lni.is_some() {
      if nl.get_uniquename() {
        $un = nl.get_uniquename();
      }
    }

    let $xml = XML::Writer->new(OUTPUT      => "self",
                              ENCODING    => "UTF-8",
                              DATA_MODE   => 1,
                              DATA_INDENT => crate::Config->getoption("output_indent"),
                              NAMESPACES  => 1,
                              PREFIX_MAP  => {$xml_prefix => "bbl"});


    // Skip entrytypes we don't want to output according to datamodel
    if dm.entrytype_is_skipout(bee) {
      return;
    }

    let mut entryopts = AttributeMap::new();
    if be.get_field("crossrefsource").is_some() {
      entryopts.insert("source", "crossref");
    }

    if be.get_field("xrefsource").is_some() {
      entryopts.insert("source", "xref");
    }

    entryopts.insert("singletitle", "[BDS]SINGLETITLE[/BDS]");
    entryopts.insert("uniquetitle", "[BDS]UNIQUETITLE[/BDS]");
    entryopts.insert("uniquebaretitle", "[BDS]UNIQUEBARETITLE[/BDS]");
    entryopts.insert("uniquework", "[BDS]UNIQUEWORK[/BDS]");
    entryopts.insert("uniqueprimaryauthor", "[BDS]UNIQUEPRIMARYAUTHOR[/BDS]");

    $xml->startTag([$xml_prefix, "entry"], key => _bblxml_norm($key), type => _bblxml_norm($bee), @entryopts);
    let mut opts = Vec::new();
    for opt in (filter_entry_options($secnum, $be)->@*) {
      opt.push(opt);
    }
    if !opts.is_empty() {
      $xml->startTag([$xml_prefix, "options"]);
      for opt in opts.iter() {
        $xml->dataElement([$xml_prefix, "option"], _bblxml_norm(opt));
      }
      $xml->endTag();// options
    }

    // Generate set information
    // Set parents are special and need very little
    if ($bee == "set") {// Set parents get <set> entry ...

      $xml->dataElement("BDS", "ENTRYSET");

      // Set parents need this - it is the labelalpha from the first entry
      if (crate::Config->getblxoption(None, "labelalpha", bee, key)) {
        $xml->dataElement("BDS", "LABELALPHA");
        $xml->dataElement("BDS", "EXTRAALPHA");
      }

      $xml->dataElement("BDS", "SORTINIT");
      $xml->dataElement("BDS", "SORTINITHASH");

      // labelprefix is list-specific. It is only defined if there is no shorthand
      // (see biblatex documentation)
      $xml->dataElement("BDS", "LABELPREFIX");

      // Label can be in set parents
      if (let $lab = be.get_field("label")) {
        $xml->dataElement([$xml_prefix, "field"], _bblxml_norm($lab), attribute_map!("name" => "label"));
      }

      // Annotation can be in set parents
      if (let $ann = be.get_field("annotation")) {
        $xml->dataElement([$xml_prefix, "field"], _bblxml_norm($ann), attribute_map!("name" => "annotation"));
      }

      // Skip everything else
      // labelnumber/labelprefix etc. are generated by biblatex after reading the .bbl
      goto 'ENDENTRY;

    }
    else { // Everything else that isn't a set parent ...
      if (let $es = be.get_field("entryset")) { // ... gets a <inset> if it's a set member
        $xml->startTag([$xml_prefix, "inset"]);
        for m in ($es->@*) {
          $xml->dataElement([$xml_prefix, "member"], _bblxml_norm($m));
        }
        $xml->endTag();// inset
      }
    }

    // Output name fields
    for namefield in dm.get_fields_of_type(FieldType::List, &[DataType::Name], None) {
      if ( let $nf = be.get_field(namefield) ) {
        if dm.field_is_skipout(namefield) {
          continue;
        }
        let nlid = nf.get_id();
        let %plo;

        // Did we have "and others" in the data?
        if nf.get_morenames() {
          $plo{more} = "true";
        }

        let total = nf.count();

        if (lni.is_some() && lni == namefield) {

          // Add uniquelist if requested
          // Don't use angles in attributes ...
          if (ul != "false") {
            $plo{ul} = format!("[BDS]UL-{nlid}[/BDS]");
          }

          // Add per-namelist options
          for nlo in CONFIG_OPT_SCOPE_BIBLATEX.iter_by_right("NAMELIST") {
            if (defined($nf->${\"get_$nlo"})) {
              let $nlov = $nf->${\"get_$nlo"};

              if ($CONFIG_BIBLATEX_OPTIONS{NAMELIST}{$nlo}{OUTPUT}) {
                $plo{$nlo} = map_boolean($nlo, $nlov, "tostring");
              }
            }
          }
        }

        $xml->startTag([$xml_prefix, "names"], type => $namefield, count => $total, map {$_ => $plo{$_}} sort keys %plo);

        // Now the names
        for n in nf.names() {

          // Per-name uniquename if this is labelname
          if ($lni == $namefield) {
            un = n.get_uniquename().or(un);
          }

          $n->name_to_bblxml($xml, $xml_prefix, $un);
        }
        $xml->endTag();// names
      }
    }

    // Output list fields
    for listfield in ($dm->get_fields_of_fieldtype("list")->@*) {
      if (let $lf = be.get_field(listfield)) {
        if dm.field_is_datatype("name", listfield) { // name is a special list
          continue;
        }
        if dm.field_is_datatype("uri", listfield) { // special lists
          continue;
        }
        if dm.field_is_skipout(listfield) {
          continue;
        }

        let %plo;

        if ( lf->[-1].to_lowercase() == crate::Config->getoption("others_string") ) {
          // Did we have "and others" in the data?
          $plo{more} = "true";
          pop $lf->@*; // remove the last element in the array
        }

        let $total = lf.len();
        $xml->startTag([$xml_prefix, "list"], name => $listfield, count => $total, map {$_ => $plo{$_}} sort keys %plo);
        for f in &lf {
          $xml->dataElement([$xml_prefix, "item"], _bblxml_norm(f));
        }
        $xml->endTag();// list
      }
    }

    // Output labelname hashes
    $xml->dataElement("BDS", "NAMEHASH");
    let $fullhash = be.get_field("fullhash");
    if $fullhash {
      $xml->dataElement([$xml_prefix, "field"], _bblxml_norm($fullhash), name => "fullhash");
    }
    $xml->dataElement("BDS", "BIBNAMEHASH");

    // Output namelist hashes
    for namefield in ($dmh->{namelists}->@*) {
      if !(be.get_field($namefield)) {
        continue;
      }
      $xml->dataElement("BDS", "${namefield}NAMEHASH");
      if (let $fullhash = be.get_field(format!("{namefield}fullhash"))) {
        $xml->dataElement([$xml_prefix, "field"], _bblxml_norm($fullhash), name => "${namefield}fullhash");
      }
      $xml->dataElement("BDS", "${namefield}BIBNAMEHASH");
    }

    // Output extraname if there is a labelname
    if be.get_labelname_info() {
      $xml->dataElement("BDS", "EXTRANAME");
    }

    if ( crate::Config->getblxoption(None, "labelalpha", $bee, $key) ) {
      $xml->dataElement("BDS", "LABELALPHA");
    }

    $xml->dataElement("BDS", "SORTINIT");
    $xml->dataElement("BDS", "SORTINITHASH");

    // The labeldateparts option determines whether "extradate" is output
    if (crate::Config->getblxoption(None, "labeldateparts", $bee, $key)) {
      $xml->dataElement("BDS", "EXTRADATE");
      if (let $edscope = be.get_field("extradatescope")) {
        $xml->dataElement([$xml_prefix, "field"], _bblxml_norm($edscope), name => "extradatescope");
      }
      if (be.field_exists("labeldatesource")) {
        $xml->dataElement([$xml_prefix, "field"], _bblxml_norm(be.get_field("labeldatesource")), name => "labeldatesource");
      }
    }

    // labelprefix is list-specific. It is only defined if there is no shorthand
    // (see biblatex documentation)
    if !(be.get_field("shorthand")) {
      $xml->dataElement("BDS", "LABELPREFIX");
    }

    // The labeltitle option determines whether "extratitle" is output
    if (crate::Config->getblxoption(None, "labeltitle", $bee, $key)) {
      $xml->dataElement("BDS", "EXTRATITLE");
    }

    // The labeltitleyear option determines whether "extratitleyear" is output
    if (crate::Config->getblxoption(None, "labeltitleyear", $bee, $key)) {
      $xml->dataElement("BDS", "EXTRATITLEYEAR");
    }

    // The labelalpha option determines whether "extraalpha" is output
    if (crate::Config->getblxoption(None, "labelalpha", $bee, $key)) {
      $xml->dataElement("BDS", "EXTRAALPHA");
    }

    // The source field for labelname
    if ($lni) {
      $xml->dataElement([$xml_prefix, "field"], _bblxml_norm($lni), name => "labelnamesource");
    }

    // The source field for labeltitle
    if (let $lti = be.get_labeltitle_info()) {
      $xml->dataElement([$xml_prefix, "field"], _bblxml_norm($lti), name => "labeltitlesource");
    }

    if (let $ck = be.get_field("clonesourcekey")) {
      $xml->dataElement([$xml_prefix, "field"], _bblxml_norm($ck), name => "clonesourcekey");
    }

    // NOTE: already sorted
    for field in dm.get_fields_of_type(FieldType::Field,
                                                    &[DataType::Entrykey,
                                                    DataType::Key,
                                                    DataType::Integer,
                                                    DataType::Literal,
                                                    DataType::Code,
                                                    DataType::Verbatim], None) {
      let $val = be.get_field(field);
      if ( length($val) || // length() catches '0' values, which we want
          (dm.field_is_nullok(field) &&
            be.field_exists(field))) {
        if dm.field_is_skipout(field) {
          continue;
        }
        if dm.get_fieldformat(field) == "xsv" {
          continue;
        }
        // we skip outputting the crossref or xref when the parent is not cited
        // (biblatex manual, section 2.2.3)
        // sets are a special case so always output crossref/xref for them since their
        // children will always be in the .bbl otherwise they make no sense.
        if !($bee == "set") {
          if field == "crossref" && !section.has_citekey(be.get_field("crossref")) {
            continue;
          }
          if field == "xref" && !section.has_citekey(be.get_field("xref")) {
            continue;
          }
        }

        $xml->dataElement([$xml_prefix, "field"],
                          NFC($val), name => $field);
      }
    }

    // Date parts
    // NOTE: already sorted
    for field in dm.get_fields_of_type(FieldType::Field, &[DataType::Datepart], None) {
      let val = be.get_field(field);

      if ( length($val) || // length() catches '0' values, which we want
          ($dm->field_is_nullok($field) &&
            be.field_exists($field))) {
        let attrs = attribute_map!("name" => field);
        let string = if (let ($d) = $field =~ m/^(.*)(?!end)year$/) {

          // Output absolute astronomical year by default (with year 0)
          // biblatex will adjust the years when printed with BCE/CE eras
          if looks_like_number($val) {
            $val = abs($val);
          }

          // Unspecified granularity
          if (let $unspec = be.get_field("${d}dateunspecified")) {
            attrs.insert("unspecified", unspec);
          }

          // Julian dates
          if (be.get_field(format!("{d}datejulian"))) {
            attrs.insert("startjulian", "true");
          }
          if (be.get_field(format!("{d}enddatejulian"))) {
            attrs.insert("endjulian", "true");
          }

          // Circa dates
          if (be.get_field(format!("{d}dateapproximate"))) {
            attrs.insert("startcirca", "true");
          }
          if (be.get_field(format!("{d}enddateapproximate"))) {
            attrs.insert("endcirca", "true");
          }

          // Uncertain dates
          if (be.get_field(format!("{d}dateuncertain"))) {
            attrs.insert("startuncertain", "true");
          }
          if (be.get_field(format!("{d}enddateuncertain"))) {
            attrs.insert("enduncertain", "true");
          }

          // Unknown dates
          if (be.get_field(format!("{d}dateunknown"))) {
            attrs.insert("startunknown", "true");
          }
          if (be.get_field(format!("{d}enddateunknown"))) {
            attrs.insert("endunknown", "true");
          }

          // Only output era for date if:
          // The field is "year" and it came from splitting a date
          // The field is any other startyear
          if ($d == "" && be.get_field("datesplit")) {
            if (let $era = be.get_field(format!("{d}era"))) {
              attrs.insert("startera", era);
            }
            if (let $era = be.get_field(format!("{d}endera"))) {
              attrs.insert("endera", era);
            }
            _bblxml_norm(be.get_field(format!("{d}year")))
          }
          else {
            _bblxml_norm($val)
          }
        }
        else {
          _bblxml_norm($val)
        };
        $xml->dataElement([$xml_prefix, "field"], string, attrs);
      }
    }

    // XSV fields
    for field in ($dmh->{xsv}->@*) {
      if (let $f = be.get_field(field)) {
        if dm.field_is_skipout(field) {
          continue;
        }
        // keywords is by default field/xsv/keyword but it is in fact
        // output with its own special macro below
        if $field == "keywords" {
          continue;
        }
        $xml->startTag([$xml_prefix, "field"], name => $field, format => "xsv");
        for f in ($f->@*) {
          $xml->dataElement([$xml_prefix, "item"], _bblxml_norm($f));
        }
        $xml->endTag();// field
      }
    }

    for rfield in ($dmh->{ranges}->@*) {
      if ( let $rf = be.get_field(rfield) ) {
        if dm.field_is_skipout(rfield) {
          continue;
        }
        // range fields are an array ref of two-element array refs [range_start, range_end]
        // range_end can be be empty for open-ended range or None
        $xml->startTag([$xml_prefix, "range"], name => $rfield);
        for f in ($rf->@*) {
          $xml->startTag([$xml_prefix, "item"], length => rangelen($rf));
          $xml->dataElement([$xml_prefix, "start"], _bblxml_norm($f->[0]));
          if (defined($f->[1])) {
            $xml->dataElement([$xml_prefix, "end"], _bblxml_norm($f->[1]));
          }
          $xml->endTag();// item
        }
        $xml->endTag();// range
      }
    }

    // uri fields
    for uri in ($dmh->{uris}->@*) {
      if ( let $f = be.get_field(uri) ) {
        if $dm->field_is_skipout(uri) {
          continue;
        }
        $xml->dataElement([$xml_prefix, "field"], _bblxml_norm($f), name => $uri);
      }
    }

    // uri lists
    for uril in ($dmh->{urils}->@*) {
      if ( let $urilf = be.get_field(uril) ) {
        if dm.field_is_skipout(uril) {
          continue;
        }
        let %plo;
        if ( urilf->[-1].to_lowercase() == crate::Config->getoption("others_string") ) {
          $plo{$uril} = "true";
          pop $urilf->@*; // remove the last element in the array
        }
        let $total = $urilf->$#* + 1;
        $xml->startTag([$xml_prefix, "list"], name => $uril, count => $total, map {$_ => $plo{$_}} sort keys %plo);

        for f in ($urilf->@*) {
          $xml->dataElement([$xml_prefix, "item"], _bblxml_norm($f));
        }
        $xml->endTag();// list
      }
    }

    // Keywords
    if ( let $kws = be.get_field("keywords") ) {
      $xml->startTag([$xml_prefix, "keywords"]);
      for k in ($kws->@*) {
        $xml->dataElement([$xml_prefix, "keyword"], _bblxml_norm($k));
      }
      $xml->endTag();// keywords
    }


    // Output nocite boolean
    if (be.get_field("nocite")) {
      $xml->emptyTag([$xml_prefix, "nocite"]);
    }

    // Output annotations
    let ann = &crate::annotation::ANN.lock().unwrap();
    for f in (ann.get_annotated_fields(Scope::Field, key)) {
      for n in (ann.get_annotations(Scope::Field, key, f)) {
        let annot = ann.get_field_annotation(key, f, n).unwrap();
        let v = annot.value();
        let l = annot.is_literal();
        $xml->dataElement([$xml_prefix, "annotation"],
                          scope => "field",
                          field => _bblxml_norm(f),
                          name  => bblxml_norm(n),
                          literal => l,
                          value => _bblxml_norm(v)
                        );
      }
    }

    for f in (ann.get_annotated_fields(Scope::Item, key)) {
      for n in (ann.get_annotations(Scope::Item, key, f)) {
        for (c, annot) in (ann.get_annotated_items(key, f)) {
          let v = annot.value();
          let l = annot.is_literal();
          $xml->dataElement([$xml_prefix, "annotation"],
                            scope => "item",
                            field => _bblxml_norm(f),
                            name  => bblxml_norm(n),
                            literal => l,
                            item  => _bblxml_norm(c),
                            value => _bblxml_norm(v)
                          );
        }
      }
    }

    for f in (ann.get_annotated_fields(Scope::Part, key)) {
      for n in (ann.get_annotations(Scope::Part, key, f)) {
        for (c, p, annot) in (ann.get_annotated_parts(key, f)) { // n?
            let v = annot.value();
            let l = annot.is_literal();
            $xml->dataElement([$xml_prefix, "annotation"],
                              scope => "part",
                              field => _bblxml_norm(f),
                              name  => bblxml_norm(n),
                              literal => $l,
                              item  => _bblxml_norm(c),
                              part  => _bblxml_norm(p),
                              value => _bblxml_norm(v)
                            );
          }
      }
    }

    // Append any warnings to the entry, if any
    if (let $w = be.get_field("warnings")) {
      for warning in ($w->@*) {
        $xml->dataElement([$xml_prefix, "warning"], _bblxml_norm($warning));
      }
    }

  'ENDENTRY:
    $xml->endTag();// entry

    // Create an index by keyname for easy retrieval
    let $exml = $xml->end();
    // Remove NS decl as we will have this at the top level
    // This exists as we are making a new XML writer for each entry
    // which makes sense because the entries are not generated in the context
    // of the main XML due to instantiate_entry() requirements
    $exml =~ s/\sxmlns:bbl="$xml_prefix"//xms;
    $self->{output_data}{ENTRIES}{$secnum}{index}{$key} = \$exml;
  }

  /// BBL output method - this takes care to output entries in the explicit order
  /// derived from the virtual order of the citekeys after sortkey sorting.
  fn output(self) {
    let $data = $self->{output_data};
    let $dm = crate::config::get_dm();
    let $xml = $self->{output_target};
    let $xml_prefix = $self->{xml_prefix};
    let $target_string = "Target"; // Default
    if ($self->{output_target_file}) {
      $target_string = $self->{output_target_file};
    }

      debug!("Preparing final output using class {}...", __PACKAGE__);

    info!("Writing '{}' with encoding '{}'", target_string, crate::Config->getoption("output_encoding"));
    if crate::Config->getoption("output_safechars") {
      info!("Converting UTF-8 to TeX macros on output to .bbl");
    }

    for secnum in (sort keys $data->{ENTRIES}->%*) {
        debug!("Writing entries for section {}", secnum);

      $xml->startTag([$xml_prefix, "refsection"], id => $secnum);

      let section = self.get_output_section(secnum);

      let mut lists = Vec::new(); // Need to reshuffle list to put global sort order list at end, see below

      // This sort is cosmetic, just to order the lists in a predictable way in the .bbl
      // but omit the global context list so that we can add this last
      for list in (sort {a.get_sortingtemplatename() cmp b.get_sortingtemplatename()} $crate::MASTER->datalists->get_lists_for_section($secnum)->@*) {
        if (list.get_sortingtemplatename() == crate::Config->getblxoption(None, "sortingtemplatename") &&
            list.get_sortingnamekeytemplatename() == "global" &&
            list.get_labelprefix() == "" &&
            list.get_type() == "entry") {
          continue;
        }
        lists.push(list);
      }

      // biblatex requires the last list in the .bbl to be the global sort  list
      // due to its sequential reading of the .bbl as the final list overrides the
      // previously read ones and the global list determines the order of labelnumber
      // and sortcites etc. when not using defernumbers
      lists.push($crate::MASTER->datalists->get_lists_by_attrs(section => secnum,
                                                                type    => "entry",
                                                                sortingtemplatename => crate::Config->getblxoption(None, "sortingtemplatename"))->@*);

      for list in &lists {
        if list.count_keys() == 0 { // skip empty lists
          continue;
        }
        let listssn = list.get_sortingtemplatename();
        let listsnksn = list.get_sortingnamekeytemplatename();
        let listpn = list.get_labelprefix();
        let listtype = list.get_type();
        let listname = list.get_name();

          debug!("Writing entries in '{}' list of type '{}' with sortingtemplatename '{}', sort name key scheme '{}' and labelprefix '{}'", listname, listtype, listssn, listsnksn, listpn);

        $xml->startTag([$xml_prefix, "datalist"], attribute_map!("type" => listtype, "id" => listname));
        $xml->raw("\n");

        // The order of this array is the sorted order
        for k in list.get_keys() {
            debug!("Writing entry for key '{}'", k);

          let $entry = $data->{ENTRIES}{$secnum}{index}{$k};

          // Instantiate any dynamic, list specific entry information
          let $entry_string = $list->instantiate_entry($section, $entry, $k, "bblxml");

          // If requested, add a printable sorting key to the output - useful for debugging
          if (crate::Config->getoption("sortdebug")) {
            $entry_string = "      <!-- sorting key for '$k':\n           " . $list->get_sortdata_for_key($k)->[0] . " -->\n" . $entry_string;
          }

          // Now output
          // this requires UNSAFE set on the main xml writer object but
          // this is ok as the ->raw() call only adds XML written by another writer
          // which had UNSAFE=0
          $entry_string =~ s/^/      /gxms; // entries are separate docs so indent is wrong
          $xml->raw($entry_string);
        }
        $xml->raw('    ');
        $xml->endTag();    // datalist
      }

      // alias citekeys are global to a section
      for k in section.get_citekey_aliases() {
        let realkey = section.get_citekey_alias(k);
        $xml->dataElement([$xml_prefix, "keyalias"], _bblxml_norm($k), attribute_map!("key" => realkey));
      }

      // undef citekeys are global to a section
      // Missing citekeys
      for k in ($section->get_undef_citekeys) {
        $xml->dataElement([$xml_prefix, "missing"], _bblxml_norm($k));
      }

      $xml->endTag();    // refsection
    }
    $xml->endTag();    // refsection

    info!("Output to $target_string");
    $xml->end();


    let exts = DS_EXTENSIONS.values().join("|");
    let schemafile = {
      let r = Regex::new(format!(r"\.(?:{exts})$")).unwrap();
      if r.is_match(target_string) {
        r.replace(target_string, ".rng")
      }
      else {
        // in tests, there is no extension as we are using a temp file
        format!("{target_string}.rng")
      }
    };

    // Generate schema to accompany output
    if !(crate::Config->getoption("no_bblxml_schema")) {
      dm.generate_bblxml_schema(schemafile);
    }

    if (crate::Config->getoption("validate_bblxml")) {
      validate_biber_xml(target_string, "bbl", "https://sourceforge.net/projects/biblatex/bblxml", $schemafile);
    }
  }

  /// Create the output from the sections data and push it into the
  /// output object.
  fn create_output_section(self) {
    let secnum = crate::MASTER.get_current_section();
    let section = crate::MASTER.sections().get_section(secnum);

    // We rely on the order of this array for the order of the .bbl
    for k in section.get_citekeys() {
      // Regular entry
      let be = section.bibentry(k) || biber_error("Cannot find entry with key '$k' to output");
      $self->set_output_entry(be, section, crate::config::get_dm());
    }

    // Make sure the output object knows about the output section
    $self->set_output_section($secnum, $section);
  }

  fn _bblxml_norm(shift: &str) -> String {
    NFC(&normalise_string_bblxml(shift))
  }
}
