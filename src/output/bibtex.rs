/*
use parent qw(crate::Output::base);

use crate::Config;
use crate::Constants;
use crate::Utils;
use List::AllUtils qw( :all );
use Encode;
use IO::File;
use Log::Log4perl qw( :no_extra_logdie_message );
use Scalar::Util qw(looks_like_number);
use Text::Wrap;
$Text::Wrap::columns = 80;
use Unicode::Normalize;
*/

/// Class for bibtex output
pub struct BibTeX;

impl BibTeX {
  /// Set the output target file of a crate::Output::bibtex object
  /// A convenience around set_output_target so we can keep track of the
  /// filename
  fn set_output_target_file(self, outfile) {
    $self->{output_target_file} = $outfile;
    return None;
  }

  /// Set the output for a comment
  fn set_output_comment(self, comment: &str) {
    let acc = String::new();

    // Make the right casing function
    let casing = match crate::Config->getoption("output_fieldcase") {
      OutputFieldCase::Upper => |s| { s.to_uppercase() },
      OutputFieldCase::Lower => |s| { s.to_lowercase() },
      OutputFieldCase::Title => |s| { ucfirst(s) },
    };

    acc.push('@');
    acc.push_str(casing("comment"));
    acc.push_str(format!("{comment}\n"));

    self.output_data.COMMENTS.push(acc);
    return;
  }

  /// Set the output for a macro
  fn set_output_macro(self, macro) {
    let mut acc = String::new();

    // Only output used macros unless we are asked to output all
    if !(crate::Config->getoption("output_all_macrodefs")) {
      if !USEDSTRINGS.contains(macro) {
        return;
      }
    }

    // Make the right casing function
    let casing = match crate::Config->getoption("output_fieldcase") {
      OutputFieldCase::Upper => |s| { s.to_uppercase() },
      OutputFieldCase::Lower => |s| { s.to_lowercase() },
      OutputFieldCase::Title => |s| { ucfirst(s) },
    };

    acc.push('@');
    acc.push_str(casing("string"));
    acc.push_str(format!("{{{} = \"{}\"}}\n", casing(macro), Text::BibTeX::macro_text(macro));

    self.output_data.MACROS.push(acc);
  }

  /// Set the output for an entry
  fn set_output_entry(
    self,
    be: crate::Entry,
    section: crate::Section, // Section object the entry occurs in
    dm: crate::DataModel
  ) { // Data
    let bee = be.get_field("entrytype");
    let dmh = &dm.helpers;
    let mut acc = String::new();
    let secnum = section.number();
    let key = be.get_field("citekey");

    // Make the right casing/output mapping function
    let outmap = match crate::Config->getoption("output_fieldcase") {
      OutputFieldCase::Upper => |f| {CONFIG_OUTPUT_FIELDREPLACE.get(f).unwrap_or(f).to_uppercase()},
      OutputFieldCase::Lower => |f| {CONFIG_OUTPUT_FIELDREPLACE.get(f).unwrap_or(f).to_lowercase()},
      OutputFieldCase::Title => |f| {ucfirst(CONFIG_OUTPUT_FIELDREPLACE.get(f).unwrap_or(f))},
    };

    acc.push('@');
    acc.push_str(outmap(bee));
    acc.push_str(format!("\{{{key},\n"));

    // hash accumulator so we can gather all the data before formatting so that things like
    // $max_field_len can be calculated
    let %acc;

    // IDs
    if (let $val = be.get_field("ids")) {
      $acc{outmap("ids")} = val.join(",");
    }

    // Name fields
    let $tonamesub = "name_to_bibtex";
    if (crate::Config->getoption("output_xname")) {
      $tonamesub = "name_to_xname";
    }

    for namefield in ($dmh->{namelists}->@*) {
      if (let $names = be.get_field(namefield)) {

        // XDATA is special
        if !(crate::Config->getoption("output_resolve_xdata")) { // already resolved
          let xdata = nf.get_xdata();
          if !names.is_empty() {
            $acc{$outmap->($namefield)} = xdatarefout(xdata, false);
            continue;
          }
        }

        let $namesep = crate::Config->getoption("output_namesep");
        let @namelist;

        // Namelist scope useprefix
        if let Some(pref) = names.get_useprefix() {// could be 0
          namelist.push(format!("useprefix={}", map_boolean("useprefix", pref, "tostring")));
        }

        // Namelist scope sortingnamekeytemplatename
        if let Some(snks) = names.get_sortingnamekeytemplatename().skip_empty() {
          namelist.push(format!("sortingnamekeytemplatename={snks}"));
        }

        // Now add all names to accumulator
        for name in names.names() {
          // XDATA is special
          if !(crate::Config->getoption("output_resolve_xdata")) {
            let xdata = name.get_xdata();
            if !xdata.is_empty() {
              namelist.push(xdatarefout(xdata, false));
              continue;
            }
          }

          namelist.push($name->$tonamesub_;
        }

        $acc{$outmap->($namefield)} = namelist.join(&format!(" {namesep} "));

        // Deal with morenames
        if names.get_morenames() {
          $acc{$outmap->($namefield)}.push_str(format!(" {namesep} others"));
        }
      }
    }

    // List fields and verbatim list fields
    for listfield in ($dmh->{lists}->@*, $dmh->{vlists}->@*) {
      if let Some(list) = be.get_field(listfield).skip_empty() {
        let $listsep = crate::Config->getoption("output_listsep");
        let mut plainlist = Vec::new();
        for mut item in list {
          if !(crate::Config->getoption("output_resolve_xdata")) {
            let xd = xdatarefcheck(item, false);
            item = xd.unwrap_or(item);
          }
          plainlist.push(item);
        }
        $acc{$outmap->($listfield)} = plainlist.join(format!(" {listsep} "));
      }
    }

    // Per-entry options
    let mut entryoptions = Vec::new();
    for opt in (crate::Config->getblxentryoptions(secnum, key)) {
      entryoptions.push(format!("{}={}", opt, crate::Config->getblxoption(secnum, opt, None, key)));
    }
    if !entryoptions.is_empty() {
      $acc{$outmap->("options")} = entryoptions.join(",");
    }

    // Date fields
    for mut d in ($dmh->{datefields}->@*) {
      if let Some(val) = d.strip_suffix("date") {
        d = val;
      }
      if !(be.get_field(format!("{d}year"))) {
        continue;
      }

      // Output legacy dates for YEAR/MONTH if requested
      if (!$d && crate::Config->getoption("output_legacy_dates")) {
        if (let $val = be.get_field("year")) {
          if (!be.get_field("day") && !be.get_field("endyear")) {
            $acc{$outmap->("year")} = $val;
            if (let $mval = be.get_field("month")) {
              if (crate::Config->getoption("nostdmacros")) {
                $acc{$outmap->("month")} = $mval;
              }
              else {
                let %RMONTHS = reverse %MONTHS;
                $acc{$outmap->("month")} = $RMONTHS{$mval};
              }
            }
            continue;
          }
          else {
            biber_warn("Date in entry '$key' has DAY or ENDYEAR, cannot be output in legacy format.");
          }
        }
      }

      $acc{outmap(format!("{d}date"))} = construct_datetime(be, d);
    }

    // If CROSSREF and XDATA have been resolved, don't output them
    if (crate::Config->getoption("output_resolve_crossrefs")) {
      if (be.get_field("crossref")) {
        be.del_field("crossref");
      }
    }
    if (crate::Config->getoption("output_resolve_xdata")) {
      if (be.get_field("xdata")) {
        be.del_field("xdata");
      }
    }

    // Standard fields
    for field in ($dmh->{fields}->@*) {
      if let Some(val) = be.get_field(field).skip_empty() {
        if !(crate::Config->getoption("output_resolve_xdata")) {
          let xd = xdatarefcheck(val, false);
          val = xd.unwrap_or(val);
        }
        // Could have been set in dates above (MONTH, YEAR special handling)
        if !($acc{$outmap->($field)}) {
          $acc{$outmap->($field)} = val;
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
      if (let $f = be.get_field($field)) {
        let mut fl = f.join(",");
        if !(crate::Config->getoption("output_resolve_xdata")) {
          let xd = xdatarefcheck(fl, false);
          fl = xd.unwrap_or(fl);
        }
        acc{outmap(field)}.push_str(fl);
      }
    }

    // Ranges
    for rfield in ($dmh->{ranges}->@*) {
      if ( let $rf = be.get_field(rfield) ) {
        let mut rfl = construct_range(rf);
        if !(crate::Config->getoption("output_resolve_xdata")) {
          let xd = xdatarefcheck(rfl, false);
          rfl = xd.unwrap_or(rfl);
        }
        acc{outmap(rfield)}.push_str(rfl);
      }
    }

    // Verbatim fields
    for vfield in ($dmh->{vfields}->@*) {
      if ( let $vf = be.get_field(vfield) ) {
        if !(crate::Config->getoption("output_resolve_xdata")) {
          let xd = xdatarefcheck(vf, false);
          vf = xd.unwrap_or(vf);
        }
        acc{outmap(vfield)} = vf;
      }
    }

    // Keywords
    if ( let $k = be.get_field("keywords") ) {
      let mut kl = k.join(",");
      if !(crate::Config->getoption("output_resolve_xdata")) {
        let xd = xdatarefcheck(kl, false);
        kl = xd.unwrap_or(kl);
      }
      acc{outmap("keywords")} = kl;
    }

    // Annotations
    let ann = &crate::annotation::ANN.lock().unwrap();
    for f in acc.keys() {
      if ann.is_annotated_field(key, f.to_lowercase()) {
        for n in ann.get_annotation_names(key, f.to_lowercase()) {
          acc.insert(
            &format!("{}{}{}{n}", outmap(f), crate::Config->getoption("output_annotation_marker"), crate::Config->getoption("output_named_annotation_marker")),
            construct_annotation(ann, key, f.to_lowercase(), n)
          );
        }
      }
    }

    // Determine maximum length of field names
    let max_field_len;
    if (crate::Config->getoption("output_align")) {
      max_field_len = acc.keys().map(|k| k.graphemes(true).count()).max();
    }

    // Determine order of fields
    let %classmap = ("names"     => "namelists",
                    "lists"     => "lists",
                    "dates"     => "datefields");


    for field in regex!(r"\s*,\s*").split(crate::Config->getoption("output_field_order")) {
      if (field == "names" || field == "lists" || field == "dates") {
        let mut donefields = Vec::new();
        for key in acc.keys().sorted() {
          if (first {unicase::eq($_, strip_annotation($key))} $dmh->{$classmap{$field}}->@*) {
            acc.push_str(bibfield(key, $acc{$key}, max_field_len));
            donefields.push(key);
          }
        }
        delete @acc{@donefields};
      }
      else if (let $value = delete $acc{$outmap->($field)}) {
        acc.push_str(bibfield($outmap->($field), $value, max_field_len));
      }
    }

    // Now rest of fields not explicitly specified
    for field in acc.keys().sorted() {
      acc.push_str(bibfield(field, $acc{$field}, max_field_len));
    }

    acc.push_str("}\n\n");

    // If requested to convert UTF-8 to macros ...
    if (crate::Config->getoption("output_safechars")) {
      $acc = latex_recode_output($acc);
    }
    else { // ... or, check for encoding problems and force macros
      let $outenc = crate::Config->getoption("output_encoding");
      if ($outenc != "UTF-8") {
        // Can this entry be represented in the output encoding?
        if (encode($outenc, NFC($acc)) =~ /\?/) { // Malformed data encoding char
          // So convert to macro
          $acc = latex_recode_output($acc);
          biber_warn("The entry '$key' has characters which cannot be encoded in '$outenc'. Recoding problematic characters into macros.");
        }
      }
    }

    // Create an index by keyname for easy retrieval
    $self->{output_data}{ENTRIES}{$secnum}{index}{$key} = \$acc;

    return;
  }

  /// output method
  fn output(self) {
    let $data = $self->{output_data};
    let $target = $self->{output_target};

    let $target_string = "Target"; // Default
    if ($self->{output_target_file}) {
      $target_string = $self->{output_target_file};
    }

    // Instantiate output file now that input is read in case we want to do in-place
    // output for tool mode
    let $enc_out;
    if (crate::Config->getoption("output_encoding")) {
      $enc_out = ':encoding(' . crate::Config->getoption("output_encoding") . ')';
    }

    if ($target_string == '-') {
      $target = new IO::File ">-$enc_out";
    }
    else {
      $target = IO::File->new($target_string, ">$enc_out");
    }

      debug!("Preparing final output using class {}...", __PACKAGE__);

    info!("Writing '{}' with encoding '{}'", target_string, crate::Config->getoption("output_encoding"));
    if crate::Config->getoption("output_safechars") {
      info!("Converting UTF-8 to TeX macros on output");
    }

    out($target, $data->{HEAD});

    // Output any macros when in tool mode
    if (crate::Config->getoption("tool")) {
      if (exists($data->{MACROS})) {
        for macro in (sort $data->{MACROS}->@*) {
          out($target, $macro);
        }
        out($target, "\n"); // Extra newline between macros and entries, for clarity
      }
    }

      debug!("Writing entries in bibtex format");

    // Bibtex output uses just one special section, always sorted by global sorting spec
    for key in ($crate::MASTER->datalists->get_lists_by_attrs(section => 99999,
                                                                  name => crate::Config->getblxoption(None, "sortingtemplatename") . "/global//global/global",
                                                                  type => "entry",
                                                                  sortingtemplatename => crate::Config->getblxoption(None, "sortingtemplatename"),
                                                                  sortingnamekeytemplatename => "global",
                                                                  labelprefix => "",
                                                                  uniquenametemplatename => "global",
                                                                  labelalphanametemplatename => "global")->[0].get_keys()) {
      out($target, ${$data->{ENTRIES}{99999}{index}{$key}});
    }

    // Output any comments when in tool mode
    if (crate::Config->getoption("tool")) {
      for comment in ($data->{COMMENTS}->@*) {
        out($target, $comment);
      }
    }

    out($target, $data->{TAIL});

    info!("Output to {}", target_string);
    close $target;
    return;
  }

  /// Create the output from the sections data and push it into the
  /// output object.
  fn create_output_section(self) {
    let secnum = crate::MASTER.get_current_section();
    let section = crate::MASTER.sections().get_section(secnum);

    // We rely on the order of this array for the order of the .bib
    for k in section.get_citekeys() {
      // Regular entry
      let be = section.bibentry(k) || biber_error("Cannot find entry with key '$k' to output");
      $self->set_output_entry(be, section, crate::config::get_dm());
    }

    // Create the comments output
    for comment in ($crate::MASTER->{comments}->@*) {
      $self->set_output_comment($comment);
    }

    // Create the macros output unless suppressed. This has to come after entry output creation
    // above as this gather information on which macros were actually used
    if !(crate::Config->getoption("output_no_macrodefs")) {
      for m in (sort values %RSTRINGS) {
        $self->set_output_macro($m);
      }
    }

    // Make sure the output object knows about the output section
    $self->set_output_section($secnum, $section);

    return;
  }

  /// Format a single field
  fn bibfield(field, value, max_field_len) {
    let mut acc = String::new();
    let inum = crate::Config->getoption("output_indent");
    let mut ichar = ' ';
    if (substr($inum, -1) == 't') {
      $ichar = '\t';
      $inum = substr($inum, 0, length($inum)-1);
    }
    acc.push_str(&ichar.repeat(inum));
    acc.push_str(field);
    if $max_field_len {
      acc.push_str(' '.repeat(max_field_len - field.graphemes(true).count()));
    }
    acc.push_str(" = ");

    // Is the field value a macro? If so, replace with macro
    if (let $m = $RSTRINGS{$value}) {
      // Make the right casing function
      let casing = match crate::Config->getoption("output_fieldcase") {
        OutputFieldCase::Upper => |s| { s.to_uppercase() },
        OutputFieldCase::Lower => |s| { s.to_lowercase() },
        OutputFieldCase::Title => |s| { ucfirst(s) },
      };

      $value = casing(m);
      USEDSTRINGS.insert(m);
    }

    // Don't wrap fields which should be macros in braces - we can only deal with macros
    // which are the whole field value - too messy to check for part values and this is better
    // handled with XDATA anyway.
    // Don't check %RSTRINGS here as macros can come from other places (like %MONTHS). Just check
    // whether a macro is defined as that covers all sources
    if (Text::BibTeX::macro_length($value)) {
      acc.push_str(&format!("{value},\n"));
    } else {
      acc.push_str(format!("\{{{value}\}},\n"));
    }
    acc
  }

  /// Construct a field annotation
  fn construct_annotation(ann: &crate::annotation::Ann, key: &str, field: &str, name: &str) -> String {
    let mut annotations = Vec::new();

    if let Some(fa) = ann.get_field_annotation(key, field, name).map(|ann| ann.value()).skip_empty() {
       annotations.push(format!("={fa}"));
    }

    for (item, annot) in (ann.get_annotated_items(key, field, name)) {
      annotations.push(format!("{item}={}", annot.value()));
    }

    for (item, part, annot) in (ann.get_annotated_parts(key, field, name)) {
      annotations.push(format!("{item}:{part}={}", annot.value()));
    }

    return annotations.join(";");
  }

  /// Construct a range field from its components
  ///
  /// ```
  /// [m, n]      -> m-n
  /// [m, None]  -> m
  /// [m, ""]     -> m-
  /// ["", n]     -> -n
  /// ["", None] -> ignore
  /// ```
  fn construct_range(r: &[&str]) -> String {
    let mut ranges = Vec::new();
    for e in r {
      let rs = e[0];
      if let Some(e1) = e.get(1) {
        rs.push_str(--);
        rs.push_str(e1);
      }
      ranges.push(rs);
    }
    ranges.join(",")
  }

  /// Construct a datetime from its components
  fn construct_datetime(be, d) {
    let $datestring = "";
    let mut overridey = None;
    let $overridem;
    let $overrideem;
    let $overrided;

    let %yeardivisions = ( "spring"  => 21,
                          "summer"  => 22,
                          "autumn"  => 23,
                          "winter"  => 24,
                          "springN" => 25,
                          "summerN" => 26,
                          "autumnN" => 27,
                          "winterN" => 28,
                          "springS" => 29,
                          "summerS" => 30,
                          "autumnS" => 31,
                          "WinterS" => 32,
                          "Q1"      => 33,
                          "Q2"      => 34,
                          "Q3"      => 35,
                          "Q4"      => 36,
                          "QD1"     => 37,
                          "QD2"     => 38,
                          "QD3"     => 39,
                          "S1"      => 40,
                          "S2"      => 41 );

    // Did the date fields come from interpreting an ISO8601-2:2016 unspecified date?
    // If so, do the reverse of crate::Utils::parse_date_unspecified()
    if (let $unspec = be.get_field(&format!("{d}dateunspecified"))) {

      match unspec {
        // 1990/1999 -> 199X
        "yearindecade" => {
          let (_, decade) = regex_captures!(r"^(\d+)\d$", be.get_field(&format!("{d}year"))).unwrap();
          overridey = Some(format!("{decade}X"));
          be.del_field(&format!("{d}endyear"));
        }
        // 1900/1999 -> 19XX
        "yearincentury" => {
          let (_, century) = regex_captures!(r"^(\d+)\d\d$", be.get_field(&format!("{d}year"))).unwrap();
          overridey = Some(format!("{century}XX"));
          be.del_field(&format!("{d}endyear"));
        }
        // 1999-01/1999-12 => 1999-XX
        "monthinyear" => {
          overridem = "XX";
          be.del_field(&format!("{d}endyear"));
          be.del_field(&format!("{d}endmonth"));
        }
        // 1999-01-01/1999-01-31 -> 1999-01-XX
        "dayinmonth" => {
          overrided = "XX";
          be.del_field(&format!("{d}endyear"));
          be.del_field(&format!("{d}endmonth"));
          be.del_field(&format!("{d}endday"));
        }
        // 1999-01-01/1999-12-31 -> 1999-XX-XX
        "dayinyear" => {
          $overridem = "XX";
          $overrided = "XX";
          be.del_field(&format!("{d}endyear"));
          be.del_field(&format!("{d}endmonth"));
          be.del_field(&format!("{d}endday"));
        }
        _ => {}
      }
    }

    // Seasons derived from ISO 8601 dates
    if (let $s = be.get_field(&format!("{d}yeardivision"))) {
      $overridem = $yeardivisions{$s};
    }
    if (let $s = be.get_field(&format!("{d}endyeardivision"))) {
      $overrideem = $yeardivisions{$s};
    }

    // date exists if there is a start year
    if (let $sy = $overridey || be.get_field(&format!("{d}year")) ) {
      datestring.push_str(sy);
      be.del_field(&format!("{d}year"));

      // Start month
      if (let $sm = $overridem || be.get_field(&format!("{d}month"))) {
        datestring.push_str(&format!("-{sm:02}")); // TODO: parse int
        be.del_field(&format!("{d}month"));
      }

      // Start day
      if (let $sd = $overrided || be.get_field(&format!("{d}day"))) {
        datestring.push_str(&format!("-{sd:02}")); // TODO: parse int
        be.del_field(&format!("{d}day"));
      }

      // Uncertain and approximate start date
      if (be.get_field(&format!("{d}dateuncertain")) &&
          be.get_field(&format!("{d}dateapproximate"))) {
        datestring.push('%');
      }
      else {
        // Uncertain start date
        if (be.get_field(&format!("{d}dateuncertain"))) {
          datestring.push('?');
        }

        // Approximate start date
        if (be.get_field(&format!("{d}dateapproximate"))) {
          datestring.push_str('~');
        }
      }

      // If start hour, there must be minute and second
      if (let $sh = be.get_field(&format!("{d}hour"))) {
        datestring.push_str(&format!(
          "T{sh:02}:{:02}:{:02}",
          be.get_field(&format!("{d}minute")),
          be.get_field(&format!("{d}second"))
        ));
        be.del_field(&format!("{d}hour"));
        be.del_field(&format!("{d}minute"));
        be.del_field(&format!("{d}second"));
      }

      // start timezone
      if (let $stz = be.get_field(&format!("{d}timezone"))) {
        stz = regex_replace!(r"\\bibtzminsep\s+", stz, ":");
        datestring.push_str(stz);
        be.del_field(&format!("{d}timezone"));
      }

      // End year, can be empty
      if be.field_exists(&format!("{d}endyear")) {
        datestring.push('/');
      }

      // End year
      if (let $ey = be.get_field(&format!("{d}endyear"))) {
        datestring.push_str(ey);
        be.del_field(&format!("{d}endyear"));

        // End month
        if (let $em = $overrideem || be.get_field(&format!("{d}endmonth"))) {
          datestring.push_str(&format!("-{em:02}")); // TODO: parse int
          be.del_field(&format!("{d}endmonth"));
        }

        // End day
        if (let $ed = be.get_field(&format!("{d}endday"))) {
          datestring.push_str(&format!("-{ed:02}"));
          be.del_field(&format!("{d}endday"));
        }

        // Uncertain and approximate end date
        if (be.get_field(&format!("{d}enddateuncertain")) &&
            be.get_field(&format!("{d}enddateapproximate"))) {
          datestring.push('%');
        }
        else {
          // Uncertain end date
          if (be.get_field(&format!("{d}enddateuncertain"))) {
            datestring.push('?');
          }

          // Approximate end date
          if (be.get_field(&format!("{d}enddateapproximate"))) {
            datestring.push_str('~');
          }
        }

        // If end hour, there must be minute and second
        if (let $eh = be.get_field(&format!("{d}endhour"))) {
          datestring.push_str(&format!("T{eh:02}{:02}{:02}", be.get_field(&format!("{d}endminute")), be.get_field(&format!("{d}endsecond"))));
          be.del_field(&format!("{d}endhour"));
          be.del_field(&format!("{d}endminute"));
          be.del_field(&format!("{d}endsecond"));
        }

        // end timezone
        if (let $etz = be.get_field(&format!("{d}endtimezone"))) {
          etz = regex_replace!(r"\\bibtzminsep\s+", etz, ":");
          datestring.push_str(etz);
          be.del_field(&format!("{d}endtimezone"));
        }
      }
    }
    return $datestring;
  }
}
