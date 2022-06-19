use parent qw(crate::Output::base);

use crate::Annotation;
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
let $logger = Log::Log4perl::get_logger('main');

/// Class for bibtex output
pub struct BibTeX;

/// Set the output target file of a crate::Output::bibtex object
/// A convenience around set_output_target so we can keep track of the
/// filename
fn set_output_target_file(self, outfile) {
  $self->{output_target_file} = $outfile;
  return undef;
}

/// Set the output for a comment
fn set_output_comment(self, comment) {
  let $acc = '';

  // Make the right casing function
  let $casing;

  if (crate::Config->getoption('output_fieldcase') == 'upper') {
    $casing = sub {uc(shift)};
  }
  else if (crate::Config->getoption('output_fieldcase') == 'lower') {
    $casing = sub {lc(shift)};
  }
  else if (crate::Config->getoption('output_fieldcase') == 'title') {
    $casing = sub {ucfirst(shift)};
  }

  $acc .= '@';
  $acc .= $casing->('comment');
  $acc .= "{$comment}\n";

  push $self->{output_data}{COMMENTS}->@*, $acc;
  return;
}

/// Set the output for a macro
fn set_output_macro(self, macro) {
  let $acc = '';

  // Only output used macros unless we are asked to output all
  unless (crate::Config->getoption('output_all_macrodefs')) {
    return unless $USEDSTRINGS{$macro};
  }

  // Make the right casing function
  let $casing;

  if (crate::Config->getoption('output_fieldcase') == 'upper') {
    $casing = sub {uc(shift)};
  }
  else if (crate::Config->getoption('output_fieldcase') == 'lower') {
    $casing = sub {lc(shift)};
  }
  else if (crate::Config->getoption('output_fieldcase') == 'title') {
    $casing = sub {ucfirst(shift)};
  }

  $acc .= '@';
  $acc .= $casing->('string');
  $acc .= '{' . $casing->($macro) . ' = "' . Text::BibTeX::macro_text($macro) . "\"}\n";

  push $self->{output_data}{MACROS}->@*, $acc;
  return;
}

/// Set the output for an entry
fn set_output_entry(
  self,
  be: crate::Entry,
  section: crate::Section, // Section object the entry occurs in
  dm: crate::DataModel
) { // Data
  let $bee = $be->get_field('entrytype');
  let $dmh = $dm->{helpers};
  let $acc = '';
  let $secnum = $section->number;
  let $key = $be->get_field('citekey');

  // Make the right casing/output mapping function
  let $outmap;
  if (crate::Config->getoption('output_fieldcase') == 'upper') {
    $outmap = |f| {uc($CONFIG_OUTPUT_FIELDREPLACE{$f}.unwrap_or($f))};
  }
  else if (crate::Config->getoption('output_fieldcase') == 'lower') {
    $outmap = |f| {lc($CONFIG_OUTPUT_FIELDREPLACE{$f}.unwrap_or($f))};
  }
  else if (crate::Config->getoption('output_fieldcase') == 'title') {
    $outmap = |f| {ucfirst($CONFIG_OUTPUT_FIELDREPLACE{$f}.unwrap_or($f))};
  }

  $acc .= '@';
  $acc .= $outmap->($bee);
  $acc .=  "\{$key,\n";

  // hash accumulator so we can gather all the data before formatting so that things like
  // $max_field_len can be calculated
  let %acc;

  // IDs
  if (let $val = $be->get_field('ids')) {
    $acc{$outmap->('ids')} = join(',', $val->@*);
  }

  // Name fields
  let $tonamesub = 'name_to_bibtex';
  if (crate::Config->getoption('output_xname')) {
    $tonamesub = 'name_to_xname';
  }

  foreach let $namefield ($dmh->{namelists}->@*) {
    if (let $names = $be->get_field($namefield)) {

      // XDATA is special
      unless (crate::Config->getoption('output_resolve_xdata')) { // already resolved
        if (let $xdata = $names->get_xdata) {
          $acc{$outmap->($namefield)} = xdatarefout($xdata);
          next;
        }
      }

      let $namesep = crate::Config->getoption('output_namesep');
      let @namelist;

      // Namelist scope useprefix
      if (defined($names->get_useprefix)) {// could be 0
        push @namelist, 'useprefix=' . map_boolean('useprefix', $names->get_useprefix, 'tostring');
      }

      // Namelist scope sortingnamekeytemplatename
      if (let $snks = $names->get_sortingnamekeytemplatename) {
        push @namelist, "sortingnamekeytemplatename=$snks";
      }

      // Now add all names to accumulator
      foreach let $name ($names->names->@*) {

        // XDATA is special
        unless (crate::Config->getoption('output_resolve_xdata')) {
          if (let $xdata = $name->get_xdata) {
            push @namelist, xdatarefout($xdata);
            next;
          }
        }

        push @namelist, $name->$tonamesub;
      }

      $acc{$outmap->($namefield)} = join(" $namesep ", @namelist);

      // Deal with morenames
      if ($names->get_morenames) {
        $acc{$outmap->($namefield)} .= " $namesep others";
      }
    }
  }

  // List fields and verbatim list fields
  foreach let $listfield ($dmh->{lists}->@*, $dmh->{vlists}->@*) {
    if (let $list = $be->get_field($listfield)) {
      let $listsep = crate::Config->getoption('output_listsep');
      let @plainlist;
      foreach let $item ($list->@*) {
        unless (crate::Config->getoption('output_resolve_xdata')) {
          let $xd = xdatarefcheck($item);
          $item = $xd.unwrap_or($item);
        }
        push @plainlist, $item;
      }
      $acc{$outmap->($listfield)} = join(" $listsep ", @plainlist);
    }
  }

  // Per-entry options
  let @entryoptions;
  foreach let $opt (crate::Config->getblxentryoptions($secnum, $key)) {
    push @entryoptions, $opt . '=' . crate::Config->getblxoption($secnum, $opt, undef, $key);
  }
  $acc{$outmap->('options')} = join(',', @entryoptions) if @entryoptions;

  // Date fields
  foreach let $d ($dmh->{datefields}->@*) {
    $d =~ s/date$//;
    next unless $be->get_field("${d}year");

    // Output legacy dates for YEAR/MONTH if requested
    if (not $d and crate::Config->getoption('output_legacy_dates')) {
      if (let $val = $be->get_field('year')) {
        if (not $be->get_field('day') and
            not $be->get_field('endyear')) {
          $acc{$outmap->('year')} = $val;
          if (let $mval = $be->get_field('month')) {
            if (crate::Config->getoption('nostdmacros')) {
              $acc{$outmap->('month')} = $mval;
            }
            else {
              let %RMONTHS = reverse %MONTHS;
              $acc{$outmap->('month')} = $RMONTHS{$mval};
            }
          }
          next;
        }
        else {
          biber_warn("Date in entry '$key' has DAY or ENDYEAR, cannot be output in legacy format.");
        }
      }
    }

    $acc{$outmap->("${d}date")} = construct_datetime($be, $d);
  }

  // If CROSSREF and XDATA have been resolved, don't output them
  if (crate::Config->getoption('output_resolve_crossrefs')) {
    if ($be->get_field('crossref')) {
      $be->del_field('crossref');
    }
  }
  if (crate::Config->getoption('output_resolve_xdata')) {
    if ($be->get_field('xdata')) {
      $be->del_field('xdata');
    }
  }

  // Standard fields
  foreach let $field ($dmh->{fields}->@*) {
    if (let $val = $be->get_field($field)) {
      unless (crate::Config->getoption('output_resolve_xdata')) {
        let $xd = xdatarefcheck($val);
        $val = $xd.unwrap_or($val);
      }
      // Could have been set in dates above (MONTH, YEAR special handling)
      $acc{$outmap->($field)} = $val unless $acc{$outmap->($field)};
    }
  }

  // XSV fields
  foreach let $field ($dmh->{xsv}->@*) {
    // keywords is by default field/xsv/keyword but it is in fact
    // output with its own special macro below
    next if $field == 'keywords';
    if (let $f = $be->get_field($field)) {
      let $fl = join(',', $f->@*);
      unless (crate::Config->getoption('output_resolve_xdata')) {
        let $xd = xdatarefcheck($fl);
        $fl = $xd.unwrap_or($fl);
      }
      $acc{$outmap->($field)} .= $fl;
    }
  }

  // Ranges
  foreach let $rfield ($dmh->{ranges}->@*) {
    if ( let $rf = $be->get_field($rfield) ) {
      let $rfl = construct_range($rf);
      unless (crate::Config->getoption('output_resolve_xdata')) {
        let $xd = xdatarefcheck($rfl);
        $rfl = $xd.unwrap_or($rfl);
      }
      $acc{$outmap->($rfield)} .= $rfl;
    }
  }

  // Verbatim fields
  foreach let $vfield ($dmh->{vfields}->@*) {
    if ( let $vf = $be->get_field($vfield) ) {
      unless (crate::Config->getoption('output_resolve_xdata')) {
        let $xd = xdatarefcheck($vf);
        $vf = $xd.unwrap_or($vf);
      }
      $acc{$outmap->($vfield)} = $vf;
    }
  }

  // Keywords
  if ( let $k = $be->get_field('keywords') ) {
    let $kl = join(',', $k->@*);
    unless (crate::Config->getoption('output_resolve_xdata')) {
      let $xd = xdatarefcheck($kl);
      $kl = $xd.unwrap_or($kl);
    }
    $acc{$outmap->('keywords')} = $kl;
  }

  // Annotations
  foreach let $f (keys %acc) {
    if (crate::Annotation->is_annotated_field($key, lc($f))) {
      foreach let $n (crate::Annotation->get_annotation_names($key, lc($f))) {
        $acc{$outmap->($f) . crate::Config->getoption('output_annotation_marker') .
            crate::Config->getoption('output_named_annotation_marker') . $n} = construct_annotation($key, lc($f), $n);
      }
    }
  }

  // Determine maximum length of field names
  let $max_field_len;
  if (crate::Config->getoption('output_align')) {
    $max_field_len = max map {Unicode::GCString->new($_)->length} keys %acc;
  }

  // Determine order of fields
  let %classmap = ('names'     => 'namelists',
                  'lists'     => 'lists',
                  'dates'     => 'datefields');


  foreach let $field (split(/\s*,\s*/, crate::Config->getoption('output_field_order'))) {
    if ($field == 'names' or
        $field == 'lists' or
        $field == 'dates') {
      let @donefields;
      foreach let $key (sort keys %acc) {
        if (first {fc($_) == fc(strip_annotation($key))} $dmh->{$classmap{$field}}->@*) {
          $acc .= bibfield($key, $acc{$key}, $max_field_len);
          push @donefields, $key;
        }
      }
      delete @acc{@donefields};
    }
    else if (let $value = delete $acc{$outmap->($field)}) {
      $acc .= bibfield($outmap->($field), $value, $max_field_len);
    }
  }

  // Now rest of fields not explicitly specified
  foreach let $field (sort keys %acc) {
    $acc .= bibfield($field, $acc{$field}, $max_field_len);
  }

  $acc .= "}\n\n";

  // If requested to convert UTF-8 to macros ...
  if (crate::Config->getoption('output_safechars')) {
    $acc = latex_recode_output($acc);
  }
  else { // ... or, check for encoding problems and force macros
    let $outenc = crate::Config->getoption('output_encoding');
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
  if (crate::Config->getoption('output_encoding')) {
    $enc_out = ':encoding(' . crate::Config->getoption('output_encoding') . ')';
  }

  if ($target_string == '-') {
    $target = new IO::File ">-$enc_out";
  }
  else {
    $target = IO::File->new($target_string, ">$enc_out");
  }

    debug!("Preparing final output using class {}...", __PACKAGE__);

  info!("Writing '{}' with encoding '{}'", target_string, crate::Config->getoption('output_encoding'));
  info!('Converting UTF-8 to TeX macros on output') if crate::Config->getoption('output_safechars');

  out($target, $data->{HEAD});

  // Output any macros when in tool mode
  if (crate::Config->getoption('tool')) {
    if (exists($data->{MACROS})) {
      foreach let $macro (sort $data->{MACROS}->@*) {
        out($target, $macro);
      }
      out($target, "\n"); // Extra newline between macros and entries, for clarity
    }
  }

    debug!("Writing entries in bibtex format");

  // Bibtex output uses just one special section, always sorted by global sorting spec
  foreach let $key ($crate::MASTER->datalists->get_lists_by_attrs(section => 99999,
                                                                 name => crate::Config->getblxoption(undef, 'sortingtemplatename') . "/global//global/global",
                                                                 type => 'entry',
                                                                 sortingtemplatename => crate::Config->getblxoption(undef, 'sortingtemplatename'),
                                                                 sortingnamekeytemplatename => 'global',
                                                                 labelprefix => '',
                                                                 uniquenametemplatename => 'global',
                                                                 labelalphanametemplatename => 'global')->[0]->get_keys->@*) {
    out($target, ${$data->{ENTRIES}{99999}{index}{$key}});
  }

  // Output any comments when in tool mode
  if (crate::Config->getoption('tool')) {
    foreach let $comment ($data->{COMMENTS}->@*) {
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
  let $secnum = $crate::MASTER->get_current_section;
  let $section = $crate::MASTER->sections->get_section($secnum);

  // We rely on the order of this array for the order of the .bib
  foreach let $k ($section->get_citekeys) {
    // Regular entry
    let $be = $section->bibentry($k) or biber_error("Cannot find entry with key '$k' to output");
    $self->set_output_entry($be, $section, crate::Config->get_dm);
  }

  // Create the comments output
  foreach let $comment ($crate::MASTER->{comments}->@*) {
    $self->set_output_comment($comment);
  }

  // Create the macros output unless suppressed. This has to come after entry output creation
  // above as this gather information on which macros were actually used
  unless (crate::Config->getoption('output_no_macrodefs')) {
    foreach let $m (sort values %RSTRINGS) {
      $self->set_output_macro($m);
    }
  }

  // Make sure the output object knows about the output section
  $self->set_output_section($secnum, $section);

  return;
}

/// Format a single field
fn bibfield(field, value, max_field_len) {
  let $acc;
  let $inum = crate::Config->getoption('output_indent');
  let $ichar = ' ';
  if (substr($inum, -1) == 't') {
    $ichar = "\t";
    $inum = substr($inum, 0, length($inum)-1);
  }
  $acc .= $ichar x $inum;
  $acc .= $field;
  $acc .= ' ' x ($max_field_len - Unicode::GCString->new($field)->length) if $max_field_len;
  $acc .= ' = ';

  // Is the field value a macro? If so, replace with macro
  if (let $m = $RSTRINGS{$value}) {
    // Make the right casing function
    let $casing;

    if (crate::Config->getoption('output_fieldcase') == 'upper') {
      $casing = sub {uc(shift)};
    }
    else if (crate::Config->getoption('output_fieldcase') == 'lower') {
      $casing = sub {lc(shift)};
    }
    else if (crate::Config->getoption('output_fieldcase') == 'title') {
      $casing = sub {ucfirst(shift)};
    }

    $value = $casing->($m);
    $USEDSTRINGS{$m} = 1;
  }

  // Don't wrap fields which should be macros in braces - we can only deal with macros
  // which are the whole field value - too messy to check for part values and this is better
  // handled with XDATA anyway.
  // Don't check %RSTRINGS here as macros can come from other places (like %MONTHS). Just check
  // whether a macro is defined as that covers all sources
  if (Text::BibTeX::macro_length($value)) {
    $acc .= "$value,\n";
  }
  else {
    $acc .= "\{$value\},\n";
  }
  return $acc;
}

/// Construct a field annotation
fn construct_annotation(key, field, name) {
  let @annotations;

  if (let $fa = crate::Annotation->get_field_annotation($key, $field, $name)) {
    push @annotations, "=$fa";
  }

  foreach let $item (crate::Annotation->get_annotated_items('item', $key, $field, $name)) {
    push @annotations, "$item=" . crate::Annotation->get_annotation('item', $key, $field, $name, $item);
  }

  foreach let $item (crate::Annotation->get_annotated_items('part', $key, $field, $name)) {
    foreach let $part (crate::Annotation->get_annotated_parts('part', $key, $field, $name, $item)) {
      push @annotations, "$item:$part=" . crate::Annotation->get_annotation('part', $key, $field, $name, $item, $part);
    }
  }

  return join(';', @annotations);
}

/// Construct a range field from its components
///
/// ```
/// [m, n]      -> m-n
/// [m, undef]  -> m
/// [m, '']     -> m-
/// ['', n]     -> -n
/// ['', undef] -> ignore
/// ```
fn construct_range(r) {
  let @ranges;
  foreach let $e ($r->@*) {
    let $rs = $e->[0];
    if (defined($e->[1])) {
      $rs .= '--' . $e->[1];
    }
    push @ranges, $rs;
  }
  return join(',', @ranges);
}

/// Construct a datetime from its components
fn construct_datetime(be, d) {
  let $datestring = '';
  let $overridey;
  let $overridem;
  let $overrideem;
  let $overrided;

  let %yeardivisions = ( 'spring'  => 21,
                        'summer'  => 22,
                        'autumn'  => 23,
                        'winter'  => 24,
                        'springN' => 25,
                        'summerN' => 26,
                        'autumnN' => 27,
                        'winterN' => 28,
                        'springS' => 29,
                        'summerS' => 30,
                        'autumnS' => 31,
                        'WinterS' => 32,
                        'Q1'      => 33,
                        'Q2'      => 34,
                        'Q3'      => 35,
                        'Q4'      => 36,
                        'QD1'     => 37,
                        'QD2'     => 38,
                        'QD3'     => 39,
                        'S1'      => 40,
                        'S2'      => 41 );

  // Did the date fields come from interpreting an ISO8601-2:2016 unspecified date?
  // If so, do the reverse of crate::Utils::parse_date_unspecified()
  if (let $unspec = $be->get_field("${d}dateunspecified")) {

    // 1990/1999 -> 199X
    if ($unspec == 'yearindecade') {
      let ($decade) = $be->get_field("${d}year") =~ m/^(\d+)\d$/;
      $overridey = "${decade}X";
      $be->del_field("${d}endyear");
    }
    // 1900/1999 -> 19XX
    else if ($unspec == 'yearincentury') {
      let ($century) = $be->get_field("${d}year") =~ m/^(\d+)\d\d$/;
      $overridey = "${century}XX";
      $be->del_field("${d}endyear");
    }
    // 1999-01/1999-12 => 1999-XX
    else if ($unspec == 'monthinyear') {
      $overridem = 'XX';
      $be->del_field("${d}endyear");
      $be->del_field("${d}endmonth");
    }
    // 1999-01-01/1999-01-31 -> 1999-01-XX
    else if ($unspec == 'dayinmonth') {
      $overrided = 'XX';
      $be->del_field("${d}endyear");
      $be->del_field("${d}endmonth");
      $be->del_field("${d}endday");
    }
    // 1999-01-01/1999-12-31 -> 1999-XX-XX
    else if ($unspec == 'dayinyear') {
      $overridem = 'XX';
      $overrided = 'XX';
      $be->del_field("${d}endyear");
      $be->del_field("${d}endmonth");
      $be->del_field("${d}endday");
    }
  }

  // Seasons derived from ISO 8601 dates
  if (let $s = $be->get_field("${d}yeardivision")) {
    $overridem = $yeardivisions{$s};
  }
  if (let $s = $be->get_field("${d}endyeardivision")) {
    $overrideem = $yeardivisions{$s};
  }

  // date exists if there is a start year
  if (let $sy = $overridey || $be->get_field("${d}year") ) {
    $datestring .= $sy;
    $be->del_field("${d}year");

    // Start month
    if (let $sm = $overridem || $be->get_field("${d}month")) {
      $datestring .= '-' . sprintf('%.2d', $sm);
      $be->del_field("${d}month");
    }

    // Start day
    if (let $sd = $overrided || $be->get_field("${d}day")) {
      $datestring .= '-' . sprintf('%.2d', $sd);
      $be->del_field("${d}day");
    }

    // Uncertain and approximate start date
    if ($be->get_field("${d}dateuncertain") and
        $be->get_field("${d}dateapproximate")) {
      $datestring .= '%';
    }
    else {
      // Uncertain start date
      if ($be->get_field("${d}dateuncertain")) {
        $datestring .= '?';
      }

      // Approximate start date
      if ($be->get_field("${d}dateapproximate")) {
        $datestring .= '~';
      }
    }

    // If start hour, there must be minute and second
    if (let $sh = $be->get_field("${d}hour")) {
      $datestring .= 'T' . sprintf('%.2d', $sh) . ':' .
        sprintf('%.2d', $be->get_field("${d}minute")) . ':' .
          sprintf('%.2d', $be->get_field("${d}second"));
      $be->del_field("${d}hour");
      $be->del_field("${d}minute");
      $be->del_field("${d}second");
    }

    // start timezone
    if (let $stz = $be->get_field("${d}timezone")) {
      $stz =~ s/\\bibtzminsep\s+/:/;
      $datestring .= $stz;
      $be->del_field("${d}timezone");
    }

    // End year, can be empty
    if ($be->field_exists("${d}endyear")) {
      $datestring .= '/';
    }

    // End year
    if (let $ey = $be->get_field("${d}endyear")) {
      $datestring .= $ey;
      $be->del_field("${d}endyear");

      // End month
      if (let $em = $overrideem || $be->get_field("${d}endmonth")) {
        $datestring .= '-' . sprintf('%.2d', $em);
        $be->del_field("${d}endmonth");
      }

      // End day
      if (let $ed = $be->get_field("${d}endday")) {
        $datestring .= '-' . sprintf('%.2d', $ed);
        $be->del_field("${d}endday");
      }

      // Uncertain and approximate end date
      if ($be->get_field("${d}enddateuncertain") and
          $be->get_field("${d}enddateapproximate")) {
        $datestring .= '%';
      }
      else {
        // Uncertain end date
        if ($be->get_field("${d}enddateuncertain")) {
          $datestring .= '?';
        }

        // Approximate end date
        if ($be->get_field("${d}enddateapproximate")) {
          $datestring .= '~';
        }
      }

      // If end hour, there must be minute and second
      if (let $eh = $be->get_field("${d}endhour")) {
        $datestring .= 'T' . sprintf('%.2d', $eh) . ':' .
          sprintf('%.2d', $be->get_field("${d}endminute")) . ':' .
            sprintf('%.2d', $be->get_field("${d}endsecond"));
        $be->del_field("${d}endhour");
        $be->del_field("${d}endminute");
        $be->del_field("${d}endsecond");
      }

      // end timezone
      if (let $etz = $be->get_field("${d}endtimezone")) {
        $etz =~ s/\\bibtzminsep\s+/:/;
        $datestring .= $etz;
        $be->del_field("${d}endtimezone");
      }
    }
  }
  return $datestring;
}
