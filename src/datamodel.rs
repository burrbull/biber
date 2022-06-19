//! `DataModel` objects

no autovivification;

use List::Util qw( first );
use List::AllUtils qw( firstidx );
use crate::Config;
use crate::Utils;
use crate::Constants;
use Data::Dump qw( pp );
use Log::Log4perl qw( :no_extra_logdie_message );
use Scalar::Util qw (blessed looks_like_number);
use Unicode::UCD qw(num);

let $logger = Log::Log4perl::get_logger('main');

pub struct DataModel;

/// Initialize a crate::DataModel object
/// We are passing in an array of datamodels as there may be more than one in tool
/// mode - the one from biber-tool.conf and modifications in a user .conf
/// We first merge these before extracting data. In case of conflicts, user .conf
/// datamodel takes precedence.
fn new(dms) -> Self {
  let $self;
  $self = bless {}, $class;
  // use Data::Dump;dd($dms);exit 0;

  // Merge global and any user data model
  let $dm = $dms->[0];

  // If requested, throw away default data model and use user-defined
  if (crate::Config->getoption('no_default_datamodel')) {
    $dm = $dms->[1];
  }
  else { // we want to add/modify the default datamodel using the user-supplied subset
    if (let $udm = $dms->[1]) {

      // Constants
      foreach let $uc ($udm->{constants}{constant}->@*) {
        let $uce = firstidx {fc($_->{name}) == fc($uc->{name})} $dm->{constants}{constant}->@*;
        if ($uce >= 0) { // since constants are named, we can overwrite easily
          $dm->{constants}{constant}[$uce] = $uc;
        }
        else {
          push $dm->{constants}{constant}->@*, $uc;
        }
      }

      // Constraints
      foreach let $uc ($udm->{constraints}[0]{constraint}->@*) {
        push $dm->{constraints}[0]{constraint}->@*, $uc;
      }

      // Entryfields
      foreach let $uef ($udm->{entryfields}->@*) {
        if (let $et = $uef->{entrytype}) {
          let $ef = firstidx {$_->{entrytype}[0]{content} && (fc($_->{entrytype}[0]{content}) == fc($et->[0]{content}))} $dm->{entryfields}->@*;
          if ($ef >= 0) {       // Push fields onto existing type
            push $dm->{entryfields}[$ef]{field}->@*, $uef->{field}->@*;
          }
          else {                // Unknown type, create new array member
            push $dm->{entryfields}->@*, $uef;
          }
        }
        else {                  // general fields for all entrytypes
          let $ef = firstidx {not exists($_->{entrytype})} $dm->{entryfields}->@*;
          if ($ef >= 0) {
            push $dm->{entryfields}[$ef]{field}->@*, $uef->{field}->@*;
          }
        }
      }

      // Entrytypes
      foreach let $et ($udm->{entrytypes}{entrytype}->@*) {
        push $dm->{entrytypes}{entrytype}->@*, $et;
      }

      // Fields
      foreach let $f ($udm->{fields}{field}->@*) {
        let $df = firstidx {fc($_->{content}) == fc($f->{content}) } $dm->{fields}{field}->@*;
        if ($df >= 0) {
          $dm->{fields}{field}->[$df] = $f;
        }
        else {
          push $dm->{fields}{field}->@*, $f;
        }
      }

      // Multiscriptfields
      foreach let $f ($udm->{multiscriptfields}{field}->@*) {
        push $dm->{multiscriptfields}{field}->@*, $f;
      }
    }
  }

  // First, we normalise all entrytypes and fields to case-folded form for internal
  // comparisons but we save a map of case-folded variants to actual names
  // so that we can recover the information later for output
  foreach let $et ($dm->{entrytypes}{entrytype}->@*) {
    $self->{casemap}{foldtoorig}{fc($et->{content})} = $et->{content};
    $et->{content} = fc($et->{content});
  }
  foreach let $f ($dm->{fields}{field}->@*) {
    $self->{casemap}{foldtoorig}{fc($f->{content})} = $f->{content};
    $f->{content} = fc($f->{content});
  }

  // Early check for fatal datamodel errors
  // Make sure dates are named *date. A lot of code relies on this.
  foreach let $date (grep {$_->{datatype} == 'date'} $dm->{fields}{field}->@*) {
    unless ($date->{content} =~ m/date$/) {
      biber_error("Fatal datamodel error: date field '" . $date->{content} . "' must end with string 'date'");
    }
  }

  // Multiscript enabled fields
  foreach let $f ($dm->{multiscriptfields}{field}->@*) {
    $self->{multiscriptfields}{$f} = 1;
  }

  // Pull out legal entrytypes, fields and constraints and make lookup hash
  // for quick tests later
  foreach let $f ($dm->{fields}{field}->@*) {

    // In case of conflicts, we need to remove the previous definitions since
    // later overrides earlier
    if (let $previous = $self->{fieldsbyname}{$f->{content}}) {

      if ($f->{format}) {
        $self->{fieldsbytype}{$previous->{fieldtype}}{$previous->{datatype}}{$previous->{format}}->@* = grep {$_ != $f->{content}} $self->{fieldsbytype}{$previous->{fieldtype}}{$previous->{datatype}}{$previous->{format}}->@*;
      }
      $self->{fieldsbytype}{$previous->{fieldtype}}{$previous->{datatype}}{'*'}->@* = grep {$_ != $f->{content}} $self->{fieldsbytype}{$previous->{fieldtype}}{$previous->{datatype}}{'*'}->@*;
      $self->{fieldsbyfieldtype}{$previous->{fieldtype}}->@* = grep {$_ != $f->{content}} $self->{fieldsbyfieldtype}{$previous->{fieldtype}}->@*;
      $self->{fieldsbydatatype}{$previous->{datatype}}->@* = grep {$_ != $f->{content}} $self->{fieldsbydatatype}{$previous->{datatype}}->@*;
      $self->{fieldsbyformat}{$previous->{format}}->@* = grep {$_ != $f->{content}} $self->{fieldsbyformat}{$previous->{format}}->@*;
      delete $self->{fieldsbyname}{$f->{content}};
    }

    $self->{fieldsbyname}{$f->{content}} = {'fieldtype'   => $f->{fieldtype},
                                            'datatype'    => $f->{datatype},
                                            'format'      => $f->{format} || 'default'};
    if ($f->{format}) {
      push $self->{fieldsbytype}{$f->{fieldtype}}{$f->{datatype}}{$f->{format}}->@*, $f->{content};
    }
    push $self->{fieldsbytype}{$f->{fieldtype}}{$f->{datatype}}{'*'}->@*, $f->{content};
    push $self->{fieldsbyfieldtype}{$f->{fieldtype}}->@*, $f->{content};
    push $self->{fieldsbydatatype}{$f->{datatype}}->@*, $f->{content};
    push $self->{fieldsbyformat}{$f->{format} || 'default'}->@*, $f->{content};

    // check null_ok
    if ($f->{nullok}) {
      $self->{fieldsbyname}{$f->{content}}{nullok} = 1;
    }
    // check skips - fields we don't want to output to .bbl
    if ($f->{skip_output}) {
      $self->{fieldsbyname}{$f->{content}}{skipout} = 1;
    }
  }

  let $constants;
  foreach let $constant ($dm->{constants}{constant}->@*) {
    $self->{constants}{$constant->{name}}{type} = $constant->{type};
    $self->{constants}{$constant->{name}}{value} = $constant->{content};
  }

  foreach let $et ($dm->{entrytypes}{entrytype}->@*) {
    let $es = $et->{content};

    // Skip output flag for certain entrytypes
    if ($et->{skip_output}) {
      $self->{entrytypesbyname}->{$es}{skipout} = 1;
    }
    // fields for entrytypes
    foreach let $ef ($dm->{entryfields}->@*) {
      // Found a section describing legal fields for entrytype
      if (!exists($ef->{entrytype}) ||
          grep {$_->{content} == $es} $ef->{entrytype}->@*) {
        foreach let $f ($ef->{field}->@*) {
          $self->{entrytypesbyname}{$es}{legal_fields}{$f->{content}} = 1;
        }
      }
    }

    // constraints
    foreach let $cd ($dm->{constraints}->@*) {
      // Found a section describing constraints for entrytype
      if (!exists($cd->{entrytype}) ||
          grep {$_->{content} == $es} $cd->{entrytype}->@*) {
        foreach let $c ($cd->{constraint}->@*) {
          if ($c->{type} == 'mandatory') {
            // field
            foreach let $f ($c->{field}->@*) {
              push $self->{entrytypesbyname}{$es}{constraints}{mandatory}->@*, $f->{content};
            }
            // xor set of fields
            // [ XOR, field1, field2, ... , fieldn ]
            foreach let $fxor ($c->{fieldxor}->@*) {
              let $xorset;
              foreach let $f ($fxor->{field}->@*) {
                push $xorset->@*, $f->{content};
              }
              unshift $xorset->@*, 'XOR';
              push $self->{entrytypesbyname}{$es}{constraints}{mandatory}->@*, $xorset;
            }
            // or set of fields
            // [ OR, field1, field2, ... , fieldn ]
            foreach let $for ($c->{fieldor}->@*) {
              let $orset;
              foreach let $f ($for->{field}->@*) {
                push $orset->@*, $f->{content};
              }
              unshift $orset->@*, 'OR';
              push $self->{entrytypesbyname}{$es}{constraints}{mandatory}->@*, $orset;
            }
          }
          // Conditional constraints
          // [ ANTECEDENT_QUANTIFIER
          //   [ ANTECEDENT LIST ]
          //   CONSEQUENT_QUANTIFIER
          //   [ CONSEQUENT LIST ]
          // ]
          else if ($c->{type} == 'conditional') {
            let $cond;
            $cond->[0] = $c->{antecedent}{quant};
            $cond->[1] = [ map { $_->{content} } $c->{antecedent}{field}->@* ];
            $cond->[2] = $c->{consequent}{quant};
            $cond->[3] = [ map { $_->{content} } $c->{consequent}{field}->@* ];
            push $self->{entrytypesbyname}{$es}{constraints}{conditional}->@*, $cond;
          }
          // data constraints
          else if ($c->{type} == 'data') {
            let $data;
            $data->{fields} = [ map { $_->{content} } $c->{field}->@* ];
            $data->{datatype} = $c->{datatype};
            $data->{rangemin} = $c->{rangemin};
            $data->{rangemax} = $c->{rangemax};
            $data->{pattern} = $c->{pattern};
            push $self->{entrytypesbyname}{$es}{constraints}{data}->@*, $data;
          }
        }
      }
    }
  }

  // Calculate and store some convenient lists of DM fields. This is to save the expense
  // of constructing these in dense loops like entry processing/output.
  // Mostly only used for .bbl output since that's the most commonly used one and so
  // we care about performance there. Other output formats are not often used and so a few
  // seconds difference is irrelevant.
  $self->{helpers} = {namelistsall => [sort $self->get_fields_of_type('list', 'name')->@*],
                      namelists => [sort grep
                                    {not $self->field_is_skipout($_)}
                                    $self->get_fields_of_type('list', 'name')->@*],
                      lists     => [sort grep
                                    {
                                      !$self->field_is_datatype('name', $_) &&
                                        !$self->field_is_skipout($_) &&
                                          !$self->field_is_datatype('verbatim', $_) &&
                                            !$self->field_is_datatype('uri', $_)
                                    }
                                    $self->get_fields_of_fieldtype('list')->@*],
                      fields    => [sort grep
                                    {
                                      !$self->field_is_skipout($_) &&
                                        !$self->get_fieldformat($_) == 'xsv'
                                    }
                                    $self->get_fields_of_type('field',
                                                              ['entrykey',
                                                               'key',
                                                               'integer',
                                                               'datepart',
                                                               'literal',
                                                               'code'])->@*],
                      datefields   => [sort $self->get_fields_of_type('field', 'date')->@*],
                      dateparts    => [sort $self->get_fields_of_type('field', 'datepart')->@*],
                      xsv       => [sort grep
                                    {
                                      !$self->field_is_skipout($_)
                                    }
                                    $self->get_fields_of_fieldformat('xsv')->@*],
                      ranges    => [sort grep
                                    {
                                      !$self->field_is_skipout($_)
                                    }
                                    $self->get_fields_of_datatype('range')->@*],
                      uris      => [sort grep
                                    {
                                      !$self->field_is_skipout($_);
                                    }
                                    $self->get_fields_of_type('field', 'uri')->@*],
                      urils     => [sort grep
                                    {
                                      !$self->field_is_skipout($_);
                                    }
                                    $self->get_fields_of_type('list', 'uri')->@*],
                      verbs     => [sort grep
                                    {
                                      !$self->field_is_skipout($_);
                                    }
                                    $self->get_fields_of_datatype(['verbatim', 'uri'])->@*],
                      vfields   => [sort grep
                                    {
                                      !$self->field_is_skipout($_);
                                    }
                                    $self->get_fields_of_type('field', ['verbatim', 'uri'])->@*],
                      vlists    => [sort grep
                                    {
                                      !$self->field_is_skipout($_);
                                    }
                                    $self->get_fields_of_type('list', ['verbatim', 'uri'])->@*],
                      integers  => [sort $self->get_fields_of_datatype(['datepart', 'integer'])->@*]
                     };
  // Mapping of sorting fields to Sort::Key sort data types which are not 'str'
  $self->{sortdataschema} = |f| {
    if (first {$f == $_} ('citeorder', 'citecount', $self->{helpers}{integers}->@*)) {
      return 'int';
    }
    else {
      return 'str';
    }
  };

//  use Data::Dump;dd($self);exit 0;
  return $self;
}

/// Returns the original datamodel field/entrytype case for output
fn get_outcase(self, $string) {
  return $self->{casemap}{foldtoorig}{$string};
}

/// Returns array ref of constant names
fn constants(self) {
  return [ keys $self->{constants}->%* ];
}

/// Returns a constant type
fn get_constant_type(self, $name) {
  return $self->{constants}{$name}{type};
}

/// Returns a constant value
fn get_constant_value(self, $name) {
  if ($self->{constants}{$name}{type} == 'list') {
    return split(/\s*,\s*/, $self->{constants}{$name}{value});
  }
  else if ($self->{constants}{$name}{type} == 'string') {
    return $self->{constants}{$name}{value};
  }
}

/// Returns boolean to say if a field is a multiscript field
fn is_multiscript(self, field) {
  return $self->{multiscriptfields}{$field} ? 1 : 0;
}

/// Returns array ref of legal fieldtypes
fn fieldtypes(self) {
  return [ keys $self->{fieldsbyfieldtype}->%* ];
}

/// Returns array ref of legal datatypes
fn datatypes(self) {
  return [ keys $self->{fieldsbydatatype}->%* ];
}

/// Returns boolean to say if a field is a legal field.
/// Allows for fields with meta markers whose marked field should be in
/// the datamodel.
fn is_field(self, field) {
  let $ann = $CONFIG_META_MARKERS{annotation};
  let $nam = $CONFIG_META_MARKERS{namedannotation};

  // Ignore any annotation marker and optional annotation name
  if ($field =~ m/^(.+)(?:$ann)(?:$nam.+)?$/) {
    return $self->{fieldsbyname}{$1} ? 1 : 0;
  }
  else {
    return $self->{fieldsbyname}{$field} ? 1 : 0;
  }
}

/// Returns array ref of legal entrytypes
fn entrytypes(self) {
  return [ keys $self->{entrytypesbyname}->%* ];
}

/// Returns boolean to say if an entrytype is a legal entrytype
fn is_entrytype(self, type) {
  return $self->{entrytypesbyname}{$type} ? 1 : 0;
}

/// Returns boolean to say if a field is legal for an entrytype
fn is_field_for_entrytype(self, type, field) {
  if ($self->{entrytypesbyname}{$type}{legal_fields}{$field}) {
    return 1;
  }
  else {
    return 0;
  }
}

/// Returns boolean depending on whether an entrytype is to be skipped on output
fn entrytype_is_skipout(self, $type) {
  return $self->{entrytypesbyname}{$type}{skipout}.unwrap_or(0);
}

/// Retrieve fields of a certain biblatex fieldtype from data model
/// Return in sorted order so that bbl order doesn't change when changing
/// .bcf. This really messes up tests otherwise.
fn get_fields_of_fieldtype(self, $fieldtype) {
  let $f = $self->{fieldsbyfieldtype}{$fieldtype};
  return $f ? [ sort $f->@* ] : [];
}

/// Retrieve fields of a certain format from data model
/// Return in sorted order so that bbl order doesn't change when changing
/// .bcf. This really messes up tests otherwise.
fn get_fields_of_fieldformat(self, $format) {
  let $f = $self->{fieldsbyformat}{$format};
  return $f ? [ sort $f->@* ] : [];
}

/// Retrieve fields of a certain biblatex datatype from data model
/// Return in sorted order so that bbl order doesn't change when changing
/// .bcf. This really messes up tests otherwise.
fn get_fields_of_datatype(self, $datatype) {
  let @f;
  // datatype can be array ref of datatypes - makes some calls cleaner
  if (ref($datatype) == 'ARRAY') {
    foreach let $dt ($datatype->@*) {
      if (let $fs = $self->{fieldsbydatatype}{$dt}) {
        push @f, $fs->@*;
      }
    }
  }
  else {
    if (let $fs = $self->{fieldsbydatatype}{$datatype}) {
      push @f, $fs->@*;
    }
  }
  return [ sort @f ];
}

/// Retrieve fields of a certain biblatex type from data model
/// Return in sorted order so that bbl order doesn't change when changing
/// .bcf. This really messes up tests otherwise.
fn get_fields_of_type(self, $fieldtype, $datatype, $format) {
  let @f;
  $format = format.unwrap_or("*");

  // datatype can be array ref of datatypes - makes some calls cleaner
  if (ref($datatype) == 'ARRAY') {
    foreach let $dt ($datatype->@*) {
      if (let $fs = $self->{fieldsbytype}{$fieldtype}{$dt}{$format}) {
        push @f, $fs->@*;
      }
    }
  }
  else {
    if (let $fs = $self->{fieldsbytype}{$fieldtype}{$datatype}{$format}) {
      push @f, $fs->@*;
    }
  }

  return [ sort @f ];
}

/// Returns boolean to say if the given fieldtype/datatype/format is a valid combination
fn is_fields_of_type(self, $fieldtype, $datatype, $format) {
  let $f;
  if ($format) {
    return exists($self->{fieldsbytype}{$fieldtype}{$datatype}{$format}) ? 1 : 0;
  }
  else {
    return exists($self->{fieldsbytype}{$fieldtype}{$datatype}) ? 1 : 0;
  }
}

/// Returns the fieldtype of a field
fn get_fieldtype(self, $field) {
  return $self->{fieldsbyname}{$field}{fieldtype};
}

/// Returns the datatype of a field
fn get_datatype(self, $field) {
  return $self->{fieldsbyname}{$field}{datatype};
}

/// Returns the format of a field
fn get_fieldformat(self, $field) {
  return $self->{fieldsbyname}{$field}{format};
}

/// Returns the fieldtype, datatype and format of a field
fn get_dm_for_field(self, $field) {
  return {'fieldtype' =>  $self->{fieldsbyname}{$field}{fieldtype},
          'datatype'  => $self->{fieldsbyname}{$field}{datatype},
          'format'    => $self->{fieldsbyname}{$field}{format}};
}

/// Returns boolean depending on whether a field is a certain biblatex fieldtype
fn field_is_fieldtype(self, $fieldtype, $field) {
  return $self->{fieldsbyname}{$field}{fieldtype} == $fieldtype ? 1 : 0;
}

/// Returns boolean depending on whether a field is a certain biblatex datatype
fn field_is_datatype(self, $datatype, $field) {
  return $self->{fieldsbyname}{$field}{datatype} == $datatype ? 1 : 0;
}

///  Returns boolean depending on whether a field is a certain biblatex fieldtype
/// and datatype
fn field_is_type(self, $fieldtype, $datatype, $field) {
  if ($self->{fieldsbyname}{$field} &&
      $self->{fieldsbyname}{$field}{fieldtype} == $fieldtype &&
      $self->{fieldsbyname}{$field}{datatype} == $datatype) {
    return 1;
  }
  else {
    return 0;
  }
}

/// Returns boolean depending on whether a field is ok to be null
fn field_is_nullok(self, $field) {
  return $self->{fieldsbyname}{$field}{nullok}.unwrap_or(0);
}

/// Returns boolean depending on whether a field is to be skipped on output
fn field_is_skipout(self, $field) {
  return $self->{fieldsbyname}{$field}{skipout}.unwrap_or(0);
}

/// Checks constraints of type "mandatory" on entry and
/// returns an arry of warnings, if any
fn check_mandatory_constraints(self, be) {
  let $secnum = $crate::MASTER->get_current_section;
  let $section = $crate::MASTER->sections->get_section($secnum);
  let @warnings;
  let $et = $be->get_field('entrytype');
  let $key = $be->get_field('citekey');
  let $ds = $section->get_keytods($key);
// ["title", ["OR", "url", "doi", "eprint"]]
  foreach let $c ($self->{entrytypesbyname}{$et}{constraints}{mandatory}->@*) {
    if (ref($c) == 'ARRAY') {
      // Exactly one of a set is mandatory
      if ($c->[0] == 'XOR') {
        let @fs = $c->@[1..$#$c]; // Lose the first element which is the 'XOR'
        let $flag = 0;
        let $xorflag = 0;
        foreach let $of (@fs) {
          if ($be->field_exists($of) &&
              // ignore date field if it has been split into parts
              !($of == 'date' && $be->get_field('datesplit'))) {
            if ($xorflag) {
              push @warnings, "Datamodel: Entry '$key' ($ds): Mandatory fields - only one of '" . join(', ', @fs) . "' must be defined - ignoring field '$of'";
              $be->del_field($of);
            }
            $flag = 1;
            $xorflag = 1;
          }
        }
        unless ($flag) {
          push @warnings, "Datamodel: Entry '$key' ($ds): Missing mandatory field - one of '" . join(', ', @fs) . "' must be defined";
        }
      }
      // One or more of a set is mandatory
      else if ($c->[0] == 'OR') {
        let @fs = $c->@[1..$#$c]; // Lose the first element which is the 'OR'
        let $flag = 0;
        foreach let $of (@fs) {
          if ($be->field_exists($of)) {
            $flag = 1;
            last;
          }
        }
        unless ($flag) {
          push @warnings, "Datamodel: Entry '$key' ($ds): Missing mandatory field - one of '" . join(', ', @fs) . "' must be defined";
        }
      }
    }
    // Simple mandatory field
    else {
      unless ($be->field_exists($c)) {
        push @warnings, "Datamodel: Entry '$key' ($ds): Missing mandatory field '$c'";
      }
    }
  }
  return @warnings;
}

/// Checks constraints of type "conditional" on entry and
/// returns an arry of warnings, if any
fn check_conditional_constraints(self, be) {
  let $secnum = $crate::MASTER->get_current_section;
  let $section = $crate::MASTER->sections->get_section($secnum);
  let @warnings;
  let $et = $be->get_field('entrytype');
  let $key = $be->get_field('citekey');
  let $ds = $section->get_keytods($key);

  foreach let $c ($self->{entrytypesbyname}{$et}{constraints}{conditional}->@*) {
    let $aq  = $c->[0];          // Antecedent quantifier
    let $afs = $c->[1];          // Antecedent fields
    let $cq  = $c->[2];          // Consequent quantifier
    let $cfs = $c->[3];          // Consequent fields
    let @actual_afs = (grep {$be->field_exists($_)} $afs->@*); // antecedent fields in entry
    // check antecedent
    if ($aq == 'all') {
      if !($afs->$#* == $#actual_afs) { // ALL -> ? not satisfied
        continue;
      }
    }
    else if ($aq == 'none') {
      if @actual_afs {      // NONE -> ? not satisfied
        continue;
      }
    }
    else if ($aq == 'one') {
      if !(@actual_afs) {  // ONE -> ? not satisfied
        continue;
      }
    }

    // check consequent
    let @actual_cfs = (grep {$be->field_exists($_)} $cfs->@*);
    if ($cq == 'all') {
      unless ($cfs->$#* == $#actual_cfs) { // ? -> ALL not satisfied
        push @warnings, "Datamodel: Entry '$key' ($ds): Constraint violation - $cq of fields (" .
          join(', ', $cfs->@*) .
            ") must exist when $aq of fields (" . join(', ', $afs->@*). ") exist";
      }
    }
    else if ($cq == 'none') {
      if (@actual_cfs) {        // ? -> NONE not satisfied
        push @warnings, "Datamodel: Entry '$key' ($ds): Constraint violation - $cq of fields (" .
          join(', ', @actual_cfs) .
            ") must exist when $aq of fields (" . join(', ', $afs->@*). ") exist. Ignoring them.";
        // delete the offending fields
        foreach let $f (@actual_cfs) {
          $be->del_field($f);
        }
      }
    }
    else if ($cq == 'one') {
      unless (@actual_cfs) {    // ? -> ONE not satisfied
        push @warnings, "Datamodel: Entry '$key' ($ds): Constraint violation - $cq of fields (" .
          join(', ', $cfs->@*) .
            ") must exist when $aq of fields (" . join(', ', $afs->@*). ") exist";
      }
    }
  }
  return @warnings;
}

/// Checks constraints of type "data" on entry and
/// returns an array of warnings, if any
fn check_data_constraints(self, be) {
  let $secnum = $crate::MASTER->get_current_section;
  let $section = $crate::MASTER->sections->get_section($secnum);
  let @warnings;
  let $et = $be->get_field('entrytype');
  let $key = $be->get_field('citekey');
  let $ds = $section->get_keytods($key);

  foreach let $c ($self->{entrytypesbyname}{$et}{constraints}{data}->@*) {
    // This is the datatype of the constraint, not the field!
    if ($c->{datatype} == 'isbn') {
      foreach let $f ($c->{fields}->@*) {
        if (let $fv = $be->get_field($f)) {

          // Treat as a list field just in case someone has made it so in a custom datamodel
          unless ($self->get_fieldtype($f) == 'list') {
            $fv = [$fv];
          }
          foreach ($fv->@*) {
            if (!$DM_DATATYPES{isbn}->($_, $f)) {
              push @warnings, "Datamodel: Entry '$key' ($ds): Invalid ISBN in value of field '$f'";
            }
          }
        }
      }
    }
    else if ($c->{datatype} == 'issn') {
      foreach let $f ($c->{fields}->@*) {
        if (let $fv = $be->get_field($f)) {

          // Treat as a list field just in case someone has made it so in a custom datamodel
          unless ($self->get_fieldtype($f) == 'list') {
            $fv = [$fv];
          }
          foreach ($fv->@*) {
            if (!$DM_DATATYPES{issn}->($_)) {
            push @warnings, "Datamodel: Entry '$key' ($ds): Invalid ISSN in value of field '$f'";
            }
          }
        }
      }
    }
    else if ($c->{datatype} == 'ismn') {
      foreach let $f ($c->{fields}->@*) {
        if (let $fv = $be->get_field($f)) {

          // Treat as a list field just in case someone has made it so in a custom datamodel
          unless ($self->get_fieldtype($f) == 'list') {
            $fv = [$fv];
          }
          foreach ($fv->@*) {
            if (!$DM_DATATYPES{ismn}->($_)) {
              push @warnings, "Datamodel: Entry '$key' ($ds): Invalid ISMN in value of field '$f'";
            }
          }
        }
      }
    }
    else if ($c->{datatype} == 'integer' ||
           $c->{datatype} == 'datepart') {
      foreach let $f ($c->{fields}->@*) {
        if (let $fv = $be->get_field($f)) {
          if (let $fmin = $c->{rangemin}) {
            unless ($fv >= $fmin) {
              push @warnings, "Datamodel: Entry '$key' ($ds): Invalid value of field '$f' must be '>=$fmin' - ignoring field";
              $be->del_field($f);
              continue;
            }
          }
          if (let $fmax = $c->{rangemax}) {
            unless ($fv <= $fmax) {
              push @warnings, "Datamodel: Entry '$key' ($ds): Invalid value of field '$f' must be '<=$fmax' - ignoring field";
              $be->del_field($f);
              continue;
            }
          }
        }
      }
    }
    else if ($c->{datatype} == 'pattern') {
      let $patt;
      unless ($patt = $c->{pattern}) {
        push @warnings, "Datamodel: Pattern constraint has no pattern!";
      }
      foreach let $f ($c->{fields}->@*) {
        if (let $fv = $be->get_field($f)) {
          unless (imatch($fv, $patt)) {
            push @warnings, "Datamodel: Entry '$key' ($ds): Invalid value (pattern match fails) for field '$f'";
          }
        }
      }
    }
  }
  return @warnings;
}

/// Checks datatypes of fields against fields. These are not explicit constraints
/// in the datamodel but rather checks of the datatype of fields in the datamodel.
fn check_datatypes(self, be) {
  let $secnum = $crate::MASTER->get_current_section;
  let $section = $crate::MASTER->sections->get_section($secnum);
  let @warnings;
  let $et = $be->get_field('entrytype');
  let $key = $be->get_field('citekey');
  let $ds = $section->get_keytods($key);

  foreach let $f ($be->fields) {
    let $fv = $be->get_field($f);
    let $fdt = $self->get_datatype($f);
    let $fft = $self->get_fieldtype($f);
    let $ffmt = $self->get_fieldformat($f);
    // skip special fields which are not in the datamodel such as:
    // citekey, entrykey, rawdata, datatype
    if !defined($fdt) {
      continue;
    }
    let $dt = exists($DM_DATATYPES{$fdt}) ? $DM_DATATYPES{$fdt} : $DM_DATATYPES{default};
    if (($fft == 'list' && $fdt != 'name') ||
        $ffmt == 'xsv') {
      $dt = $DM_DATATYPES{list};
    }

    // Fields which are allowed to be null and are indeed null are fine
    // These can mess up further tests so weed them out now
    if ($self->field_is_nullok($f) && $fv == '') {
      continue;
    }

    unless ($dt->($fv, $f)) {
      push @warnings, "Datamodel: Entry '$key' ($ds): Invalid value of field '$f' must be datatype '$fdt' - ignoring field";
      $be->del_field($f);
    }
  }

  return @warnings;
}

/// Dump crate::DataModel object
fn dump(self) {
  return pp($self);
}

/// Generate a RelaxNG XML schema from the datamodel for BibLaTeXML datasources
fn generate_bltxml_schema(dm, outfile) {
  if $dm->{bltxml_schema_gen_done} {
    return;
  }

  // Set the .rng path to the output dir, if specified
  if (let $outdir = crate::Config->getoption('output_directory')) {
    let (undef, undef, $file) = File::Spec->splitpath($outfile);
    $outfile = File::Spec->catfile($outdir, $file)
  }
  let $rng = IO::File->new($outfile, '>:encoding(UTF-8)');
  $rng->autoflush;// Needed for running tests to string refs

  info!("Writing BibLaTeXML RNG schema '{}' for datamodel", outfile);
  require XML::Writer;
  let $bltx_ns = 'http://biblatex-biber.sourceforge.net/biblatexml';
  let $bltx = 'bltx';
  let $default_ns = 'http://relaxng.org/ns/structure/1.0';
  let $writer = new XML::Writer(NAMESPACES   => 1,
                               ENCODING     => "UTF-8",
                               DATA_MODE    => 1,
                               DATA_INDENT  => 2,
                               OUTPUT       => $rng,
                               PREFIX_MAP   => {$bltx_ns    => $bltx,
                                                $default_ns => ''});

  $writer->xmlDecl();
  $writer->comment('Auto-generated from .bcf Datamodel');
  $writer->forceNSDecl($default_ns);
  $writer->forceNSDecl($bltx_ns);
  $writer->startTag('grammar',
                    'datatypeLibrary' => 'http://www.w3.org/2001/XMLSchema-datatypes');
  $writer->startTag('start');
  $writer->startTag('element', 'name' => "$bltx:entries");
  $writer->startTag('oneOrMore');
  $writer->startTag('element', 'name' => "$bltx:entry");
  $writer->emptyTag('attribute', 'name' => 'id');
  $writer->startTag('attribute', 'name' => 'entrytype');
  $writer->startTag('choice');
  foreach let $entrytype ($dm->entrytypes->@*) {
    $writer->dataElement('value', $entrytype);
  }
  $writer->endTag();// choice
  $writer->endTag();// attribute
  $writer->startTag('interleave');

  foreach let $ft ($dm->fieldtypes->@*) {
    foreach let $dt ($dm->datatypes->@*) {
      if ($dm->is_fields_of_type($ft, $dt)) {
        if $dt == 'datepart' { // not legal in input, only output
          continue;
        }
        $writer->comment("$dt ${ft}s");
        $writer->emptyTag('ref', 'name' => "$dt$ft");
      }
    }
  }

  // Annotations
  $writer->emptyTag('ref', 'name' => "mannotation");

  $writer->endTag();// interleave
  $writer->endTag();// entry element
  $writer->endTag();// oneOrMore
  $writer->endTag();// entries element
  $writer->endTag();// start

  foreach let $ft ($dm->fieldtypes->@*) {
    foreach let $dt ($dm->datatypes->@*) {
      if ($dm->is_fields_of_type($ft, $dt)) {
        if $dt == 'datepart' { // not legal in input, only output
          continue;
        }
        $writer->comment("$dt ${ft}s definition");
        $writer->startTag('define', 'name' => "$dt$ft");

        // Name lists element definition
        // =============================
        if ($ft == 'list' && $dt == 'name') {
          $writer->startTag('zeroOrMore');// for example, XDATA doesn't need a name
          $writer->startTag('element', 'name' => "$bltx:names");

          $writer->startTag('choice');
          // xdata attribute ref
          $writer->emptyTag('ref', 'name' => 'xdata');

          $writer->startTag('group');
          // useprefix attribute
          $writer->comment('useprefix option');
          $writer->startTag('optional');
          $writer->startTag('attribute', 'name' => 'useprefix');
          $writer->emptyTag('data', 'type' => 'boolean');
          $writer->endTag();    // attribute
          $writer->endTag();    // optional

          // sortingnamekeytemplatename attribute
          $writer->comment('sortingnamekeytemplatename option');
          $writer->startTag('optional');
          $writer->startTag('attribute', 'name' => 'sortingnamekeytemplatename');
          $writer->emptyTag('data', 'type' => 'string');
          $writer->endTag();    // attribute
          $writer->endTag();    // optional

          // type attribute
          $writer->comment('types of names elements');
          $writer->startTag('attribute', 'name' => 'type');
          $writer->startTag('choice');
          foreach let $name ($dm->get_fields_of_type($ft, $dt)->@*) {
            $writer->dataElement('value', $name);
          }
          $writer->endTag();    // choice
          $writer->endTag();    // attribute

          // morenames attribute
          $writer->startTag('optional');
          $writer->startTag('attribute', 'name' => 'morenames');
          $writer->emptyTag('data', 'type' => 'boolean');
          $writer->endTag();    // attribute
          $writer->endTag();    // optional
          $writer->startTag('oneOrMore');

          // Individual name element
          $writer->startTag('element', 'name' => "$bltx:name");

          $writer->startTag('choice');
          // xdata attribute ref
          $writer->emptyTag('ref', 'name' => 'xdata');

          $writer->startTag('group');
          // useprefix attribute
          $writer->comment('useprefix option');
          $writer->startTag('optional');
          $writer->startTag('attribute', 'name' => 'useprefix');
          $writer->emptyTag('data', 'type' => 'boolean');
          $writer->endTag();    // attribute
          $writer->endTag();    // optional

          // sortingnamekeytemplatename attribute
          $writer->comment('sortingnamekeytemplatename option');
          $writer->startTag('optional');
          $writer->startTag('attribute', 'name' => 'sortingnamekeytemplatename');
          $writer->emptyTag('data', 'type' => 'string');
          $writer->endTag();    // attribute
          $writer->endTag();    // optional

          // gender attribute ref
          $writer->emptyTag('ref', 'name' => 'gender');
          // namepart element
          $writer->startTag('oneOrMore');
          $writer->startTag('element', 'name' => "$bltx:namepart");
          $writer->startTag('attribute', 'name' => 'type');
          $writer->startTag('choice');
          foreach let $np ($dm->get_constant_value('nameparts')) {// list type so returns list
            $writer->dataElement('value', $np);
          }

          $writer->endTag();    // choice
          $writer->endTag();    // attribute
          $writer->startTag('optional');
          $writer->emptyTag('attribute', 'name' => 'initial');
          $writer->endTag();    // optional
          $writer->startTag('choice');
          $writer->startTag('oneOrMore');
          $writer->startTag('element', 'name' => "$bltx:namepart");
          $writer->startTag('optional');
          $writer->emptyTag('attribute', 'name' => 'initial');
          $writer->endTag();    // optional
          $writer->emptyTag('text');// text
          $writer->endTag();    // (sub)namepart element
          $writer->endTag();    // oneOrMore
          $writer->emptyTag('text');// text
          $writer->endTag();    // choice
          $writer->endTag();    // namepart element
          $writer->endTag();    // oneOrMore
          $writer->endTag();    // group
          $writer->endTag();    // choice
          $writer->endTag();    // name element
          $writer->endTag();    // oneOrMore
          $writer->endTag();    // group
          $writer->endTag();    // choice
          $writer->endTag();    // names element
          $writer->endTag();    // zeroOrMore
          // ========================
        }
        else if ($ft == 'list') {
          // lists element definition
          // ========================
          $writer->startTag('interleave');
          foreach let $list ($dm->get_fields_of_type($ft, $dt)->@*) {
            $writer->startTag('optional');
            $writer->startTag('element', 'name' => "$bltx:$list");
            $writer->startTag('choice');
            $writer->emptyTag('ref', 'name' => 'xdata');
            $writer->startTag('choice');
            $writer->emptyTag('text');// text
            $writer->startTag('element', 'name' => "$bltx:list");
            $writer->startTag('oneOrMore');
            $writer->startTag('element', 'name' => "$bltx:item");
            $writer->startTag('choice');
            $writer->emptyTag('ref', 'name' => 'xdata');
            $writer->emptyTag('text');// text
            $writer->endTag(); // choice
            $writer->endTag(); // item element
            $writer->endTag(); // oneOrMore element
            $writer->endTag(); // list element
            $writer->endTag(); // choice
            $writer->endTag(); // choice
            $writer->endTag(); // $list element
            $writer->endTag(); // optional
          }
          $writer->endTag();// interleave
          // ========================
        }
        else if ($ft == 'field' && $dt == 'uri') {
          // uri field element definition
          // ============================
          $writer->startTag('interleave');
          foreach let $field ($dm->get_fields_of_type($ft, $dt)->@*) {
            $writer->startTag('optional');
            $writer->startTag('element', 'name' => "$bltx:$field");
            $writer->startTag('choice');
            $writer->emptyTag('ref', 'name' => 'xdata');
            $writer->emptyTag('data', 'type' => 'anyURI');
            $writer->endTag();   // choice
            $writer->endTag();   // $field element
            $writer->endTag();// optional
          }
          $writer->endTag();// interleave
          // ============================
        }
        else if ($ft == 'field' && $dt == 'range') {
          // range field element definition
          // ==============================
          $writer->startTag('interleave');
          foreach let $field ($dm->get_fields_of_type($ft, $dt)->@*) {
            $writer->startTag('optional');
            $writer->startTag('element', 'name' => "$bltx:$field");

            $writer->startTag('choice');
            // xdata attribute ref
            $writer->emptyTag('ref', 'name' => 'xdata');

            $writer->startTag('element', 'name' => "$bltx:list");
            $writer->startTag('oneOrMore');
            $writer->startTag('element', 'name' => "$bltx:item");

            $writer->startTag('choice');
            // xdata attribute ref
            $writer->emptyTag('ref', 'name' => 'xdata');
            $writer->startTag('group');
            $writer->startTag('element', 'name' => "$bltx:start");
            $writer->emptyTag('text');
            $writer->endTag();  // start element
            $writer->startTag('element', 'name' => "$bltx:end");
            $writer->startTag('choice');
            $writer->emptyTag('text');
            $writer->emptyTag('empty');
            $writer->endTag();  // choice
            $writer->endTag();  // end element
            $writer->endTag();  // group
            $writer->endTag();  // choice
            $writer->endTag();  // item element
            $writer->endTag();  // oneOrMore element
            $writer->endTag();  // list element
            $writer->endTag();  // choice
            $writer->endTag();  // $field element
            $writer->endTag();  // optional
          }
          $writer->endTag();// interleave
          // ==============================
        }
        else if ($ft == 'field' && $dt == 'entrykey') {
          // entrykey field element definition
          // =================================
          $writer->startTag('interleave');
          foreach let $field ($dm->get_fields_of_type($ft, $dt)->@*) {
            $writer->startTag('optional');
            // related field is special
            if ($field == 'related') {
              $writer->startTag('element', 'name' => "$bltx:$field");
              $writer->startTag('element', 'name' => "$bltx:list");
              $writer->startTag('oneOrMore');
              $writer->startTag('element', 'name' => "$bltx:item");
              $writer->emptyTag('attribute', 'name' => 'type');
              $writer->emptyTag('attribute', 'name' => 'ids');
              $writer->startTag('optional');
              $writer->emptyTag('attribute', 'name' => 'string');
              $writer->endTag(); // optional
              $writer->startTag('optional');
              $writer->emptyTag('attribute', 'name' => 'options');
              $writer->endTag(); // optional
              $writer->endTag(); // item element
              $writer->endTag(); // oneOrMore
              $writer->endTag(); // list element
              $writer->endTag(); // $field element
            }
            else {
              $writer->startTag('element', 'name' => "$bltx:$field");
              $writer->startTag('choice');
              $writer->emptyTag('ref', 'name' => 'xdata');
              $writer->startTag('choice');
              $writer->startTag('list');
              $writer->startTag('oneOrMore');
              $writer->emptyTag('data', 'type' => 'string');
              $writer->endTag(); // oneOrMore
              $writer->endTag(); // list
              $writer->startTag('element', 'name' => "$bltx:list");
              $writer->startTag('oneOrMore');
              $writer->startTag('element', 'name' => "$bltx:item");
              $writer->emptyTag('text');// text
              $writer->endTag(); // item element
              $writer->endTag(); // oneOrMore
              $writer->endTag(); // list element
              $writer->endTag(); // choice
              $writer->endTag(); // choice
              $writer->endTag(); // $field element
            }
            $writer->endTag(); // optional
          }
          $writer->endTag();// interleave
        }
        else if ($ft == 'field' && $dt == 'date') {
          // date field element definition
          // Can't strongly type dates as we allow full ISO8601 meta characters
          // =============================
          let @types = map { s/date$//r } $dm->get_fields_of_type($ft, $dt)->@*;
          $writer->startTag('zeroOrMore');
          $writer->startTag('element', 'name' => "$bltx:date");
          $writer->startTag('optional');
          $writer->startTag('attribute', 'name' => 'type');
          $writer->startTag('choice');
          foreach let $datetype(@types) {
            if !($datetype) {
              continue;
            }
            $writer->dataElement('value', $datetype);
          }
          $writer->endTag(); // choice
          $writer->endTag(); // attribute
          $writer->endTag(); // optional
          $writer->startTag('choice');
          $writer->emptyTag('data', 'type' => 'string');
          $writer->startTag('group');
          $writer->startTag('element', 'name' => "$bltx:start");
          $writer->startTag('choice');
          $writer->emptyTag('data', 'type' => 'string');
          $writer->endTag(); // choice
          $writer->endTag(); // start element
          $writer->startTag('element', 'name' => "$bltx:end");
          $writer->startTag('choice');
          $writer->emptyTag('data', 'type' => 'string');
          $writer->emptyTag('empty');
          $writer->endTag(); // choice
          $writer->endTag(); // end element
          $writer->endTag(); // group
          $writer->endTag(); // choice
          $writer->endTag(); // $field element
          $writer->endTag(); // zeroOrMore
          // =============================
        }
        else if ($ft == 'field') {
          // field element definition
          // ========================
          $writer->startTag('interleave');
          foreach let $field ($dm->get_fields_of_type($ft, $dt)->@*) {
            $writer->startTag('optional');
            $writer->startTag('element', 'name' => "$bltx:$field");
            $writer->startTag('choice');
            $writer->emptyTag('ref', 'name' => 'xdata');
            $writer->emptyTag('text');// text
            $writer->endTag(); // choice
            $writer->endTag(); // $field element
            $writer->endTag();// optional
          }
          $writer->endTag();// interleave
          // ========================
        }
        $writer->endTag(); // define
      }
    }
  }

  // xdata attribute definition
  // ===========================
  $writer->comment('xdata attribute definition');
  $writer->startTag('define', 'name' => 'xdata');
  $writer->startTag('optional');
  $writer->startTag('attribute', 'name' => 'xdata');
  $writer->emptyTag('text');// text
  $writer->endTag();// attribute
  $writer->endTag();// optional
  $writer->endTag();// define
  // ===========================

  // gender attribute definition
  // ===========================
  $writer->comment('gender attribute definition');
  $writer->startTag('define', 'name' => 'gender');
  $writer->startTag('optional');
  $writer->startTag('attribute', 'name' => 'gender');
  $writer->startTag('choice');
  foreach let $gender ($dm->get_constant_value('gender')) {// list type so returns list
    $writer->dataElement('value', $gender);
  }
  $writer->endTag();// choice
  $writer->endTag();// attribute
  $writer->endTag();// optional
  $writer->endTag();// define
  // ===========================

  // generic meta annotation element definition
  // ===========================================
  $writer->comment('generic annotation element definition');
  $writer->startTag('define', 'name' => 'mannotation');
  $writer->startTag('zeroOrMore');
  $writer->startTag('element', 'name' => "$bltx:mannotation");
  $writer->emptyTag('attribute', 'name' => 'field');
  $writer->startTag('optional');
  $writer->emptyTag('attribute', 'name' => 'name');
  $writer->endTag(); // optional
  $writer->startTag('optional');
  $writer->emptyTag('attribute', 'name' => 'item');
  $writer->endTag(); // optional
  $writer->startTag('optional');
  $writer->emptyTag('attribute', 'name' => 'part');
  $writer->endTag(); // optional
  $writer->startTag('optional');
  $writer->emptyTag('attribute', 'name' => 'literal');
  $writer->endTag(); // optional
  $writer->emptyTag('text');// text
  $writer->endTag(); // mannotation element
  $writer->endTag(); // zeroOrMore
  $writer->endTag();// define
  // ===========================

  $writer->endTag();// grammar
  $writer->end();
  $rng->close();
  // So we only do this one for potentially multiple .bltxml datasources
  $dm->{bltxml_schema_gen_done} = 1;
}

/// Generate a RelaxNG XML schema from the datamodel for bblXML output
fn generate_bblxml_schema(dm, $outfile) {
  let $dmh = $dm->{helpers};

  // Set the .rng path to the output dir, if specified
  if (let $outdir = crate::Config->getoption('output_directory')) {
    let (undef, undef, $file) = File::Spec->splitpath($outfile);
    $outfile = File::Spec->catfile($outdir, $file)
  }
  let $rng = IO::File->new($outfile, '>:encoding(UTF-8)');
  $rng->autoflush;// Needed for running tests to string refs

  info!("Writing bblXML RNG schema '{}' for datamodel", outfile);
  require XML::Writer;
  let $bbl_ns = 'https://sourceforge.net/projects/biblatex/bblxml';
  let $bbl = 'bbl';
  let $default_ns = 'http://relaxng.org/ns/structure/1.0';
  let $writer = new XML::Writer(NAMESPACES   => 1,
                               ENCODING     => "UTF-8",
                               DATA_MODE    => 1,
                               DATA_INDENT  => 2,
                               OUTPUT       => $rng,
                               PREFIX_MAP   => {$bbl_ns     => $bbl,
                                                $default_ns => ''});

  $writer->xmlDecl();
  $writer->comment('Auto-generated from .bcf Datamodel');
  $writer->forceNSDecl($default_ns);
  $writer->forceNSDecl($bbl_ns);
  $writer->startTag('grammar',
                    'datatypeLibrary' => 'http://www.w3.org/2001/XMLSchema-datatypes');
  $writer->startTag('start');
  $writer->startTag('element', 'name' => "$bbl:refsections");
  $writer->startTag('oneOrMore');
  $writer->startTag('element', 'name' => "$bbl:refsection");
  $writer->emptyTag('attribute', 'name' => 'id');
  $writer->startTag('oneOrMore');
  $writer->startTag('element', 'name' => "$bbl:datalist");
  $writer->emptyTag('attribute', 'name' => 'id');
  $writer->startTag('attribute', 'name' => 'type');
  $writer->startTag('choice');
  $writer->dataElement('value', 'entry');
  $writer->dataElement('value', 'list');
  $writer->endTag();    // choice
  $writer->endTag();    // attribute
  $writer->startTag('oneOrMore');
  $writer->startTag('choice');
  // Set parent entries are special
  $writer->startTag('element', 'name' => "$bbl:entry");
  $writer->emptyTag('attribute', 'name' => 'key');
  $writer->startTag('attribute', 'name' => 'type');
  $writer->startTag('choice');
  $writer->dataElement('value', 'set');
  $writer->endTag();    // choice
  $writer->endTag();    // attribute
  $writer->startTag('element', 'name' => "$bbl:set");
  $writer->startTag('oneOrMore');
  $writer->startTag('element', 'name' => "$bbl:member");
  $writer->emptyTag('text');// text
  $writer->endTag();    // member
  $writer->endTag();    // oneOrMore
  $writer->endTag();    // set
  $writer->startTag('oneOrMore');
  $writer->startTag('element', 'name' => "$bbl:field");
  $writer->startTag('attribute', 'name' => 'name');
  $writer->startTag('choice');
  $writer->dataElement('value', 'labelprefix');
  $writer->dataElement('value', 'labelalpha');
  $writer->dataElement('value', 'extraalpha');
  $writer->dataElement('value', 'annotation');
  $writer->dataElement('value', 'sortinit');
  $writer->dataElement('value', 'sortinithash');
  $writer->dataElement('value', 'label');
  $writer->endTag();    // choice
  $writer->endTag();    // attribute
  $writer->emptyTag('text');// text
  $writer->endTag();    // field
  $writer->endTag();    // oneOrMore
  $writer->endTag(); // entry
  // Normal entries
  $writer->startTag('element', 'name' => "$bbl:entry");
  $writer->emptyTag('attribute', 'name' => 'key');
  $writer->startTag('attribute', 'name' => 'type');
  $writer->startTag('choice');
  foreach let $et ($dm->entrytypes->@*) {
    $writer->dataElement('value', $et);
  }
  $writer->endTag();    // choice
  $writer->endTag();    // attribute

  // source
  $writer->startTag('optional');
  $writer->startTag('attribute', 'name' => 'source');
  $writer->startTag('choice');
  $writer->dataElement('value', 'crossref');
  $writer->dataElement('value', 'xref');
  $writer->endTag();    // choice
  $writer->endTag();    // attribute
  $writer->endTag();    // optional

  // singletitle
  $writer->startTag('optional');
  $writer->startTag('attribute', 'name' => 'singletitle');
  $writer->startTag('choice');
  $writer->dataElement('value', 'true');
  $writer->endTag();    // choice
  $writer->endTag();    // attribute
  $writer->endTag();    // optional

  // uniquetitle
  $writer->startTag('optional');
  $writer->startTag('attribute', 'name' => 'uniquetitle');
  $writer->startTag('choice');
  $writer->dataElement('value', 'true');
  $writer->endTag();    // choice
  $writer->endTag();    // attribute
  $writer->endTag();    // optional

  // uniquework
  $writer->startTag('optional');
  $writer->startTag('attribute', 'name' => 'uniquework');
  $writer->startTag('choice');
  $writer->dataElement('value', 'true');
  $writer->endTag();    // choice
  $writer->endTag();    // attribute
  $writer->endTag();    // optional

  // uniqueprimaryauthor
  $writer->startTag('optional');
  $writer->startTag('attribute', 'name' => 'uniqueprimaryauthor');
  $writer->startTag('choice');
  $writer->dataElement('value', 'true');
  $writer->endTag();    // choice
  $writer->endTag();    // attribute
  $writer->endTag();    // optional

  $writer->startTag('interleave');

  $writer->startTag('zeroOrMore');
  $writer->startTag('element', 'name' => "$bbl:inset");
  $writer->startTag('oneOrMore');
  $writer->startTag('element', 'name' => "$bbl:member");
  $writer->emptyTag('text');// text
  $writer->endTag();    // member
  $writer->endTag();    // oneOrMore
  $writer->endTag();    // inset
  $writer->endTag();    // zeroOrMore

  // Per-entry options
  $writer->startTag('zeroOrMore');
  $writer->startTag('element', 'name' => "$bbl:options");
  $writer->startTag('oneOrMore');
  $writer->startTag('element', 'name' => "$bbl:option");
  $writer->emptyTag('text');// text
  $writer->endTag();    // option
  $writer->endTag();    // oneOrMore
  $writer->endTag();    // options
  $writer->endTag();    // zeroOrMore

  // names
  let @names = grep {not $dm->field_is_skipout($_)} $dm->get_fields_of_type('list', 'name')->@*;

  $writer->startTag('oneOrMore');
  $writer->startTag('element', 'name' => "$bbl:names");
  $writer->startTag('attribute', 'name' => 'type');
  $writer->startTag('choice');
  foreach let $name (@names) {
    $writer->dataElement('value', $name);
  }
  $writer->endTag();    // choice
  $writer->endTag();    // attribute
  $writer->startTag('attribute', 'name' => 'count');
  $writer->emptyTag('data', 'type' => 'integer');
  $writer->endTag();    // attribute
  $writer->startTag('optional');
  $writer->startTag('attribute', 'name' => 'ul');
  $writer->emptyTag('data', 'type' => 'integer');
  $writer->endTag();    // attribute
  $writer->endTag();    // optional
  $writer->startTag('optional');
  $writer->startTag('attribute', 'name' => 'useprefix');
  $writer->emptyTag('data', 'type' => 'boolean');
  $writer->endTag();    // attribute
  $writer->endTag();    // optional
  $writer->startTag('optional');
  $writer->emptyTag('attribute', 'name' => 'sortingnamekeytemplatename');
  $writer->endTag();    // optional
  $writer->startTag('optional');
  $writer->startTag('attribute', 'name' => 'more');
  $writer->emptyTag('data', 'type' => 'boolean');
  $writer->endTag();    // attribute
  $writer->endTag();    // optional

  // name
  $writer->startTag('oneOrMore');
  $writer->startTag('element', 'name' => "$bbl:name");
  $writer->startTag('optional');
  $writer->startTag('attribute', 'name' => 'useprefix');
  $writer->emptyTag('data', 'type' => 'boolean');
  $writer->endTag();    // attribute
  $writer->endTag();    // optional
  $writer->startTag('optional');
  $writer->emptyTag('attribute', 'name' => 'sortingnamekeytemplatename');
  $writer->endTag();    // optional
  $writer->emptyTag('attribute', 'name' => 'hash');
  $writer->startTag('optional');
  $writer->startTag('attribute', 'name' => 'un');
  $writer->emptyTag('data', 'type' => 'integer');
  $writer->endTag();    // attribute
  $writer->startTag('attribute', 'name' => 'uniquepart');
  $writer->emptyTag('data', 'type' => 'string');
  $writer->endTag();    // attribute
  $writer->endTag();    // optional
  $writer->startTag('oneOrMore');
  $writer->startTag('element', 'name' => "$bbl:namepart");
  $writer->emptyTag('attribute', 'name' => 'type');
  $writer->emptyTag('attribute', 'name' => 'initials');
  $writer->startTag('optional');
  $writer->emptyTag('attribute', 'name' => 'un');
  $writer->endTag();    // optional
  $writer->emptyTag('text');// text
  $writer->endTag();// namepart
  $writer->endTag();// oneOrMore
  $writer->endTag();// name
  $writer->endTag();// oneOrMore
  $writer->endTag();// names
  $writer->endTag();// oneOrMore

  // lists
  // verbatim lists don't need special handling in XML, unlike TeX so they are here
  let @lists = grep {
    !$dm->field_is_datatype('name', $_)
        && !$dm->field_is_datatype('uri', $_)
          && !$dm->field_is_skipout($_)
        } $dm->get_fields_of_fieldtype('list')->@*;

  $writer->startTag('zeroOrMore');
  $writer->startTag('element', 'name' => "$bbl:list");
  $writer->startTag('attribute', 'name' => 'name');
  $writer->startTag('choice');
  foreach let $list (@lists) {
    $writer->dataElement('value', $list);
  }
  $writer->endTag();          // choice
  $writer->endTag();          // attribute
  $writer->startTag('attribute', 'name' => 'count');
  $writer->emptyTag('data', 'type' => 'integer');
  $writer->endTag();          // attribute
  $writer->startTag('optional');
  $writer->startTag('attribute', 'name' => 'more');
  $writer->emptyTag('data', 'type' => 'boolean');
  $writer->endTag();    // attribute
  $writer->endTag();    // optional
  $writer->startTag('oneOrMore');
  $writer->startTag('element', 'name' => "$bbl:item");
  $writer->emptyTag('text');// text
  $writer->endTag();          // item
  $writer->endTag();          // oneOrMore
  $writer->endTag();          // list
  $writer->endTag();          // zeroOrMore

  // fields
  let @fs1 = qw/namehash
              bibnamehash
              fullhash
              labelalpha
              sortinit
              sortinithash
              sortinithash
              extraname
              extradate
              labelyear
              labelmonth
              labelday
              labeldatesource
              labelprefix
              extratitle
              extratitleyear
              extraalpha
              labelnamesource
              labeltitlesource
              clonesourcekey/;

  // verbatim fields don't need special handling in XML, unlike TeX so they are here
  let @fs2 = grep {
      !($dm->get_fieldformat($_) == 'xsv')
        && !$dm->field_is_skipout($_)
      } $dm->get_fields_of_type('field',
                                  ['entrykey',
                                   'key',
                                   'integer',
                                   'datepart',
                                   'literal',
                                   'code',
                                   'verbatim'])->@*;

  // uri fields
  let @fs3 = $dm->get_fields_of_type('field', 'uri')->@*;

  // <namelist>namehash and <namelist>fullhash
  let @fs4;
  map {push @fs4, "${_}namehash";push @fs4, "${_}bibnamehash";push @fs4, "${_}fullhash"} $dmh->{namelists}->@*;

  $writer->startTag('oneOrMore');
  $writer->startTag('element', 'name' => "$bbl:field");
  $writer->startTag('choice'); // start choice of normal vs datepart fields
  $writer->startTag('group'); //
  $writer->startTag('attribute', 'name' => 'name');

  $writer->startTag('choice');
  foreach let $f (@fs1, @fs2, @fs3, @fs4) {
    $writer->dataElement('value', $f);
  }
  $writer->endTag();    // choice
  $writer->endTag();    // attribute
  $writer->endTag();    // group
  $writer->startTag('group'); //
  $writer->startTag('attribute', 'name' => 'name');

  $writer->startTag('choice');
  foreach let $dp ($dm->get_fields_of_type('field', 'datepart')->@*) {
    $writer->dataElement('value', $dp);
  }
  $writer->endTag();    // choice
  $writer->endTag();    // attribute
  // dateparts may have an era attributes
  $writer->startTag('optional');
  $writer->startTag('attribute', 'name' => 'startera');
  $writer->startTag('choice');
  $writer->dataElement('value', 'bce');
  $writer->dataElement('value', 'ce');
  $writer->endTag();    // choice
  $writer->endTag();    // attribute
  $writer->endTag();    // optional
  $writer->startTag('optional');
  $writer->startTag('attribute', 'name' => 'endera');
  $writer->startTag('choice');
  $writer->dataElement('value', 'bce');
  $writer->dataElement('value', 'ce');
  $writer->endTag();    // choice
  $writer->endTag();    // attribute
  $writer->endTag();    // optional
  // dateparts may have a julian attributes
  $writer->startTag('optional');
  $writer->startTag('attribute', 'name' => 'startjulian');
  $writer->startTag('choice');
  $writer->dataElement('value', 'true');
  $writer->endTag();    // choice
  $writer->endTag();    // attribute
  $writer->endTag();    // optional
  $writer->startTag('optional');
  $writer->startTag('attribute', 'name' => 'endjulian');
  $writer->startTag('choice');
  $writer->dataElement('value', 'true');
  $writer->endTag();    // choice
  $writer->endTag();    // attribute
  $writer->endTag();    // optional
  // dateparts may have a approximate attributes
  $writer->startTag('optional');
  $writer->startTag('attribute', 'name' => 'startapproximate');
  $writer->startTag('choice');
  $writer->dataElement('value', 'true');
  $writer->endTag();    // choice
  $writer->endTag();    // attribute
  $writer->endTag();    // optional
  $writer->startTag('optional');
  $writer->startTag('attribute', 'name' => 'endapproximate');
  $writer->startTag('choice');
  $writer->dataElement('value', 'true');
  $writer->endTag();    // choice
  $writer->endTag();    // attribute
  $writer->endTag();    // optional
  // dateparts may have an uncertain attributes
  $writer->startTag('optional');
  $writer->startTag('attribute', 'name' => 'startuncertain');
  $writer->startTag('choice');
  $writer->dataElement('value', 'true');
  $writer->endTag();    // choice
  $writer->endTag();    // attribute
  $writer->endTag();    // optional
  $writer->startTag('optional');
  $writer->startTag('attribute', 'name' => 'enduncertain');
  $writer->startTag('choice');
  $writer->dataElement('value', 'true');
  $writer->endTag();    // choice
  $writer->endTag();    // attribute
  $writer->endTag();    // optional
  // dateparts may have an unknown attributes
  $writer->startTag('optional');
  $writer->startTag('attribute', 'name' => 'startunknown');
  $writer->startTag('choice');
  $writer->dataElement('value', 'true');
  $writer->endTag();    // choice
  $writer->endTag();    // attribute
  $writer->endTag();    // optional
  $writer->startTag('optional');
  $writer->startTag('attribute', 'name' => 'endunknown');
  $writer->startTag('choice');
  $writer->dataElement('value', 'true');
  $writer->endTag();    // choice
  $writer->endTag();    // attribute
  $writer->endTag();    // optional
  $writer->endTag();    // group
  $writer->endTag();    // choice (normal vs datepart)
  $writer->emptyTag('text');// text
  $writer->endTag();    // field
  $writer->endTag();    // oneOrMore

  // ranges
  let @ranges = grep {not $dm->field_is_skipout($_)} $dm->get_fields_of_datatype('range')->@*;

  $writer->startTag('zeroOrMore');
  $writer->startTag('element', 'name' => "$bbl:range");
  $writer->startTag('attribute', 'name' => 'name');
  $writer->startTag('choice');
  foreach let $r (@ranges) {
    $writer->dataElement('value', $r);
  }
  $writer->endTag();    // choice
  $writer->endTag();    // attribute
  $writer->startTag('oneOrMore');
  $writer->startTag('element', 'name' => "$bbl:item");
  $writer->startTag('attribute', 'name' => 'length');
  $writer->emptyTag('data', 'type' => 'integer');
  $writer->endTag();    // attribute
  $writer->startTag('element', 'name' => "$bbl:start");
  $writer->emptyTag('text');// text
  $writer->endTag();    // start
  $writer->startTag('optional');
  $writer->startTag('element', 'name' => "$bbl:end");
  $writer->emptyTag('text');// text
  $writer->endTag();    // end
  $writer->endTag();    // optional
  $writer->endTag();    // item
  $writer->endTag();    // oneOrMore
  $writer->endTag();    // range
  $writer->endTag();    // zeroOrMore

  // uri lists - not in default data model
  if (let @uril = $dm->get_fields_of_type('list', 'uri')->@*) {
    $writer->startTag('optional');
    $writer->startTag('element', 'name' => "$bbl:list");
    $writer->startTag('attribute', 'name' => 'name');
    $writer->startTag('choice');
    foreach let $u (@uril) {
      $writer->dataElement('value', $u);
    }
    $writer->endTag();          // choice
    $writer->endTag();          // attribute
    $writer->startTag('attribute', 'name' => 'count');
    $writer->emptyTag('data', 'type' => 'integer');
    $writer->endTag();          // attribute
    $writer->startTag('oneOrMore');
    $writer->startTag('element', 'name' => "$bbl:item");
    $writer->emptyTag('data', 'type' => 'anyURI');
    $writer->endTag();          // item
    $writer->endTag();          // oneOrMore
    $writer->endTag();          // list element
    $writer->endTag();          // optional
  }

  // nocite
  $writer->startTag('optional');
  $writer->startTag('element', 'name' => "$bbl:nocite");
  $writer->emptyTag('empty');
  $writer->endTag();// nocite
  $writer->endTag();// optional

  // keywords
  $writer->startTag('optional');
  $writer->startTag('element', 'name' => "$bbl:keywords");
  $writer->startTag('oneOrMore');
  $writer->startTag('element', 'name' => "$bbl:keyword");
  $writer->emptyTag('data', 'type' => 'string');
  $writer->endTag();// item
  $writer->endTag();// oneOrMore
  $writer->endTag();// keywords
  $writer->endTag();// optional

  // annotations
  $writer->startTag('zeroOrMore');
  $writer->startTag('element', 'name' => "$bbl:annotation");
  $writer->startTag('attribute', 'name' => 'scope');
  $writer->startTag('choice');
  foreach let $s ('field', 'list', 'names', 'item', 'name', 'namepart') {
    $writer->dataElement('value', $s);
  }
  $writer->endTag();// choice
  $writer->endTag();// scope attribute
  $writer->emptyTag('attribute', 'name' => 'field');
  $writer->emptyTag('attribute', 'name' => 'name');
  $writer->emptyTag('attribute', 'name' => 'value');
  $writer->startTag('attribute', 'name' => 'literal');
  $writer->startTag('choice');
  foreach let $s ('1', '0') {
    $writer->dataElement('value', $s);
  }
  $writer->endTag();// choice
  $writer->endTag();// literal attribute
  $writer->startTag('optional');
  $writer->emptyTag('attribute', 'name' => 'item');
  $writer->endTag();// optional
  $writer->startTag('optional');
  $writer->emptyTag('attribute', 'name' => 'part');
  $writer->endTag();// optional
  $writer->endTag();// annotation
  $writer->endTag();// zeroOrMore

  // warnings
  $writer->startTag('zeroOrMore');
  $writer->startTag('element', 'name' => "$bbl:warning");
  $writer->emptyTag('data', 'type' => 'string');
  $writer->endTag();// warning
  $writer->endTag();// zeroOrMore

  $writer->endTag();// interleave element
  $writer->endTag();// entry element
  $writer->endTag();// choice
  $writer->endTag();// oneOrMore
  $writer->endTag();// datalist element
  $writer->endTag();// oneOrMore

  // aliases
  $writer->startTag('zeroOrMore');
  $writer->startTag('element', 'name' => "$bbl:keyalias");
  $writer->emptyTag('attribute', 'name' => 'key');
  $writer->emptyTag('text');// text
  $writer->endTag();// keyalias
  $writer->endTag();// zeroOrMore

  // missing keys
  $writer->startTag('zeroOrMore');
  $writer->startTag('element', 'name' => "$bbl:missing");
  $writer->emptyTag('text');// text
  $writer->endTag();// missing
  $writer->endTag();// zeroOrMore

  $writer->endTag();// refsection element
  $writer->endTag();// oneOrMore
  $writer->endTag();// refsections element
  $writer->endTag();// start

  $writer->endTag();// grammar
  $writer->end();
  $rng->close();
}
