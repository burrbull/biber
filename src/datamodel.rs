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

pub struct DataModel;

impl DataModel {
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
    if (crate::Config->getoption("no_default_datamodel")) {
      $dm = $dms->[1];
    }
    else { // we want to add/modify the default datamodel using the user-supplied subset
      if (let $udm = $dms->[1]) {

        // Constants
        for uc in ($udm->{constants}{constant}->@*) {
          let $uce = firstidx {unicase::eq($_->{name}, $uc->{name})} $dm->{constants}{constant}->@*;
          if ($uce >= 0) { // since constants are named, we can overwrite easily
            $dm->{constants}{constant}[$uce] = $uc;
          }
          else {
            push $dm->{constants}{constant}->@*, $uc;
          }
        }

        // Constraints
        for uc in ($udm->{constraints}[0]{constraint}->@*) {
          push $dm->{constraints}[0]{constraint}->@*, $uc;
        }

        // Entryfields
        for uef in ($udm->{entryfields}->@*) {
          if (let $et = $uef->{entrytype}) {
            let $ef = firstidx {$_->{entrytype}[0]{content} && (unicase::eq($_->{entrytype}[0]{content}, $et->[0]{content}))} $dm->{entryfields}->@*;
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
        for et in ($udm->{entrytypes}{entrytype}->@*) {
          push $dm->{entrytypes}{entrytype}->@*, $et;
        }

        // Fields
        for f in ($udm->{fields}{field}->@*) {
          let $df = firstidx {unicase::eq($_->{content}, $f->{content}) } $dm->{fields}{field}->@*;
          if ($df >= 0) {
            $dm->{fields}{field}->[$df] = $f;
          }
          else {
            push $dm->{fields}{field}->@*, $f;
          }
        }

        // Multiscriptfields
        for f in ($udm->{multiscriptfields}{field}->@*) {
          push $dm->{multiscriptfields}{field}->@*, $f;
        }
      }
    }

    // First, we normalise all entrytypes and fields to case-folded form for internal
    // comparisons but we save a map of case-folded variants to actual names
    // so that we can recover the information later for output
    for et in ($dm->{entrytypes}{entrytype}->@*) {
      $self->{casemap}{foldtoorig}{UniCase::new($et->{content})} = $et->{content};
      $et->{content} = UniCase::new($et->{content});
    }
    for f in ($dm->{fields}{field}->@*) {
      $self->{casemap}{foldtoorig}{UniCase::new($f->{content})} = $f->{content};
      $f->{content} = UniCase::new($f->{content});
    }

    // Early check for fatal datamodel errors
    // Make sure dates are named *date. A lot of code relies on this.
    for date in (grep {$_->{datatype} == DataType::Date} $dm->{fields}{field}->@*) {
      if !($date->{content} =~ m/date$/) {
        biber_error("Fatal datamodel error: date field '" . $date->{content} . "' must end with string 'date'");
      }
    }

    // Multiscript enabled fields
    for f in ($dm->{multiscriptfields}{field}->@*) {
      self.multiscriptfields.insert(f)
    }

    // Pull out legal entrytypes, fields and constraints and make lookup hash
    // for quick tests later
    for f in ($dm->{fields}{field}->@*) {

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

      $self->{fieldsbyname}{$f->{content}} = {"fieldtype"   => f.fieldtype,
                                              "datatype"    => f.datatype,
                                              "format"      => f.format.unwrap_or(Format::Default)};
      if ($f->{format}) {
        push $self->{fieldsbytype}{$f->{fieldtype}}{$f->{datatype}}{$f->{format}}->@*, $f->{content};
      }
      push $self->{fieldsbytype}{$f->{fieldtype}}{$f->{datatype}}{'*'}->@*, $f->{content};
      push $self->{fieldsbyfieldtype}{$f->{fieldtype}}->@*, $f->{content};
      push $self->{fieldsbydatatype}{$f->{datatype}}->@*, $f->{content};
      push $self->{fieldsbyformat}{f.format.unwrap_or(Format::Default)}->@*, $f->{content};

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
    for constant in ($dm->{constants}{constant}->@*) {
      $self->{constants}{$constant->{name}}{type} = $constant->{type};
      $self->{constants}{$constant->{name}}{value} = $constant->{content};
    }

    for et in ($dm->{entrytypes}{entrytype}->@*) {
      let $es = $et->{content};

      // Skip output flag for certain entrytypes
      if ($et->{skip_output}) {
        $self->{entrytypesbyname}->{$es}{skipout} = 1;
      }
      // fields for entrytypes
      for ef in ($dm->{entryfields}->@*) {
        // Found a section describing legal fields for entrytype
        if (!exists($ef->{entrytype}) ||
            grep {$_->{content} == $es} $ef->{entrytype}->@*) {
          for f in ($ef->{field}->@*) {
            $self->{entrytypesbyname}{$es}{legal_fields}{$f->{content}} = 1;
          }
        }
      }

      // constraints
      for cd in ($dm->{constraints}->@*) {
        // Found a section describing constraints for entrytype
        if (!exists($cd->{entrytype}) ||
            grep {$_->{content} == $es} $cd->{entrytype}->@*) {
          for c in ($cd->{constraint}->@*) {
            if ($c->{type} == "mandatory") {
              // field
              for f in ($c->{field}->@*) {
                push $self->{entrytypesbyname}{$es}{constraints}{mandatory}->@*, $f->{content};
              }
              // xor set of fields
              // [ XOR, field1, field2, ... , fieldn ]
              for fxor in ($c->{fieldxor}->@*) {
                let $xorset;
                for f in ($fxor->{field}->@*) {
                  push $xorset->@*, $f->{content};
                }
                unshift $xorset->@*, "XOR";
                push $self->{entrytypesbyname}{$es}{constraints}{mandatory}->@*, $xorset;
              }
              // or set of fields
              // [ OR, field1, field2, ... , fieldn ]
              for for in ($c->{fieldor}->@*) {
                let $orset;
                for f in ($for->{field}->@*) {
                  push $orset->@*, $f->{content};
                }
                unshift $orset->@*, "OR";
                push $self->{entrytypesbyname}{$es}{constraints}{mandatory}->@*, $orset;
              }
            }
            // Conditional constraints
            // [ ANTECEDENT_QUANTIFIER
            //   [ ANTECEDENT LIST ]
            //   CONSEQUENT_QUANTIFIER
            //   [ CONSEQUENT LIST ]
            // ]
            else if ($c->{type} == "conditional") {
              let $cond;
              $cond->[0] = $c->{antecedent}{quant};
              $cond->[1] = [ map { $_->{content} } $c->{antecedent}{field}->@* ];
              $cond->[2] = $c->{consequent}{quant};
              $cond->[3] = [ map { $_->{content} } $c->{consequent}{field}->@* ];
              push $self->{entrytypesbyname}{$es}{constraints}{conditional}->@*, $cond;
            }
            // data constraints
            else if ($c->{type} == "data") {
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
    $self->{helpers} = {namelistsall => [sort $self.get_fields_of_type(FieldType::List, &[DataType::Name], None)->@*],
                        namelists => [sort grep
                                      {not $self->field_is_skipout($_)}
                                      $self->get_fields_of_type(FieldType::List, &[DataType::Name], None)->@*],
                        lists     => [sort grep
                                      {
                                        !$self->field_is_datatype("name", $_) &&
                                          !$self->field_is_skipout($_) &&
                                            !$self->field_is_datatype("verbatim", $_) &&
                                              !$self->field_is_datatype("uri", $_)
                                      }
                                      $self->get_fields_of_fieldtype("list")->@*],
                        fields    => [sort grep
                                      {
                                        !$self->field_is_skipout($_) &&
                                          !$self->get_fieldformat($_) == "xsv"
                                      }
                                      self.get_fields_of_type(FieldType::Field,
                                                                &[DataType::Entrykey,
                                                                DataType::Key,
                                                                DataType::Integer,
                                                                DataType::Datepart,
                                                                DataType::Literal,
                                                                DataType::Code], None)->@*],
                        datefields   => [sort self.get_fields_of_type(FieldType::Field, &[DataType::Date], None)->@*],
                        dateparts    => [sort self.get_fields_of_type(FieldType::Field, &[DataType::Datepart], None)->@*],
                        xsv       => [sort grep
                                      {
                                        !$self->field_is_skipout($_)
                                      }
                                      $self->get_fields_of_fieldformat("xsv")->@*],
                        ranges    => [sort grep
                                      {
                                        !$self->field_is_skipout($_)
                                      }
                                      self.get_fields_of_datatype(&[DataType::Range])->@*],
                        uris      => [sort grep
                                      {
                                        !$self->field_is_skipout($_);
                                      }
                                      $self->get_fields_of_type(FieldType::Field, &[DataType::Uri], None)->@*],
                        urils     => [sort grep
                                      {
                                        !$self->field_is_skipout($_);
                                      }
                                      $self->get_fields_of_type(FieldType::List, &[DataType::Uri], None)->@*],
                        verbs     => [sort grep
                                      {
                                        !$self->field_is_skipout($_);
                                      }
                                      self.get_fields_of_datatype(&[DataType::Verbatim, DataType::Uri])->@*],
                        vfields   => [sort grep
                                      {
                                        !$self->field_is_skipout($_);
                                      }
                                      $self.get_fields_of_type(FieldType::Field, &[DataType::Verbatim, DataType::Uri], None)->@*],
                        vlists    => [sort grep
                                      {
                                        !$self->field_is_skipout($_);
                                      }
                                      $self->get_fields_of_type(FieldType::List, &[DataType::Verbatim, DataType::Uri], None)->@*],
                        integers  => [sort self.get_fields_of_datatype(&[DataType::Datepart, DataType::Integer])->@*]
                      };
    // Mapping of sorting fields to Sort::Key sort data types which are not "str"
    $self->{sortdataschema} = |f| {
      if (first {$f == $_} ("citeorder", "citecount", self.helpers{integers}->@*)) {
        return "int";
      }
      else {
        return "str";
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
    if ($self->{constants}{$name}{type} == "list") {
      return split(/\s*,\s*/, $self->{constants}{$name}{value});
    }
    else if ($self->{constants}{$name}{type} == "string") {
      return $self->{constants}{$name}{value};
    }
  }

  /// Returns boolean to say if a field is a multiscript field
  fn is_multiscript(self, field: &str) -> bool {
    self.multiscriptfields.contains(field)
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
  fn is_field(&self, field: &str) -> bool {
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
  fn is_entrytype(&self, type) -> bool {
    return $self->{entrytypesbyname}{$type} ? 1 : 0;
  }

  /// Returns boolean to say if a field is legal for an entrytype
  fn is_field_for_entrytype(&self, type, field) -> bool {
    if ($self->{entrytypesbyname}{$type}{legal_fields}{$field}) {
      return 1;
    }
    else {
      return 0;
    }
  }

  /// Returns boolean depending on whether an entrytype is to be skipped on output
  fn entrytype_is_skipout(&self, $type) {
    return $self->{entrytypesbyname}{$type}{skipout}.unwrap_or(0);
  }

  /// Retrieve fields of a certain biblatex fieldtype from data model
  /// Return in sorted order so that bbl order doesn't change when changing
  /// .bcf. This really messes up tests otherwise.
  fn get_fields_of_fieldtype(&self, $fieldtype) {
    let $f = $self->{fieldsbyfieldtype}{$fieldtype};
    return $f ? [ sort $f->@* ] : [];
  }

  /// Retrieve fields of a certain format from data model
  /// Return in sorted order so that bbl order doesn't change when changing
  /// .bcf. This really messes up tests otherwise.
  fn get_fields_of_fieldformat(&self, $format) {
    let $f = $self->{fieldsbyformat}{$format};
    return $f ? [ sort $f->@* ] : [];
  }

  /// Retrieve fields of a certain biblatex datatype from data model
  /// Return in sorted order so that bbl order doesn't change when changing
  /// .bcf. This really messes up tests otherwise.
  fn get_fields_of_datatype(&self, datatype: &[DataType]) {
    let f = Vec::new();
    // datatype can be array ref of datatypes - makes some calls cleaner
    for dt in &datatype {
      if (let $fs = self.fieldsbydatatype.get(dt) {
        f.push(fs);
      }
    }
    return [ sort @f ];
  }

  /// Retrieve fields of a certain biblatex type from data model
  /// Return in sorted order so that bbl order doesn't change when changing
  /// .bcf. This really messes up tests otherwise.
  fn get_fields_of_type(self, fieldtype: FieldType, datatype: &[DataType], format: Option<Format>) -> Vec<Unknown> {
    let f = Vec::new();
    let format = format.unwrap_or("*");

    // datatype can be array ref of datatypes - makes some calls cleaner
    for dt in datatype.into_iter() {
      if (let $fs = self.fieldsbytype{$fieldtype}{$dt}{$format}) {
        f.push(fs);
      }
    }

    f.sort();
    f
  }

  /// Returns boolean to say if the given fieldtype/datatype/format is a valid combination
  fn is_fields_of_type(&self, fieldtype: FieldType, datatype: DataType, format: Option<Format>) -> bool {
    if let Some(format) = format {
      return exists($self->{fieldsbytype}{$fieldtype}{$datatype}{$format}) ? 1 : 0;
    }
    else {
      return exists($self->{fieldsbytype}{$fieldtype}{$datatype}) ? 1 : 0;
    }
  }

  /// Returns the fieldtype of a field
  fn get_fieldtype(&self, field: &str) -> FieldType {
    self.fieldsbyname.get(field).unwrap().fieldtype
  }

  /// Returns the datatype of a field
  fn get_datatype(&self, field: &str) -> Option<DataType> {
    self.fieldsbyname.get(field).and_then(|f| f.datatype)
  }

  /// Returns the format of a field
  fn get_fieldformat(self, $field) {
    return $self->{fieldsbyname}{$field}{format};
  }

  /// Returns the fieldtype, datatype and format of a field
  fn get_dm_for_field(self, field: &str) {
    return {"fieldtype" =>  $self->{fieldsbyname}{$field}{fieldtype},
            "datatype"  => $self->{fieldsbyname}{$field}{datatype},
            "format"    => $self->{fieldsbyname}{$field}{format}};
  }

  /// Returns boolean depending on whether a field is a certain biblatex fieldtype
  fn field_is_fieldtype(&self, fieldtype: FieldType, field: &str) -> bool {
    match self.fieldsbyname.get(field) {
      Some(f) if (f.fieldtype == fieldtype) => true,
      _ => false
    }
  }

  /// Returns boolean depending on whether a field is a certain biblatex datatype
  fn field_is_datatype(&self, &datatype: DataType, field: &str) -> bool {
    match self.fieldsbyname.get(field) {
      Some(f) if (f.datatype == datatype) => true,
      _ => false
    }
  }

  ///  Returns boolean depending on whether a field is a certain biblatex fieldtype
  /// and datatype
  fn field_is_type(&self, fieldtype: FieldType, datatype: DataType, field: &str) -> bool {
    match self.fieldsbyname.get(field) {
      Some(f) if (f.fieldtype == fieldtype && f.datatype == datatype) => true,
      _ => false
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
  fn check_mandatory_constraints(self, be: &mut Entry) -> Vec<String> {
    let secnum = crate::MASTER.get_current_section();
    let section = crate::MASTER.sections().get_section(secnum);
    let warnings = Vec::new();
    let et = be.get_field("entrytype");
    let key = be.get_field("citekey");
    let ds = section.get_keytods(key);
  // ["title", ["OR", "url", "doi", "eprint"]]
    for c in ($self->{entrytypesbyname}{$et}{constraints}{mandatory}->@*) {
      if (ref($c) == "ARRAY") {
        // Exactly one of a set is mandatory
        if (c[0] == "XOR") {
          let fs = &c[1..]; // Lose the first element which is the "XOR"
          let mut flag = false;
          let mut xorflag = false;
          for of in &fs {
            if b.field_exists(of) &&
                // ignore date field if it has been split into parts
                !(of == "date" && be.get_field("datesplit")) {
              if xorflag {
                warnings.push(format!("Datamodel: Entry '{key}' ({ds}): Mandatory fields - only one of '{}' must be defined - ignoring field '{of}'", fs.join(", ")));
                be.del_field(of);
              }
              flag = true;
              xorflag = true;
            }
          }
          if !flag {
            warnings.push(format!("Datamodel: Entry '{key}' ({ds}): Missing mandatory field - one of '{}' must be defined"), fs.join(", "));
          }
        }
        // One or more of a set is mandatory
        else if (c[0] == "OR") {
          let fs = &c[1..]; // Lose the first element which is the "OR"
          let mut flag = false;
          for of in &fs {
            if (be.field_exists(of)) {
              flag = true;
              break;
            }
          }
          if !flag {
            warnings.push(format!("Datamodel: Entry '{key}' ({ds}): Missing mandatory field - one of '{}' must be defined", join(', ', @fs)));
          }
        }
      }
      // Simple mandatory field
      else {
        if !be.field_exists(c) {
          warnings.push(format!("Datamodel: Entry '{key}' ({ds}): Missing mandatory field '{c}'"));
        }
      }
    }
    warnings
  }

  /// Checks constraints of type "conditional" on entry and
  /// returns an arry of warnings, if any
  fn check_conditional_constraints(self, be) -> Vec<String> {
    let secnum = crate::MASTER.get_current_section();
    let section = crate::MASTER.sections().get_section(secnum);
    let warnings = Vec::new();
    let et = be.get_field("entrytype");
    let key = be.get_field("citekey");
    let ds = section.get_keytods(key);

    for c in ($self->{entrytypesbyname}{$et}{constraints}{conditional}->@*) {
      let aq  = c[0];          // Antecedent quantifier
      let afs = c[1];          // Antecedent fields
      let cq  = c[2];          // Consequent quantifier
      let cfs = c[3];          // Consequent fields
      let actual_afs = afs.iter().filter(|cf| be.field_exists(cf)).collect(); // antecedent fields in entry
      // check antecedent
      if aq == "all" {
        if !(afs.len() == actual_afs.len()) { // ALL -> ? not satisfied
          continue;
        }
      }
      else if aq == "none" {
        if !actual_afs.len() {      // NONE -> ? not satisfied
          continue;
        }
      }
      else if aq == "one" {
        if actual_afs.len() {  // ONE -> ? not satisfied
          continue;
        }
      }

      // check consequent
      let actual_cfs = cfs.iter().filter(|cf| be.field_exists(cf)).collect();
      if cq == "all" {
        if !(cfs.len() == actual_cfs.len()) { // ? -> ALL not satisfied
          warnings.push(format!("Datamodel: Entry '{key}' ({ds}): Constraint violation - {cq} of fields ({}) must exist when {aq} of fields ({}) exist", cfs.join(", "), afs.join(", ")));
        }
      }
      else if cq == "none" {
        if !actual_cfs.is_empty() {        // ? -> NONE not satisfied
          warnings.push(format!("Datamodel: Entry '{key}' ({ds}): Constraint violation - {cq} of fields ({}) must exist when {aq} of fields ({}) exist. Ignoring them.", actual_cfs.join(", "), afs.join(", ")));
          // delete the offending fields
          for f in &actual_cfs {
            be.del_field(f);
          }
        }
      }
      else if cq == "one" {
        if actual_cfs.is_empty() {    // ? -> ONE not satisfied
          warnings.push(format!("Datamodel: Entry '{key}' ({ds}): Constraint violation - {cq} of fields ({}) must exist when {aq} of fields ({}) exist", cfs.join(", "), afs.join(", ")));
        }
      }
    }
    warnings
  }

  /// Checks constraints of type "data" on entry and
  /// returns an array of warnings, if any
  fn check_data_constraints(self, be) -> Vec<String> {
    let secnum = crate::MASTER.get_current_section();
    let section = crate::MASTER.sections().get_section(secnum);
    let warnings = Vec::new();
    let et = be.get_field("entrytype");
    let key = be.get_field("citekey");
    let ds = section.get_keytods(key);

    for c in ($self->{entrytypesbyname}{$et}{constraints}{data}->@*) {
      // This is the datatype of the constraint, not the field!
      if c.datatype == DataType::Isbn {
        for f in ($c->{fields}->@*) {
          if (let $fv = be.get_field(f)) {

            // Treat as a list field just in case someone has made it so in a custom datamodel
            if self.get_fieldtype(f) != FieldType::List {
              $fv = [$fv];
            }
            for i in fv {
              if (!$DM_DATATYPES{isbn}->(i, f)) {
                warnings.push(format!("Datamodel: Entry '{key}' ({ds}): Invalid ISBN in value of field '{f}'"));
              }
            }
          }
        }
      }
      else if c.datatype == DataType::Issn {
        for f in ($c->{fields}->@*) {
          if (let $fv = be.get_field(f)) {

            // Treat as a list field just in case someone has made it so in a custom datamodel
            if self.get_fieldtype(f) != FieldType::List {
              $fv = [$fv];
            }
            for i in fv {
              if (!$DM_DATATYPES{issn}->(i)) {
                warnings.push(format!("Datamodel: Entry '{key}' ({ds}): Invalid ISSN in value of field '{f}'"));
              }
            }
          }
        }
      }
      else if c.datatype == DataType::Ismn {
        for f in ($c->{fields}->@*) {
          if (let $fv = be.get_field(f)) {

            // Treat as a list field just in case someone has made it so in a custom datamodel
            if self.get_fieldtype(f) != FieldType::List {
              $fv = [$fv];
            }
            for i in fv {
              if (!$DM_DATATYPES{ismn}->(i)) {
                warnings.push(format!("Datamodel: Entry '{key}' ({ds}): Invalid ISMN in value of field '{f}'"));
              }
            }
          }
        }
      }
      else if c.datatype == DataType::Integer || c.datatype == DataType::Datepart {
        for f in ($c->{fields}->@*) {
          if (let $fv = be.get_field(f)) {
            if (let $fmin = $c->{rangemin}) {
              if !(fv >= fmin) {
                warnings.push(format!("Datamodel: Entry '{key}' ({ds}): Invalid value of field '{f}' must be '>={fmin}' - ignoring field"));
                be.del_field(f);
                continue;
              }
            }
            if (let $fmax = $c->{rangemax}) {
              if !(fv <= fmax) {
                warnings.push(format!("Datamodel: Entry '{key}' ({ds}): Invalid value of field '{f}' must be '<={fmax}' - ignoring field"));
                be.del_field(f);
                continue;
              }
            }
          }
        }
      }
      else if c.datatype == DataType::Pattern {
        let $patt;
        if !($patt = $c->{pattern}) {
          warnings.push("Datamodel: Pattern constraint has no pattern!".into());
        }
        for f in ($c->{fields}->@*) {
          if (let $fv = be.get_field(f)) {
            if !(imatch($fv, $patt)) {
              warnings.push(format!("Datamodel: Entry '{key}' ({ds}): Invalid value (pattern match fails) for field '{f}'"));
            }
          }
        }
      }
    }
    warnings
  }

  /// Checks datatypes of fields against fields. These are not explicit constraints
  /// in the datamodel but rather checks of the datatype of fields in the datamodel.
  fn check_datatypes(self, be: &Entry) -> Vec<String> {
    let secnum = crate::MASTER.get_current_section();
    let section = crate::MASTER.sections().get_section(secnum);
    let warnings = Vec::new();
    let et = be.get_field("entrytype");
    let key = be.get_field("citekey");
    let ds = section.get_keytods(key);

    for f in be.fields() {
      let fv = be.get_field(f);
      let fdt = self.get_datatype(f);
      let fft = self.get_fieldtype(f);
      let ffmt = self.get_fieldformat(f);
      // skip special fields which are not in the datamodel such as:
      // citekey, entrykey, rawdata, datatype
      if fdt.is_none() {
        continue;
      }
      let $dt = exists($DM_DATATYPES{fdt}) ? $DM_DATATYPES{fdt} : $DM_DATATYPES{default};
      if (fft == FieldType::List && fdt != DataType::Name) || ffmt == Format::Xsv {
        $dt = $DM_DATATYPES{list};
      }

      // Fields which are allowed to be null and are indeed null are fine
      // These can mess up further tests so weed them out now
      if self.field_is_nullok(f) && fv == "" {
        continue;
      }

      if !($dt->($fv, f)) {
        warnings.push(format!("Datamodel: Entry '{key}' ({ds}): Invalid value of field '{f}' must be datatype '{fdt}' - ignoring field"));
        be.del_field(f);
      }
    }

    warnings
  }

  /// Dump crate::DataModel object
  fn dump(self) {
    return pp($self);
  }

  /// Generate a RelaxNG XML schema from the datamodel for BibLaTeXML datasources
  fn generate_bltxml_schema(dm, outfile) {
    if dm.bltxml_schema_gen_done {
      return;
    }

    // Set the .rng path to the output dir, if specified
    if (let $outdir = crate::Config->getoption("output_directory")) {
      let (undef, undef, $file) = File::Spec->splitpath($outfile);
      $outfile = File::Spec->catfile($outdir, $file)
    }
    let $rng = IO::File->new($outfile, '>:encoding(UTF-8)');
    $rng->autoflush;// Needed for running tests to string refs

    info!("Writing BibLaTeXML RNG schema '{}' for datamodel", outfile);
    require XML::Writer;
    let $bltx_ns = "http://biblatex-biber.sourceforge.net/biblatexml";
    let $bltx = "bltx";
    let $default_ns = "http://relaxng.org/ns/structure/1.0";
    let $writer = new XML::Writer(NAMESPACES   => 1,
                                ENCODING     => "UTF-8",
                                DATA_MODE    => 1,
                                DATA_INDENT  => 2,
                                OUTPUT       => $rng,
                                PREFIX_MAP   => {$bltx_ns    => $bltx,
                                                  $default_ns => ""});

    $writer->xmlDecl();
    $writer->comment("Auto-generated from .bcf Datamodel");
    $writer->forceNSDecl($default_ns);
    $writer->forceNSDecl($bltx_ns);
    $writer->startTag("grammar",
                      "datatypeLibrary" => "http://www.w3.org/2001/XMLSchema-datatypes");
    $writer->startTag("start");
    $writer->startTag("element", "name" => "$bltx:entries");
    $writer->startTag("oneOrMore");
    $writer->startTag("element", "name" => "$bltx:entry");
    $writer->emptyTag("attribute", "name" => "id");
    $writer->startTag("attribute", "name" => "entrytype");
    $writer->startTag("choice");
    for entrytype in ($dm->entrytypes->@*) {
      $writer->dataElement("value", $entrytype);
    }
    $writer->endTag();// choice
    $writer->endTag();// attribute
    $writer->startTag("interleave");

    for ft in &dm.fieldtypes {
      for dt in &dm.datatypes {
        if dm.is_fields_of_type(ft, dt, None) {
          if dt == DataType::Datepart { // not legal in input, only output
            continue;
          }
          $writer->comment(format!("{dt} {ft}s"));
          $writer->emptyTag("ref", "name" => format!("{dt}{ft}"));
        }
      }
    }

    // Annotations
    $writer->emptyTag("ref", "name" => "mannotation");

    $writer->endTag();// interleave
    $writer->endTag();// entry element
    $writer->endTag();// oneOrMore
    $writer->endTag();// entries element
    $writer->endTag();// start

    for ft in &dm.fieldtypes {
      for dt in &dm.datatypes {
        if dm.is_fields_of_type(ft, dt, None) {
          if dt == DataType::Datepart { // not legal in input, only output
            continue;
          }
          $writer->comment(format!("{dt} {ft}s definition"));
          $writer->startTag("define", "name" => format!("{dt}{ft}"));

          // Name lists element definition
          // =============================
          if (ft == FieldType::List && dt == DataType::Name) {
            $writer->startTag("zeroOrMore");// for example, XDATA doesn't need a name
            $writer->startTag("element", "name" => format!("{bltx}:names"));

            $writer->startTag("choice");
            // xdata attribute ref
            $writer->emptyTag("ref", "name" => "xdata");

            $writer->startTag("group");
            // useprefix attribute
            $writer->comment("useprefix option");
            $writer->startTag("optional");
            $writer->startTag("attribute", "name" => "useprefix");
            $writer->emptyTag("data", "type" => "boolean");
            $writer->endTag();    // attribute
            $writer->endTag();    // optional

            // sortingnamekeytemplatename attribute
            $writer->comment("sortingnamekeytemplatename option");
            $writer->startTag("optional");
            $writer->startTag("attribute", "name" => "sortingnamekeytemplatename");
            $writer->emptyTag("data", "type" => "string");
            $writer->endTag();    // attribute
            $writer->endTag();    // optional

            // type attribute
            $writer->comment("types of names elements");
            $writer->startTag("attribute", "name" => "type");
            $writer->startTag("choice");
            for name in dm.get_fields_of_type(ft, &[dt], None) {
              $writer->dataElement("value", $name);
            }
            $writer->endTag();    // choice
            $writer->endTag();    // attribute

            // morenames attribute
            $writer->startTag("optional");
            $writer->startTag("attribute", "name" => "morenames");
            $writer->emptyTag("data", "type" => "boolean");
            $writer->endTag();    // attribute
            $writer->endTag();    // optional
            $writer->startTag("oneOrMore");

            // Individual name element
            $writer->startTag("element", "name" => format!("{bltx}:name"));

            $writer->startTag("choice");
            // xdata attribute ref
            $writer->emptyTag("ref", "name" => "xdata");

            $writer->startTag("group");
            // useprefix attribute
            $writer->comment("useprefix option");
            $writer->startTag("optional");
            $writer->startTag("attribute", "name" => "useprefix");
            $writer->emptyTag("data", "type" => "boolean");
            $writer->endTag();    // attribute
            $writer->endTag();    // optional

            // sortingnamekeytemplatename attribute
            $writer->comment("sortingnamekeytemplatename option");
            $writer->startTag("optional");
            $writer->startTag("attribute", "name" => "sortingnamekeytemplatename");
            $writer->emptyTag("data", "type" => "string");
            $writer->endTag();    // attribute
            $writer->endTag();    // optional

            // gender attribute ref
            $writer->emptyTag("ref", "name" => "gender");
            // namepart element
            $writer->startTag("oneOrMore");
            $writer->startTag("element", "name" => format!("{bltx}:namepart"));
            $writer->startTag("attribute", "name" => "type");
            $writer->startTag("choice");
            for np in ($dm->get_constant_value("nameparts")) {// list type so returns list
              $writer->dataElement("value", $np);
            }

            $writer->endTag();    // choice
            $writer->endTag();    // attribute
            $writer->startTag("optional");
            $writer->emptyTag("attribute", "name" => "initial");
            $writer->endTag();    // optional
            $writer->startTag("choice");
            $writer->startTag("oneOrMore");
            $writer->startTag("element", "name" => format!("{bltx}:namepart"));
            $writer->startTag("optional");
            $writer->emptyTag("attribute", "name" => "initial");
            $writer->endTag();    // optional
            $writer->emptyTag("text");// text
            $writer->endTag();    // (sub)namepart element
            $writer->endTag();    // oneOrMore
            $writer->emptyTag("text");// text
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
          else if ft == FieldType::List {
            // lists element definition
            // ========================
            $writer->startTag("interleave");
            for list in dm.get_fields_of_type(ft, &[dt], None) {
              $writer->startTag("optional");
              $writer->startTag("element", "name" => format!("{bltx}:$list"));
              $writer->startTag("choice");
              $writer->emptyTag("ref", "name" => "xdata");
              $writer->startTag("choice");
              $writer->emptyTag("text");// text
              $writer->startTag("element", "name" => format!("{bltx}:list"));
              $writer->startTag("oneOrMore");
              $writer->startTag("element", "name" => format!("{bltx}:item"));
              $writer->startTag("choice");
              $writer->emptyTag("ref", "name" => "xdata");
              $writer->emptyTag("text");// text
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
          else if ft == FieldType::Field && dt == DataType::Uri {
            // uri field element definition
            // ============================
            $writer->startTag("interleave");
            for field in dm.get_fields_of_type(ft, &[dt], None) {
              $writer->startTag("optional");
              $writer->startTag("element", "name" => format!("{bltx}:{field}"));
              $writer->startTag("choice");
              $writer->emptyTag("ref", "name" => "xdata");
              $writer->emptyTag("data", "type" => "anyURI");
              $writer->endTag();   // choice
              $writer->endTag();   // $field element
              $writer->endTag();// optional
            }
            $writer->endTag();// interleave
            // ============================
          }
          else if ft == FieldType::Field && dt == DataType::Range {
            // range field element definition
            // ==============================
            $writer->startTag("interleave");
            for field in dm.get_fields_of_type(ft, &[dt], None) {
              $writer->startTag("optional");
              $writer->startTag("element", "name" => format!("{bltx}:{field}"));

              $writer->startTag("choice");
              // xdata attribute ref
              $writer->emptyTag("ref", "name" => "xdata");

              $writer->startTag("element", "name" => format!("{bltx}:list"));
              $writer->startTag("oneOrMore");
              $writer->startTag("element", "name" => format!("{bltx}:item"));

              $writer->startTag("choice");
              // xdata attribute ref
              $writer->emptyTag("ref", "name" => "xdata");
              $writer->startTag("group");
              $writer->startTag("element", "name" => format!("{bltx}:start"));
              $writer->emptyTag("text");
              $writer->endTag();  // start element
              $writer->startTag("element", "name" => format!("{bltx}:end"));
              $writer->startTag("choice");
              $writer->emptyTag("text");
              $writer->emptyTag("empty");
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
          else if ft == FieldType::Field && dt == DataType::Entrykey {
            // entrykey field element definition
            // =================================
            $writer->startTag("interleave");
            for field in dm.get_fields_of_type(ft, &[dt], None) {
              $writer->startTag("optional");
              // related field is special
              if ($field == "related") {
                $writer->startTag("element", "name" => format!("{bltx}:{field}"));
                $writer->startTag("element", "name" => format!("{bltx}:list"));
                $writer->startTag("oneOrMore");
                $writer->startTag("element", "name" => format!("{bltx}:item"));
                $writer->emptyTag("attribute", "name" => "type");
                $writer->emptyTag("attribute", "name" => "ids");
                $writer->startTag("optional");
                $writer->emptyTag("attribute", "name" => "string");
                $writer->endTag(); // optional
                $writer->startTag("optional");
                $writer->emptyTag("attribute", "name" => "options");
                $writer->endTag(); // optional
                $writer->endTag(); // item element
                $writer->endTag(); // oneOrMore
                $writer->endTag(); // list element
                $writer->endTag(); // $field element
              }
              else {
                $writer->startTag("element", "name" => format!("{bltx}:{field}"));
                $writer->startTag("choice");
                $writer->emptyTag("ref", "name" => "xdata");
                $writer->startTag("choice");
                $writer->startTag("list");
                $writer->startTag("oneOrMore");
                $writer->emptyTag("data", "type" => "string");
                $writer->endTag(); // oneOrMore
                $writer->endTag(); // list
                $writer->startTag("element", "name" => format!("{bltx}:list"));
                $writer->startTag("oneOrMore");
                $writer->startTag("element", "name" => format!("{bltx}:item"));
                $writer->emptyTag("text");// text
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
          else if ft == FieldType::Field && dt == DataType::Date {
            // date field element definition
            // Can't strongly type dates as we allow full ISO8601 meta characters
            // =============================
            let types = dm.get_fields_of_type(ft, &[dt], None).iter().map(|f|
              f.strip_suffix("date").unwrap_or(f)
            );
            $writer->startTag("zeroOrMore");
            $writer->startTag("element", "name" => format!("{bltx}:date"));
            $writer->startTag("optional");
            $writer->startTag("attribute", "name" => "type");
            $writer->startTag("choice");
            for datetype in types {
              if datetype.is_empty() {
                continue;
              }
              $writer->dataElement("value", datetype);
            }
            $writer->endTag(); // choice
            $writer->endTag(); // attribute
            $writer->endTag(); // optional
            $writer->startTag("choice");
            $writer->emptyTag("data", "type" => "string");
            $writer->startTag("group");
            $writer->startTag("element", "name" => format!("{bltx}:start"));
            $writer->startTag("choice");
            $writer->emptyTag("data", "type" => "string");
            $writer->endTag(); // choice
            $writer->endTag(); // start element
            $writer->startTag("element", "name" => format!("{bltx}:end"));
            $writer->startTag("choice");
            $writer->emptyTag("data", "type" => "string");
            $writer->emptyTag("empty");
            $writer->endTag(); // choice
            $writer->endTag(); // end element
            $writer->endTag(); // group
            $writer->endTag(); // choice
            $writer->endTag(); // $field element
            $writer->endTag(); // zeroOrMore
            // =============================
          }
          else if ft == FieldType::Field {
            // field element definition
            // ========================
            $writer->startTag("interleave");
            for field in dm.get_fields_of_type(ft, &[dt], None) {
              $writer->startTag("optional");
              $writer->startTag("element", "name" => format!("{bltx}:{field}"));
              $writer->startTag("choice");
              $writer->emptyTag("ref", "name" => "xdata");
              $writer->emptyTag("text");// text
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
    $writer->comment("xdata attribute definition");
    $writer->startTag("define", "name" => "xdata");
    $writer->startTag("optional");
    $writer->startTag("attribute", "name" => "xdata");
    $writer->emptyTag("text");// text
    $writer->endTag();// attribute
    $writer->endTag();// optional
    $writer->endTag();// define
    // ===========================

    // gender attribute definition
    // ===========================
    $writer->comment("gender attribute definition");
    $writer->startTag("define", "name" => "gender");
    $writer->startTag("optional");
    $writer->startTag("attribute", "name" => "gender");
    $writer->startTag("choice");
    for gender in ($dm->get_constant_value("gender")) {// list type so returns list
      $writer->dataElement("value", $gender);
    }
    $writer->endTag();// choice
    $writer->endTag();// attribute
    $writer->endTag();// optional
    $writer->endTag();// define
    // ===========================

    // generic meta annotation element definition
    // ===========================================
    $writer->comment("generic annotation element definition");
    $writer->startTag("define", "name" => "mannotation");
    $writer->startTag("zeroOrMore");
    $writer->startTag("element", "name" => format!("{bltx}:mannotation"));
    $writer->emptyTag("attribute", "name" => "field");
    $writer->startTag("optional");
    $writer->emptyTag("attribute", "name" => "name");
    $writer->endTag(); // optional
    $writer->startTag("optional");
    $writer->emptyTag("attribute", "name" => "item");
    $writer->endTag(); // optional
    $writer->startTag("optional");
    $writer->emptyTag("attribute", "name" => "part");
    $writer->endTag(); // optional
    $writer->startTag("optional");
    $writer->emptyTag("attribute", "name" => "literal");
    $writer->endTag(); // optional
    $writer->emptyTag("text");// text
    $writer->endTag(); // mannotation element
    $writer->endTag(); // zeroOrMore
    $writer->endTag();// define
    // ===========================

    $writer->endTag();// grammar
    $writer->end();
    $rng->close();
    // So we only do this one for potentially multiple .bltxml datasources
    dm.bltxml_schema_gen_done = true;
  }

  /// Generate a RelaxNG XML schema from the datamodel for bblXML output
  fn generate_bblxml_schema(dm: &DataModel, outfile: &Path) {
    let dmh = dm.helpers;

    // Set the .rng path to the output dir, if specified
    if (let $outdir = crate::Config->getoption("output_directory")) {
      let (_, _, file) = File::Spec->splitpath(outfile);
      $outfile = File::Spec->catfile(outdir, file)
    }
    let $rng = IO::File->new($outfile, '>:encoding(UTF-8)');
    $rng->autoflush;// Needed for running tests to string refs

    info!("Writing bblXML RNG schema '{}' for datamodel", outfile);
    require XML::Writer;
    let $bbl_ns = "https://sourceforge.net/projects/biblatex/bblxml";
    let $bbl = "bbl";
    let $default_ns = "http://relaxng.org/ns/structure/1.0";
    let $writer = new XML::Writer(NAMESPACES   => 1,
                                ENCODING     => "UTF-8",
                                DATA_MODE    => 1,
                                DATA_INDENT  => 2,
                                OUTPUT       => $rng,
                                PREFIX_MAP   => {$bbl_ns     => $bbl,
                                                  $default_ns => ""});

    $writer->xmlDecl();
    $writer->comment("Auto-generated from .bcf Datamodel");
    $writer->forceNSDecl($default_ns);
    $writer->forceNSDecl($bbl_ns);
    $writer->startTag("grammar",
                      "datatypeLibrary" => "http://www.w3.org/2001/XMLSchema-datatypes");
    $writer->startTag("start");
    $writer->startTag("element", "name" => format!("{bbl}:refsections"));
    $writer->startTag("oneOrMore");
    $writer->startTag("element", "name" => format!("{bbl}:refsection"));
    $writer->emptyTag("attribute", "name" => "id");
    $writer->startTag("oneOrMore");
    $writer->startTag("element", "name" => format!("{bbl}:datalist"));
    $writer->emptyTag("attribute", "name" => "id");
    $writer->startTag("attribute", "name" => "type");
    $writer->startTag("choice");
    $writer->dataElement("value", "entry");
    $writer->dataElement("value", "list");
    $writer->endTag();    // choice
    $writer->endTag();    // attribute
    $writer->startTag("oneOrMore");
    $writer->startTag("choice");
    // Set parent entries are special
    $writer->startTag("element", "name" => format!("{bbl}:entry"));
    $writer->emptyTag("attribute", "name" => "key");
    $writer->startTag("attribute", "name" => "type");
    $writer->startTag("choice");
    $writer->dataElement("value", "set");
    $writer->endTag();    // choice
    $writer->endTag();    // attribute
    $writer->startTag("element", "name" => format!("{bbl}:set"));
    $writer->startTag("oneOrMore");
    $writer->startTag("element", "name" => format!("{bbl}:member"));
    $writer->emptyTag("text");// text
    $writer->endTag();    // member
    $writer->endTag();    // oneOrMore
    $writer->endTag();    // set
    $writer->startTag("oneOrMore");
    $writer->startTag("element", "name" => format!("{bbl}:field"));
    $writer->startTag("attribute", "name" => "name");
    $writer->startTag("choice");
    $writer->dataElement("value", "labelprefix");
    $writer->dataElement("value", "labelalpha");
    $writer->dataElement("value", "extraalpha");
    $writer->dataElement("value", "annotation");
    $writer->dataElement("value", "sortinit");
    $writer->dataElement("value", "sortinithash");
    $writer->dataElement("value", "label");
    $writer->endTag();    // choice
    $writer->endTag();    // attribute
    $writer->emptyTag("text");// text
    $writer->endTag();    // field
    $writer->endTag();    // oneOrMore
    $writer->endTag(); // entry
    // Normal entries
    $writer->startTag("element", "name" => format!("{bbl}:entry"));
    $writer->emptyTag("attribute", "name" => "key");
    $writer->startTag("attribute", "name" => "type");
    $writer->startTag("choice");
    for et in ($dm->entrytypes->@*) {
      $writer->dataElement("value", $et);
    }
    $writer->endTag();    // choice
    $writer->endTag();    // attribute

    // source
    $writer->startTag("optional");
    $writer->startTag("attribute", "name" => "source");
    $writer->startTag("choice");
    $writer->dataElement("value", "crossref");
    $writer->dataElement("value", "xref");
    $writer->endTag();    // choice
    $writer->endTag();    // attribute
    $writer->endTag();    // optional

    // singletitle
    $writer->startTag("optional");
    $writer->startTag("attribute", "name" => "singletitle");
    $writer->startTag("choice");
    $writer->dataElement("value", "true");
    $writer->endTag();    // choice
    $writer->endTag();    // attribute
    $writer->endTag();    // optional

    // uniquetitle
    $writer->startTag("optional");
    $writer->startTag("attribute", "name" => "uniquetitle");
    $writer->startTag("choice");
    $writer->dataElement("value", "true");
    $writer->endTag();    // choice
    $writer->endTag();    // attribute
    $writer->endTag();    // optional

    // uniquework
    $writer->startTag("optional");
    $writer->startTag("attribute", "name" => "uniquework");
    $writer->startTag("choice");
    $writer->dataElement("value", "true");
    $writer->endTag();    // choice
    $writer->endTag();    // attribute
    $writer->endTag();    // optional

    // uniqueprimaryauthor
    $writer->startTag("optional");
    $writer->startTag("attribute", "name" => "uniqueprimaryauthor");
    $writer->startTag("choice");
    $writer->dataElement("value", "true");
    $writer->endTag();    // choice
    $writer->endTag();    // attribute
    $writer->endTag();    // optional

    $writer->startTag("interleave");

    $writer->startTag("zeroOrMore");
    $writer->startTag("element", "name" => format!("{bbl}:inset"));
    $writer->startTag("oneOrMore");
    $writer->startTag("element", "name" => format!("{bbl}:member"));
    $writer->emptyTag("text");// text
    $writer->endTag();    // member
    $writer->endTag();    // oneOrMore
    $writer->endTag();    // inset
    $writer->endTag();    // zeroOrMore

    // Per-entry options
    $writer->startTag("zeroOrMore");
    $writer->startTag("element", "name" => format!("{bbl}:options"));
    $writer->startTag("oneOrMore");
    $writer->startTag("element", "name" => format!("{bbl}:option"));
    $writer->emptyTag("text");// text
    $writer->endTag();    // option
    $writer->endTag();    // oneOrMore
    $writer->endTag();    // options
    $writer->endTag();    // zeroOrMore

    // names
    let names = dm.get_fields_of_type(FieldType::List, DataType::Name, None).filter(|f| !dm.field_is_skipout(f));

    $writer->startTag("oneOrMore");
    $writer->startTag("element", "name" => format!("{bbl}:names"));
    $writer->startTag("attribute", "name" => "type");
    $writer->startTag("choice");
    for name in names {
      $writer->dataElement("value", name);
    }
    $writer->endTag();    // choice
    $writer->endTag();    // attribute
    $writer->startTag("attribute", "name" => "count");
    $writer->emptyTag("data", "type" => "integer");
    $writer->endTag();    // attribute
    $writer->startTag("optional");
    $writer->startTag("attribute", "name" => "ul");
    $writer->emptyTag("data", "type" => "integer");
    $writer->endTag();    // attribute
    $writer->endTag();    // optional
    $writer->startTag("optional");
    $writer->startTag("attribute", "name" => "useprefix");
    $writer->emptyTag("data", "type" => "boolean");
    $writer->endTag();    // attribute
    $writer->endTag();    // optional
    $writer->startTag("optional");
    $writer->emptyTag("attribute", "name" => "sortingnamekeytemplatename");
    $writer->endTag();    // optional
    $writer->startTag("optional");
    $writer->startTag("attribute", "name" => "more");
    $writer->emptyTag("data", "type" => "boolean");
    $writer->endTag();    // attribute
    $writer->endTag();    // optional

    // name
    $writer->startTag("oneOrMore");
    $writer->startTag("element", "name" => format!("{bbl}:name"));
    $writer->startTag("optional");
    $writer->startTag("attribute", "name" => "useprefix");
    $writer->emptyTag("data", "type" => "boolean");
    $writer->endTag();    // attribute
    $writer->endTag();    // optional
    $writer->startTag("optional");
    $writer->emptyTag("attribute", "name" => "sortingnamekeytemplatename");
    $writer->endTag();    // optional
    $writer->emptyTag("attribute", "name" => "hash");
    $writer->startTag("optional");
    $writer->startTag("attribute", "name" => "un");
    $writer->emptyTag("data", "type" => "integer");
    $writer->endTag();    // attribute
    $writer->startTag("attribute", "name" => "uniquepart");
    $writer->emptyTag("data", "type" => "string");
    $writer->endTag();    // attribute
    $writer->endTag();    // optional
    $writer->startTag("oneOrMore");
    $writer->startTag("element", "name" => format!("{bbl}:namepart"));
    $writer->emptyTag("attribute", "name" => "type");
    $writer->emptyTag("attribute", "name" => "initials");
    $writer->startTag("optional");
    $writer->emptyTag("attribute", "name" => "un");
    $writer->endTag();    // optional
    $writer->emptyTag("text");// text
    $writer->endTag();// namepart
    $writer->endTag();// oneOrMore
    $writer->endTag();// name
    $writer->endTag();// oneOrMore
    $writer->endTag();// names
    $writer->endTag();// oneOrMore

    // lists
    // verbatim lists don't need special handling in XML, unlike TeX so they are here
    let @lists = grep {
      !$dm->field_is_datatype("name", $_)
          && !$dm->field_is_datatype("uri", $_)
            && !$dm->field_is_skipout($_)
          } $dm->get_fields_of_fieldtype("list")->@*;

    $writer->startTag("zeroOrMore");
    $writer->startTag("element", "name" => format!("{bbl}:list"));
    $writer->startTag("attribute", "name" => "name");
    $writer->startTag("choice");
    for list in (@lists) {
      $writer->dataElement("value", $list);
    }
    $writer->endTag();          // choice
    $writer->endTag();          // attribute
    $writer->startTag("attribute", "name" => "count");
    $writer->emptyTag("data", "type" => "integer");
    $writer->endTag();          // attribute
    $writer->startTag("optional");
    $writer->startTag("attribute", "name" => "more");
    $writer->emptyTag("data", "type" => "boolean");
    $writer->endTag();    // attribute
    $writer->endTag();    // optional
    $writer->startTag("oneOrMore");
    $writer->startTag("element", "name" => format!("{bbl}:item"));
    $writer->emptyTag("text");// text
    $writer->endTag();          // item
    $writer->endTag();          // oneOrMore
    $writer->endTag();          // list
    $writer->endTag();          // zeroOrMore

    // fields
    let fs1 = vec![
      "namehash",
      "bibnamehash",
      "fullhash",
      "labelalpha",
      "sortinit",
      "sortinithash",
      "sortinithash",
      "extraname",
      "extradate",
      "labelyear",
      "labelmonth",
      "labelday",
      "labeldatesource",
      "labelprefix",
      "extratitle",
      "extratitleyear",
      "extraalpha",
      "labelnamesource",
      "labeltitlesource",
      "clonesourcekey",
    ];

    // verbatim fields don't need special handling in XML, unlike TeX so they are here
    let fs2 = dm.get_fields_of_type(FieldType::Field,
      &[DataType::Entrykey,
      DataType::Key,
      DataType::Integer,
      DataType::Datepart,
      DataType::Literal,
      DataType::Code,
      DataType::Verbatim], None).filter(|f| !(dm.get_fieldformat(f) == Format::Xsv)
      && !dm.field_is_skipout(f));

    // uri fields
    let fs3 = dm.get_fields_of_type(FieldType::Field, &[DataType::Uri], None);

    // <namelist>namehash and <namelist>fullhash
    let fs4 = Vec::new();
    for n in &dmh.namelists {
      fs4.push(format!("{n}namehash"));
      fs4.push(format!("{n}bibnamehash"));
      fs4.push(format!("{n}fullhash"));
    }

    $writer->startTag("oneOrMore");
    $writer->startTag("element", "name" => format!("{bbl}:field"));
    $writer->startTag("choice"); // start choice of normal vs datepart fields
    $writer->startTag("group"); //
    $writer->startTag("attribute", "name" => "name");

    $writer->startTag("choice");
    for f in fs1.iter().extend(fs2).extend(fs3.iter()).extend(fs4.iter()) {
      $writer->dataElement("value", f);
    }
    $writer->endTag();    // choice
    $writer->endTag();    // attribute
    $writer->endTag();    // group
    $writer->startTag("group"); //
    $writer->startTag("attribute", "name" => "name");

    $writer->startTag("choice");
    for dp in dm.get_fields_of_type(FieldType::Field, &[DataType::Datepart], None) {
      $writer->dataElement("value", $dp);
    }
    $writer->endTag();    // choice
    $writer->endTag();    // attribute
    // dateparts may have an era attributes
    $writer->startTag("optional");
    $writer->startTag("attribute", "name" => "startera");
    $writer->startTag("choice");
    $writer->dataElement("value", "bce");
    $writer->dataElement("value", "ce");
    $writer->endTag();    // choice
    $writer->endTag();    // attribute
    $writer->endTag();    // optional
    $writer->startTag("optional");
    $writer->startTag("attribute", "name" => "endera");
    $writer->startTag("choice");
    $writer->dataElement("value", "bce");
    $writer->dataElement("value", "ce");
    $writer->endTag();    // choice
    $writer->endTag();    // attribute
    $writer->endTag();    // optional
    // dateparts may have a julian attributes
    $writer->startTag("optional");
    $writer->startTag("attribute", "name" => "startjulian");
    $writer->startTag("choice");
    $writer->dataElement("value", "true");
    $writer->endTag();    // choice
    $writer->endTag();    // attribute
    $writer->endTag();    // optional
    $writer->startTag("optional");
    $writer->startTag("attribute", "name" => "endjulian");
    $writer->startTag("choice");
    $writer->dataElement("value", "true");
    $writer->endTag();    // choice
    $writer->endTag();    // attribute
    $writer->endTag();    // optional
    // dateparts may have a approximate attributes
    $writer->startTag("optional");
    $writer->startTag("attribute", "name" => "startapproximate");
    $writer->startTag("choice");
    $writer->dataElement("value", "true");
    $writer->endTag();    // choice
    $writer->endTag();    // attribute
    $writer->endTag();    // optional
    $writer->startTag("optional");
    $writer->startTag("attribute", "name" => "endapproximate");
    $writer->startTag("choice");
    $writer->dataElement("value", "true");
    $writer->endTag();    // choice
    $writer->endTag();    // attribute
    $writer->endTag();    // optional
    // dateparts may have an uncertain attributes
    $writer->startTag("optional");
    $writer->startTag("attribute", "name" => "startuncertain");
    $writer->startTag("choice");
    $writer->dataElement("value", "true");
    $writer->endTag();    // choice
    $writer->endTag();    // attribute
    $writer->endTag();    // optional
    $writer->startTag("optional");
    $writer->startTag("attribute", "name" => "enduncertain");
    $writer->startTag("choice");
    $writer->dataElement("value", "true");
    $writer->endTag();    // choice
    $writer->endTag();    // attribute
    $writer->endTag();    // optional
    // dateparts may have an unknown attributes
    $writer->startTag("optional");
    $writer->startTag("attribute", "name" => "startunknown");
    $writer->startTag("choice");
    $writer->dataElement("value", "true");
    $writer->endTag();    // choice
    $writer->endTag();    // attribute
    $writer->endTag();    // optional
    $writer->startTag("optional");
    $writer->startTag("attribute", "name" => "endunknown");
    $writer->startTag("choice");
    $writer->dataElement("value", "true");
    $writer->endTag();    // choice
    $writer->endTag();    // attribute
    $writer->endTag();    // optional
    $writer->endTag();    // group
    $writer->endTag();    // choice (normal vs datepart)
    $writer->emptyTag("text");// text
    $writer->endTag();    // field
    $writer->endTag();    // oneOrMore

    // ranges
    let ranges = dm.get_fields_of_datatype(&[DataType::Range]).iter().filter(|f| !dm.field_is_skipout(f));

    $writer->startTag("zeroOrMore");
    $writer->startTag("element", "name" => format!("{bbl}:range"));
    $writer->startTag("attribute", "name" => "name");
    $writer->startTag("choice");
    for r in ranges {
      $writer->dataElement("value", $r);
    }
    $writer->endTag();    // choice
    $writer->endTag();    // attribute
    $writer->startTag("oneOrMore");
    $writer->startTag("element", "name" => format!("{bbl}:item"));
    $writer->startTag("attribute", "name" => "length");
    $writer->emptyTag("data", "type" => "integer");
    $writer->endTag();    // attribute
    $writer->startTag("element", "name" => format!("{bbl}:start"));
    $writer->emptyTag("text");// text
    $writer->endTag();    // start
    $writer->startTag("optional");
    $writer->startTag("element", "name" => format!("{bbl}:end"));
    $writer->emptyTag("text");// text
    $writer->endTag();    // end
    $writer->endTag();    // optional
    $writer->endTag();    // item
    $writer->endTag();    // oneOrMore
    $writer->endTag();    // range
    $writer->endTag();    // zeroOrMore

    // uri lists - not in default data model
    {
      let uril = dm.get_fields_of_type(FieldType::List, &[DataType::Uri], None);
      if !uril.is_empty() {
        $writer->startTag("optional");
        $writer->startTag("element", "name" => format!("{bbl}:list"));
        $writer->startTag("attribute", "name" => "name");
        $writer->startTag("choice");
        for u in uril.into_iter() {
          $writer->dataElement("value", $u);
        }
        $writer->endTag();          // choice
        $writer->endTag();          // attribute
        $writer->startTag("attribute", "name" => "count");
        $writer->emptyTag("data", "type" => "integer");
        $writer->endTag();          // attribute
        $writer->startTag("oneOrMore");
        $writer->startTag("element", "name" => format!("{bbl}:item"));
        $writer->emptyTag("data", "type" => "anyURI");
        $writer->endTag();          // item
        $writer->endTag();          // oneOrMore
        $writer->endTag();          // list element
        $writer->endTag();          // optional
      }
    }

    // nocite
    $writer->startTag("optional");
    $writer->startTag("element", "name" => format!("{bbl}:nocite"));
    $writer->emptyTag("empty");
    $writer->endTag();// nocite
    $writer->endTag();// optional

    // keywords
    $writer->startTag("optional");
    $writer->startTag("element", "name" => format!("{bbl}:keywords"));
    $writer->startTag("oneOrMore");
    $writer->startTag("element", "name" => format!("{bbl}:keyword"));
    $writer->emptyTag("data", "type" => "string");
    $writer->endTag();// item
    $writer->endTag();// oneOrMore
    $writer->endTag();// keywords
    $writer->endTag();// optional

    // annotations
    $writer->startTag("zeroOrMore");
    $writer->startTag("element", "name" => format!("{bbl}:annotation"));
    $writer->startTag("attribute", "name" => "scope");
    $writer->startTag("choice");
    for s in ("field", "list", "names", "item", "name", "namepart") {
      $writer->dataElement("value", $s);
    }
    $writer->endTag();// choice
    $writer->endTag();// scope attribute
    $writer->emptyTag("attribute", "name" => "field");
    $writer->emptyTag("attribute", "name" => "name");
    $writer->emptyTag("attribute", "name" => "value");
    $writer->startTag("attribute", "name" => "literal");
    $writer->startTag("choice");
    for s in ["1", "0"] {
      $writer->dataElement("value", $s);
    }
    $writer->endTag();// choice
    $writer->endTag();// literal attribute
    $writer->startTag("optional");
    $writer->emptyTag("attribute", "name" => "item");
    $writer->endTag();// optional
    $writer->startTag("optional");
    $writer->emptyTag("attribute", "name" => "part");
    $writer->endTag();// optional
    $writer->endTag();// annotation
    $writer->endTag();// zeroOrMore

    // warnings
    $writer->startTag("zeroOrMore");
    $writer->startTag("element", "name" => format!("{bbl}:warning"));
    $writer->emptyTag("data", "type" => "string");
    $writer->endTag();// warning
    $writer->endTag();// zeroOrMore

    $writer->endTag();// interleave element
    $writer->endTag();// entry element
    $writer->endTag();// choice
    $writer->endTag();// oneOrMore
    $writer->endTag();// datalist element
    $writer->endTag();// oneOrMore

    // aliases
    $writer->startTag("zeroOrMore");
    $writer->startTag("element", "name" => format!("{bbl}:keyalias"));
    $writer->emptyTag("attribute", "name" => "key");
    $writer->emptyTag("text");// text
    $writer->endTag();// keyalias
    $writer->endTag();// zeroOrMore

    // missing keys
    $writer->startTag("zeroOrMore");
    $writer->startTag("element", "name" => format!("{bbl}:missing"));
    $writer->emptyTag("text");// text
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
}