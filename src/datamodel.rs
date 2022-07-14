//! `DataModel` objects

/*
no autovivification;

use List::Util qw( first );
use List::AllUtils qw( firstidx );
use crate::Config;
use Scalar::Util qw (blessed looks_like_number);
use Unicode::UCD qw(num);
*/

#[derive(Debug)]
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
      if f.nullok {
        $self->{fieldsbyname}{$f->{content}}.nullok = true;
      }
      // check skips - fields we don't want to output to .bbl
      if ($f->{skip_output}) {
        $self->{fieldsbyname}{$f->{content}}.skipout = true;
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
        $self->{entrytypesbyname}->{$es}.skipout = true;
      }
      // fields for entrytypes
      for ef in ($dm->{entryfields}->@*) {
        // Found a section describing legal fields for entrytype
        if (!exists($ef->{entrytype}) ||
            grep {$_->{content} == $es} $ef->{entrytype}->@*) {
          for f in ($ef->{field}->@*) {
            self.entrytypesbyname{$es}.legal_fields.insert(&f.content);
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
              let man = &mut self->{entrytypesbyname}{$es}{constraints}{mandatory};
              for f in ($c->{field}->@*) {
                man.push(f->{content});
              }
              // xor set of fields
              // [ XOR, field1, field2, ... , fieldn ]
              for fxor in ($c->{fieldxor}->@*) {
                let mut xorset = Vec::new();
                for f in ($fxor->{field}->@*) {
                  xorset.push(f->{content});
                }
                xorset.insert(0, "XOR");
                man.push(xorset);
              }
              // or set of fields
              // [ OR, field1, field2, ... , fieldn ]
              for f_or in ($c->{fieldor}->@*) {
                let mut orset = Vec::new();
                for f in ($f_or->{field}->@*) {
                  orset.push(f->{content});
                }
                orset.insert(0, "OR");
                man.push(orset);
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
      if ["citeorder", "citecount"].into_iter().chain(self.helpers{integers}.iter()).any(|k| f == k) {
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
  fn get_outcase(&self, $string) {
    return $self->{casemap}{foldtoorig}{$string};
  }

  /// Returns array ref of constant names
  fn constants(&self) {
    return [ keys $self->{constants}->%* ];
  }

  /// Returns a constant type
  fn get_constant_type(&self, $name) {
    return $self->{constants}{$name}{type};
  }

  /// Returns a constant value
  fn get_constant_value(&self, name: &str) {
    if ($self->{constants}{$name}{type} == "list") {
      return split(/\s*,\s*/, $self->{constants}{$name}{value});
    }
    else if ($self->{constants}{$name}{type} == "string") {
      return $self->{constants}{$name}{value};
    }
  }

  /// Returns boolean to say if a field is a multiscript field
  fn is_multiscript(&self, field: &str) -> bool {
    self.multiscriptfields.contains(field)
  }

  /// Returns array ref of legal fieldtypes
  fn fieldtypes(&self) {
    return [ keys $self->{fieldsbyfieldtype}->%* ];
  }

  /// Returns array ref of legal datatypes
  fn datatypes(&self) {
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
  fn entrytypes(&self) -> impl Iterator<Item=&String> {
    return [ keys $self->{entrytypesbyname}->%* ];
  }

  /// Returns boolean to say if an entrytype is a legal entrytype
  fn is_entrytype(&self, type) -> bool {
    return $self->{entrytypesbyname}{$type} ? 1 : 0;
  }

  /// Returns boolean to say if a field is legal for an entrytype
  fn is_field_for_entrytype(&self, type, field: &str) -> bool {
    self.entrytypesbyname{$type}.legal_fields.contains(field) // NOTE: legal_fields: HashSet
  }

  /// Returns boolean depending on whether an entrytype is to be skipped on output
  fn entrytype_is_skipout(&self, $type) -> bool {
    return $self->{entrytypesbyname}{$type}.skipout.unwrap_or(false);
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
  fn get_fields_of_type(&self, fieldtype: FieldType, datatype: &[DataType], format: Option<Format>) -> Vec<Unknown> {
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
  fn get_fieldformat(&self, $field) {
    return $self->{fieldsbyname}{$field}{format};
  }

  /// Returns the fieldtype, datatype and format of a field
  fn get_dm_for_field(&self, field: &str) {
    return {"fieldtype" =>  self.fieldsbyname{$field}{fieldtype},
            "datatype"  => self.fieldsbyname{$field}{datatype},
            "format"    => self.fieldsbyname{$field}{format}};
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
  fn field_is_nullok(&self, field: &str) -> bool {
    return $self->{fieldsbyname}{$field}.nullok.unwrap_or(false); // TODO: false by default
  }

  /// Returns boolean depending on whether a field is to be skipped on output
  fn field_is_skipout(&self, field: &str) -> bool {
    return $self->{fieldsbyname}{$field}.skipout.unwrap_or(false); // TODO: false by default
  }

  /// Checks constraints of type "mandatory" on entry and
  /// returns an arry of warnings, if any
  fn check_mandatory_constraints(&self, be: &mut Entry) -> Vec<String> {
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
            warnings.push(format!("Datamodel: Entry '{key}' ({ds}): Missing mandatory field - one of '{}' must be defined", fs.join(", ")));
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
  fn check_conditional_constraints(&self, be: &mut Entry) -> Vec<String> {
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
        if afs.len() != actual_afs.len() { // ALL -> ? not satisfied
          continue;
        }
      }
      else if aq == "none" {
        if actual_afs.len() == 0 {      // NONE -> ? not satisfied
          continue;
        }
      }
      else if aq == "one" {
        if actual_afs.len() != 0 {  // ONE -> ? not satisfied
          continue;
        }
      }

      // check consequent
      let actual_cfs = cfs.iter().filter(|cf| be.field_exists(cf)).collect();
      if cq == "all" {
        if cfs.len() != actual_cfs.len() { // ? -> ALL not satisfied
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
  fn check_data_constraints(&self, be: &mut Entry) -> Vec<String> {
    let secnum = crate::MASTER.get_current_section();
    let section = crate::MASTER.sections().get_section(secnum);
    let warnings = Vec::new();
    let et = be.get_field("entrytype");
    let key = be.get_field("citekey");
    let ds = section.get_keytods(key);

    for c in ($self->{entrytypesbyname}{$et}{constraints}{data}->@*) {
      // This is the datatype of the constraint, not the field!
      match c.datatype {
        DataType::Isbn => {
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
        DataType::Issn => {
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
        DataType::Ismn => {
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
        DataType::Integer | DataType::Datepart => {
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
        DataType::Pattern => {
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
        _ => {}
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

  /// Generate a RelaxNG XML schema from the datamodel for BibLaTeXML datasources
  fn generate_bltxml_schema(&self, outfile: &Path) {
    if self.bltxml_schema_gen_done {
      return;
    }

    // Set the .rng path to the output dir, if specified
    if (let $outdir = crate::Config->getoption("output_directory")) {
      let file = outfile.file_name();
      outfile = outdir.join(file);
    }

    info!("Writing BibLaTeXML RNG schema '{}' for datamodel", outfile);

    let bltx_ns = "http://biblatex-biber.sourceforge.net/biblatexml";
    let bltx = "bltx";
    let default_ns = "http://relaxng.org/ns/structure/1.0";

    #[rustfmt::skip]
    let mut grammar_tag = Element::builder("grammar").attr("datatypeLibrary", "http://www.w3.org/2001/XMLSchema-datatypes")
      .append(Element::builder("start")
        .append(Element::builder("element").attr("name", format!("{bltx}:entries"))
          .append(Element::builder("attribute").attr("name", "id"))
          .append(Element::builder("attribute").attr("name", "entrytype")
            .append(Element::builder("choice")
              .append_all(self.entrytypes()
                .map(|entrytype| Element::builder("value").text(entrytype))
              )
            )
          )

          .append({
            let mut builder = Element::builder("interleave");
    
            for ft in &self.fieldtypes {
              for dt in &self.datatypes {
                if self.is_fields_of_type(ft, dt, None) {
                  if dt == DataType::Datepart { // not legal in input, only output
                    continue;
                  }
                  builder = builder.comment(format!("{dt} {ft}s"));
                  builder = builder.append(Element::builder("ref").attr("name", format!("{dt}{ft}")));
                }
              }
            }
    
            // Annotations
            builder.append(Element::builder("ref").attr("name", "mannotation"))
          })
        )
      );

    for ft in &self.fieldtypes {
      for dt in &self.datatypes {
        if self.is_fields_of_type(ft, dt, None) {
          if dt == DataType::Datepart { // not legal in input, only output
            continue;
          }
          grammar_tag = grammar_tag.comment(format!("{dt} {ft}s definition"));
          grammar_tag = grammar_tag.append(
            Element::builder("define").attr("name", format!("{dt}{ft}")).append(
          // Name lists element definition
          // =============================

          #[rustfmt::skip]
          match (ft, dt) {
            (FieldType::List, DataType::Name) => {
              Element::builder("zeroOrMore")// for example, XDATA doesn't need a name
                .append(Element::builder("element")\.attr("name", format!("{bltx}:names"))
                  .append(Element::builder("choice")
                    // xdata attribute ref
                    .append(Element::builder("ref").attr("name", "xdata"))
                    .append(Element::builder("group")
                      // useprefix attribute
                      .comment("useprefix option")
                      .append(Element::builder("optional")
                        .append(Element::builder("attribute").attr("name", "useprefix")
                          .append(Element::builder("data").attr("type", "boolean"))
                        )
                      )

                      // sortingnamekeytemplatename attribute
                      .comment("sortingnamekeytemplatename option")
                      .append(Element::builder("optional")
                        .append(Element::builder("attribute").attr("name", "sortingnamekeytemplatename")
                          .append(Element::builder("data").attr("type", "string"))
                        )
                      )

                      // type attribute
                      .comment("types of names elements")
                      .append(Element::builder("attribute").attr("name", "type")
                        .append(Element::builder("choice")
                          .append_all(self.get_fields_of_type(ft, &[dt], None).iter()
                            .map(|name| Element::builder("value").text(name))
                          )
                        )
                      )

                      // morenames attribute
                      .append(Element::builder("optional")
                        .append(Element::builder("attribute").attr("name", "morenames")
                          .append(Element::builder("data").attr("type", "boolean"))
                        )
                      )

                      .append(Element::builder("oneOrMore")
                        // Individual name element
                        .append(Element::builder("element").attr("name", format!("{bltx}:name"))
                          .append(Element::builder("choice")
                            // xdata attribute ref
                            .append(Element::builder("ref").attr("name", "xdata"))
                            .append(Element::builder("group")
                              // useprefix attribute
                              .comment("useprefix option")
                              .append(Element::builder("optional")
                                .append(Element::builder("attribute").attr("name", "useprefix")
                                  .append(Element::builder("data").attr("type", "boolean"))
                                )
                              )

                              // sortingnamekeytemplatename attribute
                              .comment("sortingnamekeytemplatename option")
                              .append(Element::builder("optional")
                                .append(Element::builder("attribute").attr("name", "sortingnamekeytemplatename")
                                  .append(Element::builder("data").attr("type", "string"))
                                )
                              )

                              // gender attribute ref
                              .append(Element::builder("ref").attr("name", "gender"))

                              // namepart element
                              .append(Element::builder("oneOrMore")
                                .append(Element::builder("element").attr("name", format!("{bltx}:namepart"))
                                  .append(Element::builder("attribute").attr("name", "type")
                                    .append(Element::builder("choice")
                                      // list type so returns list
                                      .append_all(self.get_constant_value("nameparts").iter()
                                        .map(|np| Element::builder("value").text(np))
                                      )
                                    )
                                  )
                                  .append(Element::builder("optional")
                                    .append(Element::builder("attribute").attr("name", "initial"))
                                  )
                                  .append(Element::builder("choice")
                                    .append(Element::builder("oneOrMore")
                                      .append(Element::builder("element").attr("name", format!("{bltx}:namepart"))
                                        .append(Element::builder("optional")
                                          .append(Element::builder("attribute").attr("name", "initial"))
                                        )
                                        .append(Element::builder("text"))
                                      )
                                    )
                                    .append(Element::builder("text"))
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              // ========================
            }
            (FieldType::List, _) => {
              // lists element definition
              // ========================
              Element::builder("interleave")
                .append_all(self.get_fields_of_type(ft, &[dt], None).iter().map(|list| {
                  Element::builder("optional")
                    .append(Element::builder("element").attr("name", format!("{bltx}:{list}"))
                      .append(Element::builder("choice")
                        .append(Element::builder("ref").attr("name", "xdata"))
                        .append(Element::builder("choice")
                          .append(Element::builder("text"))
                          .append(Element::builder("element").attr("name", format!("{bltx}:list"))
                            .append(Element::builder("oneOrMore")
                              .append(Element::builder("element").attr("name", format!("{bltx}:item"))
                                .append(Element::builder("choice")
                                  .append(Element::builder("ref").attr("name", "xdata"))
                                  .append(Element::builder("text")),
                                ),
                              ),
                            ),
                          ),
                        ),
                      ),
                  )
                }))
              // ========================
            }
            (FieldType::Field, DataType::Uri) => {
              // uri field element definition
              // ============================
              Element::builder("interleave")
              .append_all(self.get_fields_of_type(ft, &[dt], None).iter().map(|field| {
                Element::builder("optional")
                  .append(Element::builder("element").attr("name", format!("{bltx}:{field}"))
                    .append(Element::builder("choice")
                      .append(Element::builder("ref").attr("name", "xdata"))
                      .append(Element::builder("data").attr("type", "anyURI"))
                    ),
                )
              }))
              // ============================
            }
            (FieldType::Field, DataType::Range) => {
              // range field element definition
              // ==============================
              Element::builder("interleave")
              .append_all(self.get_fields_of_type(ft, &[dt], None).iter().map(|field| {
                Element::builder("optional")
                  .append(Element::builder("element").attr("name", format!("{bltx}:{field}"))
                    .append(Element::builder("choice")
                      // xdata attribute ref
                      .append(Element::builder("ref").attr("name", "xdata"))
                      .append(Element::builder("element").attr("name", format!("{bltx}:list"))
                        .append(Element::builder("oneOrMore")
                          .append(Element::builder("element").attr("name", format!("{bltx}:item"))
                            .append(Element::builder("choice")
                              // xdata attribute ref
                              .append(Element::builder("ref").attr("name", "xdata"))
                              .append(Element::builder("group")
                                .append(Element::builder("element").attr("name", format!("{bltx}:start"))
                                  .append(Element::builder("text"))
                                )
                                .append(Element::builder("element").attr("name", format!("{bltx}:end"))
                                  .append(Element::builder("choice")
                                    .append(Element::builder("text"))
                                    .append(Element::builder("empty"))
                                  )
                                )
                              ),
                            ),
                          ),
                        ),
                      ),
                    ),
                )
              }))
              // ==============================
            }
            (FieldType::Field, DataType::Entrykey) => {
              // entrykey field element definition
              // =================================
              Element::builder("interleave")
              .append_all(self.get_fields_of_type(ft, &[dt], None).iter().map(|field| {
                Element::builder("optional").append(
                  if field == "related" {
                    Element::builder("element").attr("name", format!("{bltx}:{field}"))
                      .append(Element::builder("element").attr("name", format!("{bltx}:list"))
                        .append(Element::builder("oneOrMore")
                          .append(Element::builder("element").attr("name", format!("{bltx}:item"))
                            .append(Element::builder("attribute").attr("name", "type"))
                            .append(Element::builder("attribute").attr("name", "ids"))
                            .append(Element::builder("optional")
                              .append(Element::builder("attribute").attr("name", "string"))
                            )
                            .append(Element::builder("optional")
                              .append(Element::builder("attribute").attr("name", "options"))
                            )
                          )
                        )
                      )
                  } else {
                    Element::builder("element").attr("name", format!("{bltx}:{field}"))
                      .append(Element::builder("choice")
                        .append(Element::builder("ref").attr("name", "xdata"))
                        .append(Element::builder("choice")
                          .append(Element::builder("list")
                            .append(Element::builder("oneOrMore")
                              .append(Element::builder("data").attr("type", "string"))
                            )
                          )
                          .append(Element::builder("element").attr("name", format!("{bltx}:list"))
                            .append(Element::builder("oneOrMore")
                              .append(Element::builder("element").attr("name", format!("{bltx}:item"))
                                .append(Element::builder("text"))
                              ),
                            ),
                          ),
                        )
                      )
                  }
                )
              }))
            }
            (FieldType::Field, DataType::Date) => {
              // date field element definition
              // Can't strongly type dates as we allow full ISO8601 meta characters
              // =============================
              let types = self.get_fields_of_type(ft, &[dt], None).iter().map(|f|
                f.strip_suffix("date").unwrap_or(f)
              );
              Element::builder("zeroOrMore")
                .append(Element::builder("element").attr("name", format!("{bltx}:date"))
                  .append(Element::builder("optional")
                    .append(Element::builder("attribute").attr("name", "type")
                      .append(Element::builder("choice")
                        .append_all(types
                          .filter(|datetype| !datetype.is_empty())
                          .map(|datetype| Element::builder("value").text(datetype))
                        )
                      )
                    )
                  )
                  .append(Element::builder("choice")
                    .append(Element::builder("data").attr("type", "string")
                    )
                    .append(Element::builder("group")
                      .append(Element::builder("element").attr("name", format!("{bltx}:start"))
                        .append(Element::builder("choice")
                          .append(Element::builder("data").attr("type", "string"))
                        )
                      )
                      .append(Element::builder("element").attr("name", format!("{bltx}:end"))
                        .append(Element::builder("choice")
                          .append(Element::builder("data").attr("type", "string"))
                          .append(Element::builder("empty"))
                        )
                      )
                    )
                  )
                )
              // =============================
            }
            (FieldType::Field, _) => {
              // field element definition
              // ========================
              Element::builder("interleave")
                .append_all(self.get_fields_of_type(ft, &[dt], None).iter().map(|field| {
                  Element::builder("optional")
                    .append(Element::builder("element").attr("name", format!("{bltx}:{field}"))
                      .append(Element::builder("choice")
                        .append(Element::builder("ref").attr("name", "xdata"))
                        .append(Element::builder("text"))
                      ),
                    )
                }))
              // ========================
            }
          }))
        }
      }
    }

    // xdata attribute definition
    // ===========================
    #[rustfmt::skip]
    grammar_tag = grammar_tag
      .comment("xdata attribute definition")
      .append(Element::builder("define").attr("name", "xdata")
        .append(Element::builder("optional")
          .append(Element::builder("attribute").attr("name", "xdata")
            .append(Element::builder("text"))
          )
        )
      )
    // ===========================

    // gender attribute definition
    // ===========================
      .comment("gender attribute definition")
      .append(Element::builder("define").attr("name", "gender")
        .append(Element::builder("optional")
          .append(Element::builder("attribute").attr("name", "gender")
            .append(Element::builder("choice")
              .append_all(self.get_constant_value("gender").iter()
                // list type so returns list
                .map(|gender| Element::builder("value").text(gender))
              )
            )
          )
        )
      )
    // ===========================

    // generic meta annotation element definition
    // ===========================================
      .comment("generic annotation element definition")
      .append(Element::builder("define").attr("name", "mannotation")
        .append(Element::builder("zeroOrMore")
          .append(Element::builder("element").attr("name", format!("{bltx}:mannotation"))
            .append(Element::builder("attribute").attr("name", "field"))
            .append(Element::builder("optional")
              .append(Element::builder("attribute").attr("name", "name"))
            )
            .append(Element::builder("optional")
              .append(Element::builder("attribute").attr("name", "item"))
            )
            .append(Element::builder("optional")
              .append(Element::builder("attribute").attr("name", "part"))
            )
            .append(Element::builder("optional")
              .append(Element::builder("attribute").attr("name", "literal"))
            )
            .append(Element::builder("text"))
          )
        )
      );
      // ===========================

    let grammar_tag = grammar_tag.build();

    let mut ns = Namespace::empty();
    ns.force_put(bltx, bltx_ns);
    ns.force_put("", default_ns);
    grammar_tag.namespaces = Some(ns);

    let mut rng = File::create(outfile).unwrap;
    write!(&mut rng, r##"<?xml version="1.0" encoding="UTF-8"?>
<!-- Auto-generated from .bcf Datamodel -->

"##);
    let mut cfg = EmitterConfig::new();
    cfg.perform_indent = true;
    cfg.write_document_declaration = false;
    grammar_tag.write_with_config(&mut rng, cfg).unwrap();

    // So we only do this one for potentially multiple .bltxml datasources
    self.bltxml_schema_gen_done = true;
  }

  /// Generate a RelaxNG XML schema from the datamodel for bblXML output
  fn generate_bblxml_schema(&self, outfile: &Path) {
    let dmh = &self.helpers;

    // Set the .rng path to the output dir, if specified
    if (let $outdir = crate::Config->getoption("output_directory")) {
      let file = outfile.file_name();
      outfile = outdir.join(file);
    }

    info!("Writing bblXML RNG schema '{}' for datamodel", outfile);

    let bbl_ns = "https://sourceforge.net/projects/biblatex/bblxml";
    let bbl = "bbl";
    let default_ns = "http://relaxng.org/ns/structure/1.0";

    #[rustfmt::skip]
    let grammar_tag = Element::builder("grammar").attr("datatypeLibrary", "http://www.w3.org/2001/XMLSchema-datatypes"))
      .append(Element::builder("start")
        .append(Element::builder("element").attr("name", format!("{bbl}:refsections"))
          .append(Element::builder("oneOrMore")
            .append(Element::builder("element").attr("name", format!("{bbl}:refsection"))
              .append(Element::builder("attribute").attr("name", "id"))
              .append(Element::builder("oneOrMore")
                .append(Element::builder("element").attr("name", format!("{bbl}:datalist"))
                  .append(Element::builder("attribute").attr("name", "id"))
                  .append(Element::builder("attribute").attr("name", "type")
                    .append(Element::builder("choice")
                      .append(Element::builder("value").text("entry"))
                      .append(Element::builder("value").text("list"))
                    )
                  )
                  .append(Element::builder("oneOrMore")
                    .append(Element::builder("choice")
                      // Set parent entries are special
                      .append(Element::builder("element").attr("name", format!("{bbl}:entry"))
                        .append(Element::builder("attribute").attr("name", "key"))
                        .append(Element::builder("attribute").attr("name", "type")
                          .append(Element::builder("choice")
                            .append(Element::builder("value").text("set"))
                          )
                        )
                        .append(Element::builder("element").attr("name", format!("{bbl}:set"))
                          .append(Element::builder("oneOrMore")
                            .append(Element::builder("element").attr("name", format!("{bbl}:member"))
                              .append(Element::builder("text"))
                            )
                          )
                        )
                        .append(Element::builder("oneOrMore")
                          .append(Element::builder("element").attr("name", format!("{bbl}:field"))
                            .append(Element::builder("attribute").attr("name", "name")
                              .append(Element::builder("choice")
                                .append(Element::builder("value").text("labelprefix"))
                                .append(Element::builder("value").text("labelalpha"))
                                .append(Element::builder("value").text("extraalpha"))
                                .append(Element::builder("value").text("annotation"))
                                .append(Element::builder("value").text("sortinit"))
                                .append(Element::builder("value").text("sortinithash"))
                                .append(Element::builder("value").text("label"))
                              )
                            )
                            .append(Element::builder("text"))
                          )
                        )
                      )
                      // Normal entries
                      .append(Element::builder("element").attr("name", format!("{bbl}:entry"))
                        .append(Element::builder("attribute").attr("name", "key"))
                        .append(Element::builder("attribute").attr("name", "type")
                          .append(Element::builder("choice")
                            .append_all(self.entrytypes()
                              .map(|et| Element::builder("value").text(et))
                            )
                          )
                        )

                        // source
                        .append(Element::builder("optional")
                          .append(Element::builder("attribute").attr("name", "source")
                            .append(Element::builder("choice")
                              .append(Element::builder("value").text("crossref"))
                              .append(Element::builder("value").text("xref"))
                            )
                          )
                        )

                        // singletitle
                        .append(Element::builder("optional")
                          .append(Element::builder("attribute").attr("name", "singletitle")
                            .append(Element::builder("choice")
                              .append(Element::builder("value").text("true"))
                            )
                          )
                        )

                        // uniquetitle
                        .append(Element::builder("optional")
                          .append(Element::builder("attribute").attr("name", "uniquetitle")
                            .append(Element::builder("choice")
                              .append(Element::builder("value").text("true"))
                            )
                          )
                        )

                        // uniquework
                        .append(Element::builder("optional")
                          .append(Element::builder("attribute").attr("name", "uniquework")
                            .append(Element::builder("choice")
                              .append(Element::builder("value").text("true"))
                            )
                          )
                        )

                        // uniqueprimaryauthor
                        .append(Element::builder("optional")
                          .append(Element::builder("attribute").attr("name", "uniqueprimaryauthor")
                            .append(Element::builder("choice")
                              .append(Element::builder("value").text("true"))
                            )
                          )
                        )

                        .append(Element::builder("interleave")
                          .append(Element::builder("zeroOrMore")
                            .append(Element::builder("element").attr("name", format!("{bbl}:inset"))
                              .append(Element::builder("oneOrMore")
                                .append(Element::builder("element").attr("name", format!("{bbl}:member"))
                                  .append(Element::builder("text"))
                                )
                              )
                            )
                          )

                          // Per-entry options
                          .append(Element::builder("zeroOrMore")
                            .append(Element::builder("element").attr("name", format!("{bbl}:options"))
                              .append(Element::builder("oneOrMore")
                                .append(Element::builder("element").attr("name", format!("{bbl}:option"))
                                  .append(Element::builder("text"))
                                )
                              )
                            )
                          )

                          // names
                          .append(Element::builder("oneOrMore")
                            .append(Element::builder("element").attr("name", format!("{bbl}:names"))
                              .append(Element::builder("attribute").attr("name", "type")
                                .append(Element::builder("choice")
                                  .append_all(self.get_fields_of_type(FieldType::List, DataType::Name, None)
                                    .filter(|f| !self.field_is_skipout(f))
                                    .map(|name| Element::builder("value").text(name))
                                  )
                                )
                              )
                              .append(Element::builder("attribute").attr("name", "count")
                                .append(Element::builder("data").attr("type", "integer"))
                              )
                              .append(Element::builder("optional")
                                .append(Element::builder("attribute").attr("name", "ul")
                                  .append(Element::builder("data").attr("type", "integer"))
                                )
                              )
                              .append(Element::builder("optional")
                                .append(Element::builder("attribute").attr("name", "useprefix")
                                  .append(Element::builder("data").attr("type", "boolean"))
                                )
                              )
                              .append(Element::builder("optional")
                                .append(Element::builder("attribute").attr("name", "sortingnamekeytemplatename"))
                              )
                              .append(Element::builder("optional")
                                .append(Element::builder("attribute").attr("name", "more")
                                  .append(Element::builder("data").attr("type", "boolean"))
                                )
                              )

                              // name
                              .append(Element::builder("oneOrMore")
                                .append(Element::builder("element").attr("name", format!("{bbl}:name"))
                                  .append(Element::builder("optional")
                                    .append(Element::builder("attribute").attr("name", "useprefix")
                                      .append(Element::builder("data").attr("type", "boolean"))
                                    )
                                  )
                                  .append(Element::builder("optional")
                                    .append(Element::builder("attribute").attr("name", "sortingnamekeytemplatename"))
                                  )
                                  .append(Element::builder("attribute").attr("name", "hash"))
                                  .append(Element::builder("optional")
                                    .append(Element::builder("attribute").attr("name", "un")
                                      .append(Element::builder("data").attr("type", "integer"))
                                    )
                                    .append(Element::builder("attribute").attr("name", "uniquepart")
                                      .append(Element::builder("data").attr("type", "string"))
                                    )
                                  )
                                  .append(Element::builder("oneOrMore")
                                    .append(Element::builder("element").attr("name", format!("{bbl}:namepart"))
                                      .append(Element::builder("attribute").attr("name", "type"))
                                      .append(Element::builder("attribute").attr("name", "initials"))
                                      .append(Element::builder("optional")
                                        .append(Element::builder("attribute").attr("name", "un"))
                                      )
                                      .append(Element::builder("text"))
                                    )
                                  )
                                )
                              )
                            )
                          )

                          // lists
                          // verbatim lists don't need special handling in XML, unlike TeX so they are here
                          .append(Element::builder("zeroOrMore")
                            .append(Element::builder("element").attr("name", format!("{bbl}:list"))
                              .append(Element::builder("attribute").attr("name", "name")
                                .append(Element::builder("choice")
                                  .append_all(self.get_fields_of_fieldtype("list")
                                  .filter(|k| !self.field_is_datatype("name", k) && !self.field_is_datatype("uri", k) && !self.field_is_skipout(k))
                                    .map(|list| Element::builder("value").text(list))
                                  )
                                )
                              )
                              .append(Element::builder("attribute").attr("name", "count")
                                .append(Element::builder("data").attr("type", "integer"))
                              )
                              .append(Element::builder("optional")
                                .append(Element::builder("attribute").attr("name", "more")
                                  .append(Element::builder("data").attr("type", "boolean"))
                                )
                              )
                              .append(Element::builder("oneOrMore")
                                .append(Element::builder("element").attr("name", format!("{bbl}:item"))
                                  .append(Element::builder("text"))
                                )
                              )
                            )
                          )

                          // fields
                          .append({
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
                            let fs2 = self.get_fields_of_type(FieldType::Field,
                              &[DataType::Entrykey,
                              DataType::Key,
                              DataType::Integer,
                              DataType::Datepart,
                              DataType::Literal,
                              DataType::Code,
                              DataType::Verbatim], None).filter(|f| !(self.get_fieldformat(f) == Format::Xsv)
                              && !self.field_is_skipout(f));

                            // uri fields
                            let fs3 = self.get_fields_of_type(FieldType::Field, &[DataType::Uri], None);

                            // <namelist>namehash and <namelist>fullhash
                            let fs4 = Vec::new();
                            for n in &dmh.namelists {
                              fs4.push(format!("{n}namehash"));
                              fs4.push(format!("{n}bibnamehash"));
                              fs4.push(format!("{n}fullhash"));
                            }

                            Element::builder("oneOrMore")
                              .append(Element::builder("element").attr("name", format!("{bbl}:field"))
                                // start choice of normal vs datepart fields
                                .append(Element::builder("choice")
                                  .append(Element::builder("group")
                                    .append(Element::builder("attribute").attr("name", "name")
                                      .append(Element::builder("choice")
                                        .append_all(fs1.iter()
                                          .chain(fs2)
                                          .chain(fs3.iter())
                                          .chain(fs4.iter())
                                          .map(|f| Element::builder("value").text(f))
                                        )
                                      )
                                    )
                                  )
                                  .append(Element::builder("group")
                                    .append(Element::builder("attribute").attr("name", "name")
                                      .append(Element::builder("choice")
                                        .append_all(self.get_fields_of_type(FieldType::Field, &[DataType::Datepart], None)
                                          .map(|dp| Element::builder("value").text(dp))
                                        )
                                      )
                                    )
                                    // dateparts may have an era attributes
                                    .append(Element::builder("optional")
                                      .append(Element::builder("attribute").attr("name", "startera")
                                        .append(Element::builder("choice")
                                          .append(Element::builder("value").text("bce"))
                                          .append(Element::builder("value").text("ce"))
                                        )
                                      )
                                    )
                                    .append(Element::builder("optional")
                                      .append(Element::builder("attribute").attr("name", "endera")
                                        .append(Element::builder("choice")
                                          .append(Element::builder("value").text("bce"))
                                          .append(Element::builder("value").text("ce"))
                                        )
                                      )
                                    )
                                    // dateparts may have a julian attributes
                                    .append(Element::builder("optional")
                                      .append(Element::builder("attribute").attr("name", "startjulian")
                                        .append(Element::builder("choice")
                                          .append(Element::builder("value").text("true"))
                                        )
                                      )
                                    )
                                    .append(Element::builder("optional")
                                      .append(Element::builder("attribute").attr("name", "endjulian")
                                        .append(Element::builder("choice")
                                          .append(Element::builder("value").text("true"))
                                        )
                                      )
                                    )
                                    // dateparts may have a approximate attributes
                                    .append(Element::builder("optional")
                                      .append(Element::builder("attribute").attr("name", "startapproximate")
                                        .append(Element::builder("choice")
                                          .append(Element::builder("value").text("true"))
                                        )
                                      )
                                    )
                                    .append(Element::builder("optional")
                                      .append(Element::builder("attribute").attr("name", "endapproximate")
                                        .append(Element::builder("choice")
                                          .append(Element::builder("value").text("true"))
                                        )
                                      )
                                    )
                                    // dateparts may have an uncertain attributes
                                    .append(Element::builder("optional")
                                      .append(Element::builder("attribute").attr("name", "startuncertain")
                                        .append(Element::builder("choice")
                                          .append(Element::builder("value").text("true"))
                                        )
                                      )
                                    )
                                    .append(Element::builder("optional")
                                      .append(Element::builder("attribute").attr("name", "enduncertain")
                                        .append(Element::builder("choice")
                                          .append(Element::builder("value").text("true"))
                                        )
                                      )
                                    )
                                    // dateparts may have an unknown attributes
                                    .append(Element::builder("optional")
                                      .append(Element::builder("attribute").attr("name", "startunknown")
                                        .append(Element::builder("choice")
                                          .append(Element::builder("value").text("true"))
                                        )
                                      )
                                    )
                                    .append(Element::builder("optional")
                                      .append(Element::builder("attribute").attr("name", "endunknown")
                                        .append(Element::builder("choice")
                                          .append(Element::builder("value").text("true"))
                                        )
                                      )
                                    )
                                  )
                                )
                                .append(Element::builder("text"))
                              )
                          })

                          // ranges
                          .append(Element::builder("oneOrMore")
                            .append(Element::builder("element").attr("name", format!("{bbl}:range"))
                              .append(Element::builder("attribute").attr("name", format!("{bbl}:range"))
                                .append(Element::builder("choice")
                                  .append_all(self.get_fields_of_datatype(&[DataType::Range]).iter()
                                    .filter(|f| !self.field_is_skipout(f))
                                    .map(|r| Element::builder("value").text(r))
                                  )
                                )
                              )
                              .append(Element::builder("oneOrMore")
                                .append(Element::builder("element").attr("name", format!("{bbl}:item"))
                                  .append(Element::builder("attribute").attr("name", "length")
                                    .append(Element::builder("data").attr("type", "integer"))
                                  )
                                  .append(Element::builder("element").attr("name", format!("{bbl}:start"))
                                    .append(Element::builder("text"))
                                  )
                                  .append(Element::builder("optional")
                                    .append(Element::builder("element").attr("name", format!("{bbl}:end"))
                                      .append(Element::builder("text"))
                                    )
                                  )
                                )
                              )
                            )
                          )

                          // uri lists - not in default data model
                          .optional_append({
                            let uril = self.get_fields_of_type(FieldType::List, &[DataType::Uri], None);
                            (!uril.is_empty()).then(||
                              Element::builder("optional")
                                .append(Element::builder("element").attr("name", format!("{bbl}:list"))
                                  .append(Element::builder("attribute").attr("name", "name")
                                    .append(Element::builder("choice")
                                      .append_all(uril.into_iter()
                                        .map(|u| Element::builder("value").text(u))
                                      )
                                    )
                                  )
                                  .append(Element::builder("attribute").attr("name", "count")
                                    .append(Element::builder("data").attr("type", "integer"))
                                  )
                                  .append(Element::builder("oneOrMore")
                                    .append(Element::builder("element").attr("name", format!("{bbl}:item"))
                                      .append(Element::builder("data").attr("type", "anyURI"))
                                    )
                                  )
                                )
                            )
                          })

                          // nocite
                          .append(Element::builder("optional")
                            .append(Element::builder("element").attr("name", format!("{bbl}:item"))
                              .append(Element::builder("empty"))
                            )
                          )

                          // keywords
                          .append(Element::builder("optional")
                            .append(Element::builder("element").attr("name", format!("{bbl}:keywords"))
                              .append(Element::builder("oneOrMore")
                                .append(Element::builder("element").attr("name", format!("{bbl}:keyword"))
                                  .append(Element::builder("data").attr("type", "string"))
                                )
                              )
                            )
                          )

                          // annotations
                          .append(Element::builder("zeroOrMore")
                            .append(Element::builder("element").attr("name", format!("{bbl}:annotation"))
                              .append(Element::builder("attribute").attr("name", "scope")
                                .append(Element::builder("choice")
                                  .append_all(["field", "list", "names", "item", "name", "namepart"].into_iter()
                                    .map(|s| Element::builder("value").text(s))
                                  )
                                )
                              )
                              .append(Element::builder("attribute").attr("name", "field"))
                              .append(Element::builder("attribute").attr("name", "name"))
                              .append(Element::builder("attribute").attr("name", "value"))
                              .append(Element::builder("attribute").attr("name", "literal")
                                .append(Element::builder("choice")
                                  .append_all(["1", "0"].into_iter()
                                    .map(|s| Element::builder("value").text(s))
                                  )
                                )
                              )
                              .append(Element::builder("optional")
                                .append(Element::builder("attribute").attr("name", "item"))
                              )
                              .append(Element::builder("optional")
                                .append(Element::builder("attribute").attr("name", "part"))
                              )
                            )
                          )

                          // warnings
                          .append(Element::builder("zeroOrMore")
                            .append(Element::builder("element").attr("name", format!("{bbl}:warning"))
                              .append(Element::builder("data").attr("type", "string"))
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )

              // aliases
              .append(Element::builder("zeroOrMore")
                .append(Element::builder("element").attr("name", format!("{bbl}:keyalias"))
                  .append(Element::builder("attribute").attr("name", "key"))
                  .append(Element::builder("text"))
                )
              )

              // missing keys
              .append(Element::builder("zeroOrMore")
                .append(Element::builder("element").attr("name", format!("{bbl}:missing"))
                  .append(Element::builder("text"))
                )
              )
            )
          )
        )
      )
      .build();
      let mut ns = Namespace::empty();
      ns.force_put(bbl, bbl_ns);
      ns.force_put("", default_ns);
      grammar_tag.namespaces = Some(ns);
  
      let mut rng = File::create(outfile).unwrap;
      write!(&mut rng, r##"<?xml version="1.0" encoding="UTF-8"?>
  <!-- Auto-generated from .bcf Datamodel -->
  
  "##);
      let mut cfg = EmitterConfig::new();
      cfg.perform_indent = true;
      cfg.write_document_declaration = false;
      grammar_tag.write_with_config(&mut rng, cfg).unwrap();
  }
}