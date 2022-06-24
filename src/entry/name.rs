///! `crate::entry::Name` objects

// no autovivification;

use Regexp::Common qw( balanced );
use crate::Annotation;
use crate::Config;
use crate::Constants;
use Data::Dump qw( pp );
use Log::Log4perl qw( :no_extra_logdie_message );
use List::Util qw( first );
use Unicode::Normalize;

impl Name {
  /// Initialise a crate::Entry::Name object, optionally with key=>value arguments.
  fn new(params) -> Self {
    let $dm = crate::config::get_dm();
    if (%params) {
      let $name = {};

      // Name is an XDATA reference
      if (let $xdata = $params{xdata}) {
        $name->{xdata} = $xdata;
        $name->{id} = base62_uuid::base62_uuid(),
        return bless $name, $class;
      }

      foreach let $attr (keys $CONFIG_SCOPEOPT_BIBLATEX{NAME}->%*,
                        "gender",
                        "useprefix",
                        "strip") {
        if (exists $params{$attr}) {
          $name->{$attr} = $params{$attr};
        }
      }
      foreach let $np ($dm->get_constant_value("nameparts")) {
        if (exists $params{$np}) {
          $name->{nameparts}{$np} = $params{$np};
        }
      }
      $name->{rawstring} = join("",
                                map {$name->{nameparts}{$_}{string}.unwrap_or("")} keys $name->{nameparts}->%*);
      $name->{id} = base62_uuid::base62_uuid();
      return bless $name, $class;
    }
    else {
      return bless {id => base62_uuid::base62_uuid()}, $class;
    }
  }

  fn get_gender(&self) -> Unknown {
    &self.gender
  }

  pub fn get_hash(&self) -> Unknown {
    &self.hash
  }

  pub fn set_hash(&mut self, hash: Unknown) {
    self.hash = hash;
  }

  pub fn get_index(&self) -> usize {
    self.index
  }

  pub fn set_index(&mut self, index: usize) {
    self.index = index;
  }

  pub fn get_id(&self) -> &String {
    &self.id
  }

  pub fn get_rawstring(&self) -> &String {
    &self.rawstring
  }

  /// Serialiser for JSON::XS::encode
  fn TO_JSON(self) {
    let $json;
    while (let ($k, $v) = each(self.%*)) {
      $json->{$k} = $v;
    }
    return $json;
  }

  /// Test for an empty object
  fn notnull(self) {
    let @arr = keys %$self;
    return $#arr > -1 ? 1 : 0;
  }

  /// Return boolean to tell if the passed field had braces stripped from the original
  fn was_stripped(self, part) {
    self.strip.map(|strip| strip[part])
  }

  /// Get nameparts for a name
  fn get_nameparts(self) {
    self.nameparts.keys()
  }

  /// Get any xdata reference information for a name
  fn get_xdata(&self) -> &str {
    self.xdata.unwrap_or("")
  }

  /// Get a namepart by passed name
  fn get_namepart(&self, namepart: &str) -> &str {
    // prevent warnings when concating arbitrary nameparts
    self.nameparts[namepart].string.unwrap_or("")
  }

  /// Set a namepart by passed name
  fn set_namepart(&mut self, namepart: &str, val: &str) {
    self.nameparts[namepart].string = val.into();
  }

  /// Get a namepart initial by passed name
  fn get_namepart_initial(self, namepart: &str) {
    return self.nameparts[namepart].initial;
  }

  /// Set a namepart initial by passed name
  fn set_namepart_initial(self, $namepart, $val) {
    self.{nameparts}{$namepart}{initial} = $val;
    return;
  }

  /// Create biblatexml data for a name
  fn name_to_biblatexml(self, $out, $xml, $key, $namefield, $count) -> String {
    let $xml_prefix = $out->{xml_prefix};
    let $dm = crate::config::get_dm();
    let @attrs;


    // Add per-name options
    foreach let $no (keys $CONFIG_SCOPEOPT_BIBLATEX{NAME}->%*) {
      if (defined(self.${\"get_$no"})) {
        let $nov = self.${\"get_$no"};

        if ($CONFIG_BIBLATEX_OPTIONS{NAME}{$no}{OUTPUT}) {
          push @attrs, ($no => crate::utils::map_boolean($no, $nov, "tostring"));
        }
      }
    }

    // gender
    if let Some(g) = self.get_gender().skip_empty() {
      push @attrs, ("gender" => g);
    }

    // name scope annotation
    if (let $ann = crate::Annotation->get_annotation("item", $key, $namefield, $count)) {
      push @attrs, ("annotation" => $ann);
    }

    $xml->startTag([$xml_prefix, "name"], @attrs);

    foreach let $np ($dm->get_constant_value("nameparts")) {// list type so returns list
      self.name_part_to_bltxml($xml, $xml_prefix, $key, $namefield, $np, $count);
    }

    $xml->endTag(); // Name
  }

  /// Return BibLaTeXML data for a name
  fn name_part_to_bltxml(self, $xml, $xml_prefix, $key, $namefield, $npn, $count) {
    let $np = self.get_namepart($npn);
    let $nip = self.get_namepart_initial($npn);
    if ($np) {
      let $parts = [split(/[\s~]/, $np)];
      let @attrs;

      // namepart scope annotation
      if (let $ann = crate::Annotation->get_annotation("part", $key, $namefield, $count, $npn)) {
        push @attrs, ("annotation" => $ann);
      }

      // Compound name part
      if parts.len() > 1 {
        $xml->startTag([$xml_prefix, "namepart"], type => $npn, @attrs);
        for (i, part) in parts {
          if (let $init = nip[i]) {
            $xml->startTag([$xml_prefix, "namepart"], initial => $init);
          }
          else {
            $xml->startTag([$xml_prefix, "namepart"]);
          }
          $xml->characters(NFC(part));
          $xml->endTag();         // namepart
        }
        $xml->endTag();           // namepart
      }
      else { // simple name part
        if (let $init = nip[0]) {
          $xml->startTag([$xml_prefix, "namepart"], type => $npn, initial => $init, @attrs);
        }
        else {
          $xml->startTag([$xml_prefix, "namepart"]);
        }
        $xml->characters(NFC(parts[0]));
        $xml->endTag();           // namepart
      }
    }
  }

  /// Return bbl data for a name
  fn name_to_bbl(self, un: &str) -> String {
    let $dm = crate::config::get_dm();
    let @pno; // per-name options
    let @namestrings;
    let $nid = self.{id};

    foreach let $np ($dm->get_constant_value("nameparts")) {// list type so returns list
      let $npc;
      let $npci;
      if ($npc = self.get_namepart($np)) {

        if (self.was_stripped($np)) {
          $npc = crate::utils::add_outer($npc);
        }
        else {
          // Don't insert name seps in protected names
          $npc = crate::utils::join_name($npc);
        }

        $npci = self.get_namepart_initial($np).join(r"\bibinitperiod\bibinitdelim ") + r"\bibinitperiod";
        $npci =~ s/\p{Pd}/\\bibinithyphendelim /gxms;
      }
      // Some of the subs above can result in these being undef so make sure there is an empty
      // string instead of undef so that interpolation below doesn't produce warnings
      let npc = npc.unwrap_or("");
      let npci = npci.unwrap_or("");

      if !npc.is_empty() {
        namestrings.push(format!("           {np}={{{npc}}}"));
        namestrings.push(format!("           {np}i={{{npci}}}"));
        // Only if uniquename is true
        if un != "false" {
          namestrings.push(format!("           <BDS>UNP-{np}-{nid}</BDS>"));
        }
      }
    }

    // Generate uniquename if uniquename is requested
    if un != "false" {
      pno.push(format!("<BDS>UNS-{nid}</BDS>"));
      pno.push(format!("<BDS>UNP-{nid}</BDS>"));
    }

    // Add per-name options
    foreach let $no (keys $CONFIG_SCOPEOPT_BIBLATEX{NAME}->%*) {
      if (defined(self.${\"get_$no"})) {
        let $nov = self.${\"get_$no"};

        if ($CONFIG_BIBLATEX_OPTIONS{NAME}{$no}{OUTPUT}) {
          pno.push(format!("{no}={}", crate::utils::map_boolean(no, nov, "tostring")));
        }
      }
    }

    // Add the name hash to the options
    push @pno, "<BDS>${nid}-PERNAMEHASH</BDS>";
    let pno = pno.join(",");

    let mut namestring = format!("        {{{{{pno}}}{{%\n");
    namestring += namestrings.join(",\n");
    namestring += "}}%\n";

    namestring
  }

  /// Return bblxml data for a name
  fn name_to_bblxml*(self, $xml, $xml_prefix, $un) {
    let $dm = crate::config::get_dm();
    let %pno; // per-name options
    let %names;
    let $nid = self.{id};

    foreach let $np ($dm->get_constant_value("nameparts")) {// list type so returns list
      let $npc;
      let $npci;

      if ($npc = self.get_namepart($np)) {
        $npci = join('. ', @{self.get_namepart_initial($np)});
      }
      // Some of the subs above can result in these being undef so make sure there is an empty
      // string instead of undef so that interpolation below doesn't produce warnings
      $npc = npc.unwrap_or("");
      $npci = npci.unwrap_or("");
      if ($npc) {
        $names{$np} = [$npc, $npci];
        if ($un != "false") {
          push $names{$np}->@*, "[BDS]UNP-${np}-${nid}[/BDS]";
        }
      }
    }

    // Generate uniquename if uniquename is requested
    if ($un != "false") {
      $pno{un} = "[BDS]UNS-${nid}[/BDS]";
      $pno{uniquepart} = "[BDS]UNP-${nid}[/BDS]";
    }

    // Add per-name options
    foreach let $no (keys $CONFIG_SCOPEOPT_BIBLATEX{NAME}->%*) {
      if (defined(self.${\"get_$no"})) {
        let $nov = self.${\"get_$no"};

        if ($CONFIG_BIBLATEX_OPTIONS{NAME}{$no}{OUTPUT}) {
          $pno{$no} = crate::utils::map_boolean($no, $nov, "tostring");
        }
      }
    }

    // Add the name hash to the options
    $pno{hash} = "[BDS]${nid}-PERNAMEHASH[/BDS]";

    $xml->startTag([$xml_prefix, "name"], map {$_ => $pno{$_}} sort keys %pno);
    foreach let $key (sort keys %names) {
      let $value = $names{$key};
      let %un;
      if ($un != "false") {
        %un = (un => $value->[2]);
      }
      $xml->startTag([$xml_prefix, "namepart"],
                     type => $key,
                     %un,
                     initials => NFC(crate::utils::normalise_string_bblxml($value->[1])));
      $xml->characters(NFC(crate::utils::normalise_string_bblxml($value->[0])));
      $xml->endTag();// namepart
    }
    $xml->endTag();// names

    return;
  }

  /// Return standard bibtex data format for name
  fn name_to_bibtex(self) {
    let $parts;
    let $namestring = String::new();

    if (let $xdata = self.get_xdata) {
      return $xdata;
    }

    foreach let $np ("prefix", "family", "suffix", "given") {
      if ($parts->{$np} = self.get_namepart($np)) {
        $parts->{$np} =~ s/~/ /g;
        if (self.was_stripped($np)) {
          $parts->{$np} = crate::utils::add_outer($parts->{$np});
        }
      }
    }

    if (let $p = $parts->{prefix}) {$namestring .= "$p "};
    if (let $f = $parts->{family}) {$namestring .= $f};
    if (let $s= $parts->{suffix}) {$namestring .= ", $s"};
    if (let $g= $parts->{given}) {$namestring .= ", $g"};

    return $namestring;
  }

  /// Return extended bibtex data format for name
  fn name_to_xname(self) {
    let $dm = crate::config::get_dm();
    let $parts;
    let namestring = Vec::new();
    let $xns = crate::Config->getoption("output_xnamesep");

    for np in (sort $dm->get_constant_value("nameparts")) {// list type so returns list
      if ($parts->{$np} = self.get_namepart(np)) {
        $parts->{$np} =~ s/~/ /g;
        namestring.push(format!("{np}{xns}{}", parts.np));
      }
    }

    // Name scope useprefix
    if let Some(useprefix) = self.get_useprefix) {// could be 0
      namestring.push(format!("useprefix{xns}{}", crate::utils::map_boolean("useprefix", useprefix, "tostring")));
    }

    // Name scope sortingnamekeytemplatename
    if let Some(snks) = self.get_sortingnamekeytemplatename().skip_empty() {
      namestring.push(format!("sortingnamekeytemplatename{xns}{snks}"));
    }

    namestring.join(", ");
  }

  /// Dump crate::entry::Name object
  fn dump(self) {
    return pp($self);
  }
}
