///! `crate::entry::Name` objects

// no autovivification;

use itertools::Itertools;
use crate::Id;
use indexmap::indexmap as attribute_map;
type AttributeMap = indexmap::IndexMap<String, String>;

/*
use Regexp::Common qw( balanced );
use crate::Config;
use crate::Constants;
use Data::Dump qw( pp );
use Log::Log4perl qw( :no_extra_logdie_message );
use List::Util qw( first );
use Unicode::Normalize;
*/

impl Name {
  /// Initialise a crate::Entry::Name object, optionally with key=>value arguments.
  fn new(params) -> Self {
    let $dm = crate::config::get_dm();
    if (%params) {
      let $name = {};

      // Name is an XDATA reference
      if (let $xdata = $params{xdata}) {
        $name->{xdata} = $xdata;
        $name->{id} = Id::new(),
        return bless $name, $class;
      }

      for attr in (CONFIG_OPT_SCOPE_BIBLATEX.iter_by_right("NAME"),
                        "gender",
                        "useprefix",
                        "strip") {
        if (exists $params{$attr}) {
          $name->{$attr} = $params{$attr};
        }
      }
      for np in dm.get_constant_value("nameparts") {
        if (exists $params{$np}) {
          $name->{nameparts}{$np} = $params{$np};
        }
      }
      $name->{rawstring} = name.nameparts.values().map(|val| val.string.unwrap_or("")).join("");
      $name->{id} = Id::new();
      return bless $name, $class;
    }
    else {
      return bless {id => Id::new()}, $class;
    }
  }

  fn get_gender(&self) -> Unknown {
    &self.gender
  }

  pub fn get_hash(&self) -> &Option<String> {
    &self.hash
  }

  pub fn set_hash(&mut self, hash: &str) {
    self.hash = Some(hash.into());
  }

  pub fn get_index(&self) -> usize {
    self.index
  }

  pub fn set_index(&mut self, index: usize) {
    self.index = index;
  }

  pub fn get_id(&self) -> Id {
    self.id
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
  fn get_nameparts(&self) -> impl Iterator<Item=&String> {
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
  fn name_to_biblatexml(self, out, xml: &mut Element, $key, $namefield, $count) -> String {
    let $xml_prefix = $out->{xml_prefix};
    let $dm = crate::config::get_dm();
    let mut attrs = AttributeMap::new();


    // Add per-name options
    for no in CONFIG_OPT_SCOPE_BIBLATEX.iter_by_right("NAME") {
      if (defined(self.${\"get_$no"})) {
        let $nov = self.${\"get_$no"};

        if ($CONFIG_BIBLATEX_OPTIONS{NAME}{$no}{OUTPUT}) {
          attrs.insert(no, crate::utils::map_boolean($no, $nov, "tostring"));
        }
      }
    }

    // gender
    if let Some(g) = self.get_gender().skip_empty() {
      attrs.insert("gender", g);
    }

    // name scope annotation
    let ann = &crate::annotation::ANN.lock().unwrap();
    if let Some(ann) = ann.get_item_annotation(key, namefield, count).map(|ann| ann.value()).skip_empty() {
      attrs.insert("annotation", ann);
    }

    $xml->startTag([$xml_prefix, "name"], @attrs);

    for np in dm.get_constant_value("nameparts") {// list type so returns list
      self.name_part_to_bltxml($xml, $xml_prefix, key, namefield, np, count);
    }

    $xml->endTag(); // Name
  }

  /// Return BibLaTeXML data for a name
  fn name_part_to_bltxml(self, $xml, $xml_prefix, $key, $namefield, $npn, $count) {
    let $np = self.get_namepart($npn);
    let $nip = self.get_namepart_initial($npn);
    if ($np) {
      let parts: Vec<_> = regex!(r"[\s~]").split(np).collect();
      let mut attrs = AttributeMap::new();

      // namepart scope annotation
      let ann = &crate::annotation::ANN.lock().unwrap();
      if let Some(ann) = ann.get_part_annotation(key, namefield, count, npn).map(|ann| ann.value()).skip_empty() {
        attrs.insert("annotation", ann);
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

    for np in ($dm->get_constant_value("nameparts")) {// list type so returns list
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
    for no in CONFIG_OPT_SCOPE_BIBLATEX.iter_by_right("NAME") {
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
    let mut pno = BTreeMap::new(); // per-name options
    let %names;
    let $nid = self.{id};

    for np in ($dm->get_constant_value("nameparts")) {// list type so returns list
      let npc = self.get_namepart(np);
      let npci = None;

      if npc.skip_empty().is_some() {
        npci = Some(self.get_namepart_initial(np).join(". "));
      }
      // Some of the subs above can result in these being undef so make sure there is an empty
      // string instead of undef so that interpolation below doesn't produce warnings
      let npc = npc.unwrap_or("");
      let npci = npci.unwrap_or("");
      if !npc.is_empty() {
        names.insert(np, vec![npc, npci]);
        if un != "false" {
          names{np}.push(format!("[BDS]UNP-{np}-{nid}[/BDS]"));
        }
      }
    }

    // Generate uniquename if uniquename is requested
    if un != "false" {
      pno.insert("un", format!("[BDS]UNS-{nid}[/BDS]"));
      pno.insert("uniquepart", format!("[BDS]UNP-{nid}[/BDS]"));
    }

    // Add per-name options
    for no in CONFIG_OPT_SCOPE_BIBLATEX.iter_by_right("NAME") {
      if (defined(self.${\"get_$no"})) {
        let $nov = self.${\"get_$no"};

        if ($CONFIG_BIBLATEX_OPTIONS{NAME}{$no}{OUTPUT}) {
          pno.insert(no, crate::utils::map_boolean(no, nov, "tostring"));
        }
      }
    }

    // Add the name hash to the options
    pno.insert("hash", format!("[BDS]{nid}-PERNAMEHASH[/BDS]"));

    $xml->startTag([$xml_prefix, "name"], map {$_ => $pno{$_}} sort keys %pno);
    for (key, value) in names.iter().sorted_by(|(k, _) k) {
      let %un;
      if un != "false" {
        %un = (un => value[2]);
      }
      $xml->startTag([$xml_prefix, "namepart"],
                     type => $key,
                     %un,
                     initials => NFC(crate::utils::normalise_string_bblxml(value[1])));
      $xml->characters(NFC(crate::utils::normalise_string_bblxml(value[0])));
      $xml->endTag();// namepart
    }
    $xml->endTag();// names

    return;
  }

  /// Return standard bibtex data format for name
  fn name_to_bibtex(&self) -> String {
    let parts = HashMap::new();
    let namestring = String::new();

    let xdata = self.get_xdata();
    if !xdata.is_empty() {
      return xdata.into();
    }

    for np in ["prefix", "family", "suffix", "given"] {
      let namepart = self.get_namepart(np);
      parts.insert(np, namepart);
      if !namepart.is_empty() {
        let namepart = namepart.replace("~", " ");
        parts.insert(np, namepart);
        if (self.was_stripped(np)) {
          parts.insert(np, crate::utils::add_outer(&namepart));
        }
      }
    }

    if (let $p = $parts->{prefix}) {
      namestring.push_str(p);
      namestring.push(' ');
    };
    if (let $f = $parts->{family}) {
      namestring.push_str(f);
    };
    if (let $s= $parts->{suffix}) {
      namestring.push_str(", ");
      namestring.push_str(s);
    };
    if (let $g= $parts->{given}) {
      namestring.push_str(", ");
      namestring.push_str(g);
    };

    namestring
  }

  /// Return extended bibtex data format for name
  fn name_to_xname(self) {
    let $dm = crate::config::get_dm();
    let $parts;
    let namestring = Vec::new();
    let $xns = crate::Config->getoption("output_xnamesep");

    for np in (sort $dm->get_constant_value("nameparts")) {// list type so returns list
      let namepart = self.get_namepart(np);
      parts.insert(np, namepart);
      if !namepart.is_empty() {
        let namepart = namepart.replace("~", " ");
        namestring.push(format!("{np}{xns}{}", &namepart));
        parts.insert(np, namepart);
      }
    }

    // Name scope useprefix
    if let Some(pref) = self.get_useprefix() {// could be 0
      namestring.push(format!("useprefix{xns}{}", crate::utils::map_boolean("useprefix", pref, "tostring")));
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
