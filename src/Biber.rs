//! main module for biber, a bibtex replacement for users of biblatex
//!
//! ```
//! use Biber;
//! let $biber = Biber->new();
//! $biber->parse_ctrlfile("example.bcf");
//! $biber->prepare;
//! ```

use crate::section::{DataSource, DataSourceType};
use crate::datalist::Filter;

/*
use parent qw(Class::Accessor crate::Internals);

use constant {
  EXIT_OK => 0,
  EXIT_ERROR => 2
};

use crate::Config;
use crate::DataLists;
use crate::DataList;
use crate::DataModel;
use crate::Constants;
use crate::Internals;
use crate::Entries;
use crate::Entry;
use crate::Entry::Names;
use crate::Entry::Name;
use crate::Sections;
use crate::Section;
use crate::LaTeX::Recode;
use crate::UCollate;
use crate::Utils;
use Carp;
use Data::Dump;
use Data::Compare;
use Encode;
use File::Copy;
use File::Slurper;
use File::Spec;
use File::Temp;
use IO::File;
use List::AllUtils qw( first uniq max first_index );
use Log::Log4perl qw( :no_extra_logdie_message );
use Scalar::Util qw(looks_like_number);
use Sort::Key qw ( multikeysorter );
use Text::BibTeX qw(:macrosubs);
use Unicode::Normalize;
*/

pub struct Biber;

impl Biber {
  /// Initialize the Biber object, optionally passing named options as arguments.
  fn new(%opts) -> Self {
    let self = bless {}, $class;

    crate::Config->_initopts(\%opts);

    // Add a reference to a global temp dir used for various things
    $self->{TEMPDIR} = File::Temp->newdir("biber_tmp_XXXX",
                                          TMPDIR => 1,
                                          CLEANUP => (crate::Config->getoption("noremove_tmp_dir") ? 0 : 1));
    $self->{TEMPDIRNAME} = $self->{TEMPDIR}->dirname;

    // Initialise recoding schemes
    crate::LaTeX::Recode->init_sets(crate::Config->getoption("decodecharsset"),
                                    crate::Config->getoption("output_safecharsset"));

    $MASTER = $self;

    // Validate if asked to.
    // This has to be here, after config file is read and options
    // are parsed. It seems strange to validate the config file after it's been
    // read but there is no choice and it's useful anyway as this will catch some semantic
    // errors. Uses biber_error() and so $MASTER has to be defined before we call this
    if (crate::Config->getoption("validate_config") && $opts{configfile}) {
      validate_biber_xml($opts{configfile}, "config", "");
    }
/*
    // Set up LangTag parser
    $self->{langtags} = crate::LangTags->new();
*/
    return $self;
  }


  /// Output summary of warnings/errors/misc before exit
  fn display_end(self) {
    // Show location of temporary directory
    if (crate::Config->getoption("show_tmp_dir")) {
      if (crate::Config->getoption("noremove_tmp_dir")) {
        info!("TEMP DIR: {}", self.biber_tempdir_name());
      }
      else {
        biber_warn("--noremove-tmp-dir was not set, no temporary directory to show");
      }
    }

    if !self.warnings.is_empty() {
      for w in &self.warnings {
        warn!(w);
      }
      info!("WARNINGS: {}", self.warnings.len());
    }
    if ($self->{errors}) {
      info!("ERRORS: {}", $self->{errors});
      exit EXIT_ERROR;
    }
  }

  /// Returns a File::Temp directory object for use in various things
  fn biber_tempdir(self) {
    return $self->{TEMPDIR};
  }

  /// Returns the directory name of the File::Temp directory object
  fn biber_tempdir_name(self) {
    return $self->{TEMPDIRNAME};
  }

  /// Returns a crate::Sections object describing the bibliography sections
  ///
  /// ```
  /// let sections = biber.sections()
  /// ```
  fn sections(&self) -> &Sections {
    &self.sections
  }

  /// Adds a crate::Sections object. Used externally from, e.g. biber
  fn add_sections(&mut self, sections: Sections) {
    self.sections = sections;
  }

  /// Returns a crate::DataLists object describing the bibliography sorting lists
  ///
  /// ```
  /// let $datalists = $biber->datalists
  /// ```
  fn datalists(&self) -> &DataLists {
    &self.datalists
  }
/*
  /// Returns a crate::LangTags object containing a parser for BCP47 tags
  fn langtags(self) {
    return $self->{langtags};
  }
*/
  /// Sets the object used to output final results
  /// Must be a subclass of crate::Output::base
  fn set_output_obj(self, obj) {
    assert!($obj->isa("crate::Output::base"), "Output object must be subclass of crate::Output::base!");
    $self->{output_obj} = $obj;
    return;
  }

  /// Returns the current preamble as an array ref
  fn get_preamble(self) {
    return $self->{preamble};
  }

  /// Returns the object used to output final results
  fn get_output_obj(&mut self) -> &mut crate::Output {
    &mut self.output_obj
  }

  /// Sets the current section number that we are working on to a section number
  fn set_current_section(self, secnum: u32) {
    self.current_section = secnum;
  }

  /// Gets the current section number that we are working on
  fn get_current_section(&self) -> u32 {
    self.current_section
  }

  /// Fakes parts of the control file for tool mode
  fn tool_mode_setup(&mut self) {
    let bib_sections = crate::Sections::new();
    // There are no sections in tool mode so create a pseudo-section
    let $bib_section = crate::Section::new(99999);
    let mut ifs = Vec::new();
    for i_f in (@ARGV) {
      ifs.push(DataSource {
        type: DataSourceType::File,
        name: i_f,
        datatype: crate::Config->getoption("input_format"),
        encoding: crate::Config->getoption("input_encoding"),
        glob: None,
      });
    }
    bib_section.set_datasources(ifs);

    bib_section.set_allkeys(true);
    bib_sections.add_section(bib_section);

    // Always resolve date meta-information in tool mode
    crate::Config->setblxoption(None, "dateapproximate", 1);
    crate::Config->setblxoption(None, "dateera", 1);
    crate::Config->setblxoption(None, "dateuncertain", 1);

    // No need to worry about this in tool mode but it needs to be set
    crate::Config->setblxoption(None, "namestrunchandling", 0);

    // Add the crate::Sections object to the Biber object
    self.add_sections(bib_sections);

    let datalists = crate::DataLists::new();
    let seclist = crate::DataList::new(
      99999,
      "tool/global//global/global",
      "entry",
      "tool",
      "global",
      "global",
      "global",
      "",
    );
    // Locale just needs a default here - there is no biblatex option to take it from
    crate::Config->setblxoption(None, "sortlocale", "en_US");
      debug!("Adding "entry" list "tool" for pseudo-section 99999");
    datalists.add_list(seclist);
    self.datalists = datalists;

    // User maps are set in config file and need some massaging which normally
    // happens in parse_ctrlfile
    if (let $usms = crate::Config->getoption("sourcemap")) {
      // Force "user" level for the maps
      $usms->@* = map {$_->{level} = "user";$_} $usms->@*;
    }
    return;
  }

  /// This method reads the control file
  /// generated by biblatex to work out the various biblatex options.
  /// See Constants.pm for defaults and example of the data structure being built here.
  fn parse_ctrlfile(self, $ctrl_file) {
    let $ctrl_file_path = locate_data_file($ctrl_file);
    crate::Config->set_ctrlfile_path($ctrl_file_path);

    if !($ctrl_file_path && check_exists($ctrl_file_path)) {
      biber_error("Cannot find control file '$ctrl_file'! - Did latex run successfully on your .tex file before you ran biber?")
    }

    // Early check to make sure .bcf is well-formed. If not, this means that the last biblatex run
    // exited prematurely while writing the .bcf. This results is problems for latexmk. So, if the
    // .bcf is broken, just stop here, remove the .bcf and exit with error so that we don't write
    // a bad .bbl
    let $checkbuf;
    if !($checkbuf = eval {slurp_switchr($ctrl_file_path)->$*}) {
      // Reading ctrl-file as UTF-8 failed. Probably it was written by fontenc as latin1
      // with some latin1 char in it (probably a sourcemap), so try that as a last resort
      if !(eval {$checkbuf = slurp_switchr($ctrl_file_path, "latin1")->$*}) {
        biber_error("$ctrl_file_path is not UTF-8 or even latin1, please delete it and run latex again or check that biblatex is writing a valid .bcf file.");
      }
      // Write ctrl file as UTF-8
      slurp_switchw($ctrl_file_path, $checkbuf);// Unicode NFC boundary
    }

    $checkbuf = NFD($checkbuf);// Unicode NFD boundary
    if !(eval "XML::LibXML->load_xml(string => \$checkbuf)") {
      let $output = self.get_output_obj()->get_output_target_file;
      if $output != '-' {
        unlink($output);// ignore deletion of STDOUT marker
      }
      biber_error("$ctrl_file_path is malformed, last biblatex run probably failed. Deleted $output");
    }

    // Validate if asked to
    if (crate::Config->getoption("validate_control")) {
      validate_biber_xml($ctrl_file_path, "bcf", "https://sourceforge.net/projects/biblatex");
    }

    // Convert .bcf to .html using XSLT transform if asked to
    if (crate::Config->getoption("convert_control")) {

      require XML::LibXSLT;
      require XML::LibXML;

      let $xslt = XML::LibXSLT->new();
      let $CFstyle;

      // we assume that the schema files are in the same dir as Biber.pm:
      let (vol, biber_path, _) = File::Spec->splitpath( $INC{"Biber.pm"} );

      // Deal with the strange world of PAR::Packer paths
      // We might be running inside a PAR executable and @INC is a bit odd in this case
      // Specifically, "Biber.pm" in @INC might resolve to an internal jumbled name
      // nowhere near to these files. You know what I mean if you've dealt with pp
      let $bcf_xsl;
      if ($biber_path =~ m|/par\-| && $biber_path !~ m|/inc|) { // a mangled PAR @INC path
        $bcf_xsl = File::Spec->catpath($vol, "$biber_path/inc/lib/Biber", "bcf.xsl");
      }
      else {
        $bcf_xsl = File::Spec->catpath($vol, "$biber_path/Biber", "bcf.xsl");
      }

      if (check_exists($bcf_xsl)) {
        $CFstyle = XML::LibXML->load_xml( location => $bcf_xsl, no_cdata=>1 )
      }
      else {
        biber_warn("Cannot find XML::LibXSLT stylesheet. Skipping conversion : $!");
        goto 'LOADCF;
      }

      let $CF = XML::LibXML->load_xml(location => $ctrl_file_path);
      let $stylesheet = $xslt->parse_stylesheet($CFstyle);
      let $CFhtml = $stylesheet->transform($CF);
      $stylesheet->output_file($CFhtml, $ctrl_file_path . ".html");
      info!("Converted BibLaTeX control file '{}' to '{}.html'", ctrl_file_path, ctrl_file_path);
    }

    // Open control file
  'LOADCF:
    info!("Reading '{}'", ctrl_file_path);
    let $buf = slurp_switchr($ctrl_file_path)->$*;
    $buf = NFD($buf);// Unicode NFD boundary

    // Read control file
    require XML::LibXML::Simple;

    let $bcfxml = XML::LibXML::Simple::XMLin($buf,
                                            "ForceContent" => 1,
                                            "ForceArray" => [
                                                            qr/\A(?:no)*citekey(?:count)?\z/,
                                                            qr/\Aoption\z/,
                                                            qr/\Aoptions\z/,
                                                            qr/\Avalue\z/,
                                                            qr/\Asortitem\z/,
                                                            qr/\Abibdata\z/,
                                                            qr/\Adatasource\z/,
                                                            qr/\Aconstant\z/,
                                                            qr/\Asection\z/,
                                                            qr/\Asort(?:ex|in)clusion\z/,
                                                            qr/\A(?:ex|in)clusion\z/,
                                                            qr/\Asort\z/,
                                                            qr/\Amode\z/,
                                                            qr/\Amaps\z/,
                                                            qr/\Amap\z/,
                                                            qr/\Amap_step\z/,
                                                            qr/\Aper_type\z/,
                                                            qr/\Aper_nottype\z/,
                                                            qr/\Akeypart\z/,
                                                            qr/\Apart\z/,
                                                            qr/\Asortingnamekeytemplate\z/,
                                                            qr/\Asortingtemplate\z/,
                                                            qr/\Aper_datasource\z/,
                                                            qr/\Anosort\z/,
                                                            qr/\Anonamestring\z/,
                                                            qr/\Amember\z/,
                                                            qr/\Anoinit\z/,
                                                            qr/\Anolabel\z/,
                                                            qr/\Anolabelwidthcount\z/,
                                                            qr/\Apresort\z/,
                                                            qr/\Atype_pair\z/,
                                                            qr/\Ainherit\z/,
                                                            qr/\Anamepart\z/,
                                                            qr/\Afieldor\z/,
                                                            qr/\Afieldxor\z/,
                                                            qr/\Afield\z/,
                                                            qr/\Ascope\z/,
                                                            qr/\Atransliteration\z/,
                                                            qr/\Atranslit\z/,
                                                            qr/\Aalias\z/,
                                                            qr/\Aalsoset\z/,
                                                            qr/\Aconstraints\z/,
                                                            qr/\Aconstraint\z/,
                                                            qr/\Aentryfields\z/,
                                                            qr/\Aentrytype\z/,
                                                            qr/\Adatetype\z/,
                                                            qr/\Adatalist\z/,
                                                            qr/\Alabel(?:part|element|alpha(?:name)?template)\z/,
                                                            qr/\Auniquenametemplate\z/,
                                                            qr/\Acondition\z/,
                                                            qr/\Afilter(?:or)?\z/,
                                                            qr/\Aoptionscope\z/,
                                                            ],
                                            "NsStrip" => 1,
                                            "KeyAttr" => []);
  //  use Data::Dump;dd($bcfxml);exit 0;
    let $controlversion = $bcfxml->{version};
    let $bltxversion = $bcfxml->{bltxversion};
    crate::Config->setblxoption(None, "controlversion", $controlversion);
    if $controlversion != $BCF_VERSION {
      biber_error("Error: Found biblatex control file version $controlversion, expected version $BCF_VERSION.\nThis means that your biber ($crate::Config::VERSION) and biblatex ($bltxversion) versions are incompatible.\nSee compat matrix in biblatex or biber PDF documentation.");
    }

    // Option scope
    for bcfscopeopts in ($bcfxml->{optionscope}->@*) {
      let $scope = $bcfscopeopts->{type};
      for bcfscopeopt in ($bcfscopeopts->{option}->@*) {
        let $opt = $bcfscopeopt->{content};
        $CONFIG_BIBLATEX_OPTIONS{$scope}{$opt}{OUTPUT} = $bcfscopeopt->{backendout} || 0;
        if (let $bin = process_backendin($bcfscopeopt->{backendin})) {
          $CONFIG_BIBLATEX_OPTIONS{$scope}{$opt}{INPUT} = $bin;
        }
        CONFIG_OPT_SCOPE_BIBLATEX.insert(opt, scope);
        if (defined($CONFIG_OPTTYPE_BIBLATEX{$opt}) &&
            CONFIG_OPTTYPE_BIBLATEX{$opt}.to_lowercase() != $bcfscopeopt->{datatype}.to_lowercase()) {
          biber_warn("Warning: Datatype for biblatex option '$opt' has conflicting values, probably at different scopes. This is not supported.");
        }
        else {
          $CONFIG_OPTTYPE_BIBLATEX{$opt} = bcfscopeopt.datatype.to_lowercase();
        }
      }
    }
    // Now we have the per-namelist options, make the accessors for them in the Names package
    for nso in CONFIG_OPT_SCOPE_BIBLATEX.iter_by_right("NAMELIST") {
      crate::Entry::Names->follow_best_practice;
      crate::Entry::Names->mk_accessors($nso);
    }
    // Now we have the per-name options, make the accessors for them in the Name package
    for no in CONFIG_OPT_SCOPE_BIBLATEX.iter_by_right("NAME") {
      crate::Entry::Name->follow_best_practice;
      crate::Entry::Name->mk_accessors($no);
    }

    // OPTIONS
    for bcfopts in ($bcfxml->{options}->@*) {

      // Biber options
      if ($bcfopts->{component} == "biber") {

        // Global options
        if ($bcfopts->{type} == "global") {
          for bcfopt in ($bcfopts->{option}->@*) {
            // unless already explicitly set from cmdline/config file
            if !(crate::Config->isexplicitoption($bcfopt->{key}{content})) {
              if ($bcfopt->{type} == "singlevalued") {
                crate::Config->setoption($bcfopt->{key}{content}, $bcfopt->{value}[0]{content});
              }
              else if ($bcfopt->{type} == "multivalued") {
                crate::Config->setoption($bcfopt->{key}{content},
                  [ bcfopt.value.iter().sorted_by_key(|a| a.order).map(|a| a.content) ]);
              }
            }
          }
        }
      }

      // BibLaTeX options
      if ($bcfopts->{component} == "biblatex") {

        // Global options
        if ($bcfopts->{type} == "global") {
          for bcfopt in ($bcfopts->{option}->@*) {
            if ($bcfopt->{type} == "singlevalued") {
              crate::Config->setblxoption(None, $bcfopt->{key}{content}, $bcfopt->{value}[0]{content});
            }
            else if ($bcfopt->{type} == "multivalued") {
              // sort on order attribute and then remove it
              crate::Config->setblxoption(None, $bcfopt->{key}{content},
                [ map {delete($_->{order}); $_} sort {$a->{order} <=> $b->{order}} $bcfopt->{value}->@* ]);
            }
          }
        }

        // Entrytype options
        else {
          let $entrytype = $bcfopts->{type};
          for bcfopt in ($bcfopts->{option}->@*) {
            if ($bcfopt->{type} == "singlevalued") {
              crate::Config->setblxoption(None, $bcfopt->{key}{content}, $bcfopt->{value}[0]{content}, "ENTRYTYPE", $entrytype);
            }
            else if ($bcfopt->{type} == "multivalued") {
              // sort on order attribute and then remove it
              crate::Config->setblxoption(None, $bcfopt->{key}{content},
                [ map {delete($_->{order}); $_} sort {$a->{order} <=> $b->{order}} $bcfopt->{value}->@* ],
                "ENTRYTYPE",
                $entrytype);
            }
          }
        }
      }
    }

    // DATAFIELD SETS
    // Since we have to use the datamodel to resolve some members, just record the settings
    // here for processing after the datamodel is parsed
    for s in ($bcfxml->{datafieldset}->@*) {
      let $name = s.name.to_lowercase();
      for m in (s.member->@*) {
        if (let $field = $m->{field}[0]) {// "field" has forcearray for other things
          DATAFIELD_SETS{$name}.push(DataField::String(field));
        }
        else {
            DATAFIELD_SETS{$name}.push(DataField::Field {
              fieldtype: $m->{fieldtype},
              datatype: $m->{datatype}
            });
        }
      }
    }

    // DATASOURCE MAPPING
    // This is special as it's both a biblatex option and a biber option
    // We merge into the biber option
    // In biblatex you can set driver mappings but not in biber
    // Order of application of maps is decided by the level and within "user" level,
    // which can come from two places (biber.conf and \DeclareSourcemap), order is
    // \DeclareSourcemap, then biber.conf
    if (exists($bcfxml->{sourcemap})) {
      // User maps are set in config file
      if (let $usms = crate::Config->getoption("sourcemap")) {
        // Force "user" level for the maps
        for m in usms {
          m.level = "user";
        }

        // Merge any user maps from the document set by \DeclareSourcemap into user
        // maps set in the biber config file. These document user maps take precedence so go
        // at the front of any other user maps
        // Are there any doc maps to merge?
        if (let @docmaps = grep {$_->{level} == "user"} $bcfxml->{sourcemap}{maps}->@*) {
          // If so, get a reference to the maps in the config map and prepend all
          // of the doc maps to it. Must also deref the doc maps map element to make
          // sure that they collapse nicely
          let $configmaps = first {$_->{level} == "user"} $usms->@*;
          configmaps.map.splice(0..0, docmaps.map(|d| d.map));
        }

        // Merge the driver/style maps with the user maps from the config file
        if (let @m = grep {$_->{level} == "driver" ||
                          $_->{level} == "style"} $bcfxml->{sourcemap}{maps}->@* ) {
          crate::Config->setoption("sourcemap", [$usms->@*, @m]);
        }
        else { // no driver defaults, just override the config file user map settings
          crate::Config->setoption("sourcemap", $bcfxml->{sourcemap}{maps});
        }
      }
      else { // just write the option as there are no config file settings at all
        crate::Config->setoption("sourcemap", $bcfxml->{sourcemap}{maps});
      }
    }

    // LABELALPHA NAME TEMPLATE
    let mut lants = HashMap::new();
    for t in ($bcfxml->{labelalphanametemplate}->@*) {
      let mut lant = Vec::new();
      for np in t.namepart.iter().sorted_by_key(|a| a.order) {
        lant.push({namepart           => np.content,
                    use                => np.use,
                    pre                => np.pre,
                    substring_compound => np.substring_compound,
                    substring_side     => np.substring_side,
                    substring_width    => np.substring_width
                  });
      }
      lants.insert(t.name, lant);
    }
    crate::Config->setblxoption(None, "labelalphanametemplate", lants);

    // LABELALPHA TEMPLATE
    for t in ($bcfxml->{labelalphatemplate}->@*) {
      let latype = t.type;
      if latype == "global" {
        crate::Config->setblxoption(None, "labelalphatemplate", t);
      }
      else {
        crate::Config->setblxoption(None, "labelalphatemplate", t, "ENTRYTYPE", latype);
      }
    }

    // EXTRADATE specification
    let mut ed = Vec::new();
    for scope in ($bcfxml->{extradatespec}->{scope}->@*) {
      let mut fields = Vec::new();
      for field in scope.field.iter().sorted_by_key(|a| a.order) {
        fields.push(field.content);
      }
      ed.push(fields);
    }
    crate::Config->setblxoption(None, "extradatespec", ed);

    // INHERITANCE schemes for crossreferences (always global)
    crate::Config->setblxoption(None, "inheritance", $bcfxml->{inheritance});

    // NOINIT
    // Make the data structure look like the biber config file structure
    // "value" is forced to arrays for other elements so we extract
    // the first element here as they will always be only length=1
    let mut noinit = Vec::new();
    for ni in ($bcfxml->{noinits}{noinit}->@*) {
      noinit.push({ value => $ni->{value}[0]});
    }
    // There is a default so don't set this option if nothing is in the .bcf
    if !noinit.is_empty() {
      crate::Config->setoption("noinit", noinit);
    }

    // NOLABEL
    // Make the data structure look like the biber config file structure
    // "value" is forced to arrays for other elements so we extract
    // the first element here as they will always be only length=1
    let mut nolabel = Vec::new();
    for nl in ($bcfxml->{nolabels}{nolabel}->@*) {
      nolabel.push({ value => $nl->{value}[0]});
    }
    // There is a default so don't set this option if nothing is in the .bcf
    if !nolabel.is_empty() {
      crate::Config->setoption("nolabel", nolabel);
    }

    // NOLABELWIDTHCOUNT
    // Make the data structure look like the biber config file structure
    // "value" is forced to arrays for other elements so we extract
    // the first element here as they will always be only length=1
    let mut nolabelwidthcount = Vec::new();
    for nlwc in ($bcfxml->{nolabelwidthcounts}{nolabelwidthcount}->@*) {
      nolabelwidthcount.push({ value => $nlwc->{value}[0]});
    }
    // There is a default so don't set this option if nothing is in the .bcf
    if !nolabelwidthcount.is_empty() {
      crate::Config->setoption("nolabelwidthcount", nolabelwidthcount);
    }

    // NOSORT
    // Make the data structure look like the biber config file structure
    // "field" and "value" are forced to arrays for other elements so we extract
    // the first element here as they will always be only length=1
    let mut nosort = Vec::new();
    for ns in ($bcfxml->{nosorts}{nosort}->@*) {
      nosort.push({name => $ns->{field}[0], value => $ns->{value}[0]});
    }
    // There is a default so don't set this option if nothing is in the .bcf
    if !nosort.is_empty() {
      crate::Config->setoption("nosort", nosort);
    }

    // NONAMESTRING
    // Make the data structure look like the biber config file structure
    // "field" and "value" are forced to arrays for other elements so we extract
    // the first element here as they will always be only length=1
    let mut nonamestring = Vec::new();
    for ns in ($bcfxml->{nonamestrings}{nonamestring}->@*) {
      nonamestring.push({name => $ns->{field}[0], value => $ns->{value}[0]});
    }
    if !nonamestring.is_empty() {
      crate::Config->setoption("nonamestring", nonamestring);
    }

    // UNIQUENAME TEMPLATE
    let mut unts = HashMap::new();
    let mut checkbase = false;
    for unt in ($bcfxml->{uniquenametemplate}->@*) {
      let mut untval = Vec::new();
      for np in unt.namepart.iter().sorted_by_key(|a| a.order) {
        if np.base {
          checkbase = true;
        }
        untval.push({namepart        => np.content,
                      use             => np.use,
                      disambiguation  => np.disambiguation,
                      base            => np.base
                    });
      }
      unts.insert(unt.name, untval);
    }

    // Check to make sure we have a base to disambiguate from. If not, we can get infinite loops
    // in the disambiguation code
    if !checkbase {
      biber_error("The uniquenametemplate must contain at least one "base" part otherwise name disambiguation is impossible");
    }
    
    crate::Config->setblxoption(None, "uniquenametemplate", unts);

    // SORTING NAME KEY
    // Use the order attributes to make sure things are in right order and create a data structure
    // we can use later
    let $snss;
    for sns in ($bcfxml->{sortingnamekeytemplate}->@*) {
      let mut snkps = Vec::new();
      for snkp in sns.keypart.iter().sorted_by_key(|a| a.order) {
        let mut snps = Vec::new();
        for snp in snkp.part.iter().sorted_by_key(|a| a.order) {
          let np;
          if snp.type == "namepart" {
            np = { type => "namepart", value => snp.content };
            if let Some(val) = snp.use {
              np.use = val;
            }
            if let Some(val) = snp.inits {
              np.inits = val;
            }
          } else if snp.type == "literal" {
            np = { type => "literal", value => snp.content };
          }
          snps.push(np);
        }
        snkps.push(snps);
      }
      $snss->{sns.name}.visibility = sns.visibility;
      $snss->{sns.name}.template = snkps;
    }
    crate::Config->setblxoption(None, "sortingnamekeytemplate", $snss);

    // SORTING

    // transliterations
    for tr in ($bcfxml->{transliteration}->@*) {
      if tr.entrytype[0] == '*' { // already array forced for another option
        crate::Config->setblxoption(None, "translit", tr.translit);
      } else { // per_entrytype
        crate::Config->setblxoption(None, "translit", tr.translit, "ENTRYTYPE", tr.entrytype[0]);
      }
    }

    // sorting excludes
    for sex in ($bcfxml->{sortexclusion}->@*) {
      let mut excludes = HashSet::new();
      for ex in sex.exclusion {
        excludes.insert(ex.content);
      }
      crate::Config->setblxoption(None, "sortexclusion", excludes, "ENTRYTYPE", sex.type);
    }

    // sorting includes
    for sin in ($bcfxml->{sortinclusion}->@*) {
      let mut includes = HashSet::new();
      for in in sin.inclusion {
        includes.insert(in.content);
      }
      crate::Config->setblxoption(None, "sortinclusion", includes, "ENTRYTYPE", sin.type);
    }

    // presort defaults
    for presort in ($bcfxml->{presort}->@*) {
      if presort.type.is_none() { // Global presort default
        crate::Config->setblxoption(None, "presort", presort.content);
      } else { // Per-type default
        crate::Config->setblxoption(None, "presort", presort.content, "ENTRYTYPE", presort.type);
      }
    }

    let mut sortingtemplates = HashMap::new();
    for ss in ($bcfxml->{sortingtemplate}->@*) {
      sortingtemplates.insert(ss.name, _parse_sort(ss));
    }
    crate::Config->setblxoption(None, "sortingtemplate", sortingtemplates);

    // DATAMODEL schema (always global and is an array to accomodate multiple
    // datamodels in tool mode)

    // Because in tests, parse_ctrlfile() is called several times so we need to sanitise this here
    crate::Config->setblxoption(None, "datamodel", []);
    crate::Config->addtoblxoption(None, "datamodel", $bcfxml->{datamodel});

    // SECTIONS
    // This is also where we set data files as these are associated with a bib section

    // Data sources
    let %bibdatasources = ();
    for data in ($bcfxml->{bibdata}->@*) {
      for datasource in ($data->{datasource}->@*) {
        if !bibdatasources{$data->{section}[0]}.iter().any(|d|
          d.typ == $datasource->{type} &&
          d.datatype == $datasource->{datatype} &&
          d.name == $datasource->{content}
        ) {
          bibdatasources{$data->{section}[0]}.push(DataSource{ type     => $datasource->{type},
            name: $datasource->{content},
            datatype: $datasource->{datatype},
            encoding: $datasource->{encoding}.unwrap_or(crate::Config->getoption("input_encoding")),
            glob: $datasource->{glob}.unwrap_or(crate::Config->getoption("glob_datasources"),
          });
        }
      }
    }

    // Be friendly to latexmk etc.
    if !(%bibdatasources) {
      biber_warn("No data sources defined!");
      exit EXIT_OK;
    }

    let key_flag = false;
    let bib_sections = crate::Sections::new();

  SECTION: for section in ($bcfxml->{section}->@*) {
      let secnum = section.number();
      // Can be multiple section 0 entries and so re-use that section object if it exists
      let existing_section = bib_sections.get_section(secnum);
      let $bib_section = if existing_section {
        existing_section
      } else {
        crate::Section::new(secnum)
      };

      // Set the data files for the section unless we've already done so
      // (for example, for multiple section 0 entries)
      if bib_section.get_datasources().skip_empty().is_none() {
        bib_section.set_datasources(bibdatasources{secnum});
      }

      let @prekeys = ();
      let @keys = ();
      // Pre-process to deal with situation where key is both \nocite'd and \cited
      // \cite'd takes priority
      for keyc in ($section->{citekey}->@*) {
        let $key = NFD($keyc->{content}); // Key is already UTF-8 - it comes from UTF-8 XML

        if ($keyc->{nocite}) {// \nocite'd
          // Don't add if there is an identical key without nocite since \cite takes precedence
          if !prekeys.iter().any(|k| key == NFD(k->{content})) {
            prekeys.push(keyc);
          }
        }
        else {// \cite'd
          // If there is already a nocite of this key, remove the nocite attribute and don't add
          if prekeys.iter().any(|k| (key == NFD(k->{content})) && k->{nocite}) {
            @prekeys = map {delete($_->{nocite}) if key == NFD($_->{content});$_} @prekeys;
          }
          else {
            prekeys.push(keyc);
          }
        }
      }

      // Loop over all section keys
      for keyc in &prekeys {
        let key = NFD(keyc.content); // Key is already UTF-8 - it comes from UTF-8 XML
        // Stop reading citekeys if we encounter "*" as a citation as this means
        // "all keys"
        if key == '*' {
          bib_section.set_allkeys(true);
          crate::Config->set_keyorder(secnum, key, keyc.order);
          if (keyc.nocite) {
            bib_section.set_allkeys_nocite(1);
          }
          key_flag = true; // There is at least one key, used for error reporting below
        }
        else if !bib_section.get_seenkey(key) {
          // Dynamic set definition
          // Save dynamic key -> member keys mapping for set entry auto creation later
          // We still need to find these even if allkeys is set
          match keyc.type.as_ref() {
            Some("set") => {
              bib_section.set_dynamic_set(key, regex!(r"\s*,\s*").split(&keyc.members));
              keys.push(key);
              key_flag = true; // There is at least one key, used for error reporting below
            }
            _ => {
              // Track cite/nocite - needed for sourcemapping logic
              if (keyc.nocite) {
                bib_section.add_nocite(key);
              }
              else {
                bib_section.add_cite(key);
              }
              // Set order information - there is no order on dynamic key defs above
              // as they are a definition, not a cite
              crate::config::set_keyorder(secnum, key, keyc.order);
              // order of keys which have the same order so we can track order in \cite{a,b,c}
              if (keyc.intorder) {
                Biber::Config->set_internal_keyorder(secnum, key, keyc.intorder);
              }
              keys.push(key);
              key_flag = true; // There is at least one key, used for error reporting below
            }
          }
        }
        bib_section.incr_seenkey(key); // always increment
      }

      // Get citecounts if present
      for keycount in ($section->{citekeycount}->@*) {
        let $key = NFD(keycount.content); // Key is already UTF-8 - it comes from UTF-8 XML
        bib_section.set_citecount(key, keycount.count);
      }

      if bib_section.is_allkeys()) {
        // Normalise - when allkeys is true don't need citekeys - just in case someone
        // lists "*" and also some other citekeys
        bib_section.del_citekeys();
        info!("Using all citekeys in bib section {secnum}");
      }
      else {
        info!("Found {} citekeys in bib section {secnum}", keys.len());
      }

      if !bib_section.is_allkeys() {
          debug!("The citekeys for section {secnum} are: {}\n", keys.join(", "));
      }

      if !bib_section.is_allkeys() {
        bib_section.add_citekeys(keys);
      }
      bib_sections.add_section(bib_section);
    }

    // Add the crate::Sections object to the Biber object
    $self->{sections} = $bib_sections;

    // Read datalists
    let datalists = crate::DataLists::new();

    for list in ($bcfxml->{datalist}->@*) {
      let $ltype  = $list->{type};
      let $lstn = $list->{sortingtemplatename};
      let $lsnksn = $list->{sortingnamekeytemplatename};
      let $luntn = $list->{uniquenametemplatename};
      let $llantn = $list->{labelalphanametemplatename};
      let $lpn = $list->{labelprefix};
      let $lname = $list->{name};

      let $lsection = $list->{section}[0]; // because "section" needs to be a list elsewhere in XML
      if ($datalists->get_list(section                    => $lsection,
                              name                       => $lname,
                              type                       => $ltype,
                              sortingtemplatename        => $lstn,
                              sortingnamekeytemplatename => $lsnksn,
                              labelprefix                => $lpn,
                              uniquenametemplatename     => $luntn,
                              labelalphanametemplatename => $llantn)) {
          debug!("Section datalist '{}' of type '{}' with sortingtemplate '{}', sortingnamekeytemplatename '{}', labelprefix '{}', uniquenametemplate '{}' and labelalphanametemplate '{}' is repeated for section {} - ignoring", lname, ltype, lstn, lsnksn, lpn, luntn, llantn, lsection);
          continue;
      }

      let datalist = crate::DataList::new(
        lsection,
        lname || format!("{lstn}/{lsnksn}/{lpn}/{luntn}/{llantn}"), // default to ss+snkss+pn+untn+lantn
        ltype.to_lowercase() || "entry", // lists are entry lists by default
        lstn,
        lsnksn,
        luntn,
        llantn,
        lpn,
      );
      for filter in ($list->{filter}->@*) {
        datalist.add_filter(Filter {
          type: FilterType::from_str(filter->{type}).unwrap(),
          value: filter->{content}
        });
      }
      // disjunctive filters are an array ref of filter hashes
      for orfilter in ($list->{filteror}->@*) {
        let mut orfilts = Vec::new();
        for filter in ($orfilter->{filter}->@*) {
          orfilts.push(Filter {
            type: FilterType::from_str(filter->{type}).unwrap(),
            value: filter->{content}
          });
        }
        if !orfilts.is_empty() {
          datalist.add_filter(&orfilts);
        }
      }

      // Collator for determining primary weight hash for sortinit
      // Here as it varies only with the locale and that doesn't vary between entries in a list
      // Potentially, the locale could be different for the first field in the sort spec in which
      // case that might give wrong results but this is highly unlikely as it is only used to
      // determine sortinithash in DataList.pm and that only changes \bibinitsep in biblatex.
      datalist.set_sortinit_collator(Unicode::Collate::Locale->new(locale => crate::Config->getblxoption(None, "sortingtemplate")->{datalist.get_sortingtemplatename()}->{locale}, level => 1));

        debug!("Adding datalist of type '{}' with sortingtemplate '{}', sortingnamekeytemplatename '{}', labelprefix '{}', uniquenametemplate '{}', labelalphanametemplate '{}' and name '{}' for section {}", ltype, lstn, lsnksn, lpn, luntn, llantn, lname, lsection);
      datalists.add_list(datalist);
    }

    // Check to make sure that each section has an entry datalist for global sorting
    // We have to make sure in case sortcites is used which uses the global order.
    for section in ($bcfxml->{section}->@*) {
      let $globalss = crate::Config->getblxoption(None, "sortingtemplatename");
      let secnum = section.number();

      if !($datalists->get_lists_by_attrs(section                    => $secnum,
                                            type                       => "entry",
                                            sortingtemplatename        => $globalss,
                                            sortingnamekeytemplatename => "global",
                                            uniquenametemplatename     => "global",
                                            labelalphanametemplatename => "global",
                                            labelprefix                => "",
                                            name                       => format!("{globalss}/global//global/global"))) {
        let datalist = crate::DataList::new(
          secnum,
          &format!("{globalss}/global//global/global"),
          "entry",
          globalss,
          "global",
          "global",
          "global",
          "",
        );
        datalists.add_list(datalist);
        // See comment above

        datalist.set_sortinit_collator(Unicode::Collate::Locale->new(locale => crate::Config->getblxoption(None, "sortingtemplate")->{datalist.get_sortingtemplatename()}->{locale}, level => 1));
      }
    }

    // Add the crate::DataLists object to the Biber object
    self.datalists = datalists;

    // Warn if there are no citations in any section
    if !key_flag {
      biber_warn("The file '$ctrl_file_path' does not contain any citations!");
    }

    // Normalise any UTF-8 encoding string immediately to exactly what we want
    // We want the strict perl utf8 "UTF-8"
    normalise_utf8();

    // bibtex output when not in tool mode, is essentially entering tool mode but
    // without allkeys. We are not in tool mode if we are here. We fake tool mode
    // and then add a special section which contains all cited keys from all sections
    // No reference resolution for bibtex output and always include all cross/xrefs
    // otherwise the output won't be a standalone .bib file
    if (crate::Config->getoption("output_format") == "bibtex") {
      crate::Config->setoption("tool", 1);
      crate::Config->setoption("mincrossrefs", 1);
      crate::Config->setoption("minxrefs", 1);

      let bib_section = crate::Section::new(99999);

      for section in &self.sections().get_sections() {
        if section.is_allkeys() {
          bib_section.set_allkeys(true);
        }
        else {
          bib_section.add_citekeys(section.get_citekeys());
        }
        for ds in section.get_datasources() {
          bib_section.add_datasource(ds);
        }
      }

      self.sections().add_section(bib_section);

      let datalist = crate::DataList::new(
        99999,
        crate::Config->getblxoption(None, "sortingtemplatename") . "/global//global/global",
        "entry",
        crate::Config->getblxoption(None, "sortingtemplatename"),
        "global",
        "global",
        "global",
        "",
      );
        debug!("Adding "entry" list "none" for pseudo-section 99999");
      self.datalists.add_list(datalist);
    }

    return;
  }


  /// Place to put misc pre-processing things needed later
  fn process_setup(&mut self) {
    // If this is tool mode and therefore there is a 99999 section, delete all other sections
    // This is because bibtex output not in real tool mode retains sections from the .bcf
    // which are not needed and cause unnecessary dual-processing of entries since everything
    // is already in the 99999 section anyway
    for section in &self.sections().get_sections() {
      if (crate::Config->getoption("output_format") == "bibtex") {
        if section.number() != 99999 {
          self.sections().delete_section(section);
        }
      }
    }

    // Make sure there is a default entry list with global sorting for each refsection
    // Needed in case someone cites entries which are included in no
    // bibliography as this results in no entry list in the .bcf
    for section in &self.sections().get_sections() {
      let secnum = section.number();

      if !(self.datalists.has_lists_of_type_for_section(secnum, "entry")) {
        let datalist = crate::DataList::new(
          secnum,
          &format!("{}/global//global/global", crate::Config->getblxoption(None, "sortingtemplatename")),
          "entry",
          crate::Config->getblxoption(None, "sortingtemplatename"),
          "global",
          "global",
          "global",
          "",
        );
        self.datalists.add_list(datalist);
        // See comment for same call in .bcf instantiation of datalists
        datalist.set_sortinit_collator(Unicode::Collate::Locale->new(locale => crate::Config->getblxoption(None, "sortingtemplate")->{datalist.get_sortingtemplatename()}->{locale}, level => 1));
      }
    }

    // Break data model information up into more processing-friendly formats
    // for use in verification checks later
    // This has to be here as opposed to in parse_ctrlfile() so that it can pick
    // up user config dm settings
    crate::Config->set_dm(crate::DataModel->new(crate::Config->getblxoption(None, "datamodel")));

    // Now resolve any datafield sets from the .bcf
    _resolve_datafieldsets();

    // Force output_safechars flag if output to ASCII and input_encoding is not ASCII
    if regex_is_match!(r"(?:x-)?ascii"xmsi,  crate::Config->getoption("output_encoding")) &&
    !regex_is_match!(r"(?:x-)?ascii"xmsi, crate::Config->getoption("input_encoding")) {
      crate::Config->setoption("output_safechars", 1);
    }
  }

  /// Place to put misc pre-processing things needed later for tool mode
  fn process_setup_tool(&self) {
    crate::Config->set_dm(crate::DataModel->new(crate::Config->getblxoption(None, "datamodel")));

    // Now resolve any datafield sets from the .bcf
    _resolve_datafieldsets();

    // Force output_safechars flag if output to ASCII and input_encoding is not ASCII
    if regex_is_match!(r"(?:x-)?ascii"xmsi, crate::Config->getoption("output_encoding")) &&
    !regex_is_match!(r"(?:x-)?ascii"xmsi, crate::Config->getoption("input_encoding")) {
      crate::Config->setoption("output_safechars", 1);
    }
  }

  // datafield sets need to be resolved after the datamodel is parsed
  fn _resolve_datafieldsets {
    let dm = crate::config::get_dm();
    for value in DATAFIELD_SETS.values_mut() {
      let fs = Vec::new();
      for m in ($value->@*) {
        if (ref $m == "HASH") {
          match (m.fieldtype, m.datatype) {
            (Some(fieldtype), Some(datatype)) => {
              fs.extend(dm.get_fields_of_type(fieldtype, &[datatype], None));
            }
            (Some(fieldtype), None) => {
              fs.extend(dm.get_fields_of_fieldtype(fieldtype));
            }
            (None, Some(datatype)) => {
              fs.extend(dm.get_fields_of_datatype(&[datatype]));
            }
            _ => {}
          }
        }
        else {
          fs.push(m);
        }
      }
      *value = fs;
    }
  }


  /// Resolve aliases in xref/crossref/xdata which take keys as values to their real keys
  ///
  /// We use set_datafield as we are overriding the alias in the datasource
  fn resolve_alias_refs(&mut self) {
    let secnum = self.get_current_section();
    let section = self.sections().get_section(secnum);
    let $dm = crate::config::get_dm();


    // Don't resolve alias refs in tool mode unless told to
    if (crate::Config->getoption("tool") &&
        not (crate::Config->getoption("output_resolve_crossrefs") ||
            crate::Config->getoption("output_resolve_xdata"))) {
      return;
    }

    for citekey in section.get_citekeys() {
      let be = section.bibentry(citekey);

      // XREF
      if (let $refkey = be.get_field("xref")) {
        if (let $realkey = section.get_citekey_alias(refkey)) {
          be.set_datafield("xref", realkey);
        }
      }
      // CROSSREF
      if (let $refkey = be.get_field("crossref")) {
        if (let $realkey = section.get_citekey_alias(refkey)) {
          be.set_datafield("crossref", realkey);
        }
      }
      // XDATA
      let xdata = be.get_xdata_refs()
      if !xdata.is_empty() {
        let $resolved_keys;
        for xdataref in xdata {
          if xdataref.xdatafield.is_none() { // XDATA ref to whole entry
            for refkey in xdataref.xdataentries { // whole entry XDATA can be xsv
              let refkey = section.get_citekey_alias(refkey).unwrap_or(refkey);
              resolved_keys.push(refkey);
            }
            $xdataref->{xdataentries} = $resolved_keys;
          }
          else { // granular XDATA ref - only one entry key
            let refkey = $xdataref->{xdataentries}->[0];
            let refkey = section.get_citekey_alias(refkey).unwrap_or(refkey);
            $xdataref->{xdataentries} = vec![refkey];
          }
        }
      }
    }
  }

  /// Remove citekey aliases from citekeys as they don't point to real
  /// entries.
  fn process_citekey_aliases(&mut self) {
    let secnum = self.get_current_section();
    let section = self.sections().get_section(secnum);
    for citekey in section.get_citekeys() {
      if (let $a = section.get_citekey_alias(citekey)) {
          debug!("Pruning citekey alias '{}' from citekeys", citekey);
        section.del_citekey(citekey);
      }
    }
  }

  /// This instantiates any dynamic entries so that they are available
  /// for processing later on. This has to be done before most all other
  /// processing so that when we call `section.bibentry(key)`, as we
  /// do many times in the code, we don't die because there is a key but
  /// no Entry object.
  fn instantiate_dynamic(&mut self) {
    let secnum = self.get_current_section();
    let section = self.sections().get_section(secnum);

      debug!("Creating dynamic entries (sets/related) for section {}", secnum);

    // Instantiate any dynamic set entries before we do anything else
    for dset in section.dynamic_set_keys() {
      let @members = section.get_dynamic_set(dset);

      // Resolve any aliases in the members
      let mur realmems = Vec::new();
      for mem in (@members) {
        realmems.push(section.get_citekey_alias(mem).unwrap_or(mem));
      }
      @members = @realmems;
      section.set_dynamic_set(dset, realmems.iter().map(|s| s.as_str()));

      let be = crate::Entry::new();
      be.set_field("entrytype", "set");
      be.set_field("entryset", [ @members ]);
      be.set_field("citekey", dset);
      be.set_field("datatype", "dynamic");
      section.bibentries().add_entry(dset, be);
        debug!("Created dynamic set entry '{}' in section {}", dset, secnum);

      for m in (@members) {
      // Save graph information if requested
        if (crate::Config->getoption("output_format") == "dot") {
          crate::Config->set_graph("set", dset, m);
        }
        // Instantiate any related entry clones we need from dynamic set members
        section.bibentry(m).relclone();
      }
      // Setting dataonly options for members is handled by process_sets()
    }

    // Instantiate any related entry clones we need from regular entries
    for citekey in section.get_citekeys() {
      section.bibentry(citekey).relclone();
    }

    return;
  }

  /// Resolve xdata
  fn resolve_xdata(self) {
    let secnum = self.get_current_section();
    let section = self.sections().get_section(secnum);

    // Don't resolve xdata in tool mode unless told to
    if (crate::Config->getoption("tool") &&
        not crate::Config->getoption("output_resolve_xdata")) {
      return;
    }

      debug!("Resolving XDATA for section {}", secnum);

    // We are not looping over citekeys here as XDATA entries are not cited.
    // They may have been added to the section as entries, however.
    for be in section.bibentries.entries() {
      // Don't directly resolve XDATA entrytypes - this is done recursively in the Entry method
      // Otherwise, we will die on loops etc. for XDATA entries which are never referenced from
      // any cited entry
      if be.get_field("entrytype") == "xdata" {
        continue;
      }
      {
        let xdata = be.get_xdata_refs();
        if xdata.is_empty() {
          continue;
        }
      }
      be.resolve_xdata(xdata);
    }
  }

  /// Promotes set member to cited status
  fn cite_setmembers(self) {
    let secnum = self.get_current_section();
    let section = self.sections().get_section(secnum);

      debug!("Adding set members to citekeys for section {}", secnum);

    for citekey in section.get_citekeys() {
      let be = section.bibentry(citekey);

      // promote indirectly cited inset set members to fully cited entries
      if be.get_field("entrytype") == "set" && be.get_field("entryset") {
        let inset_keys = be.get_field("entryset");

        // Ignore empty sets (likely this means that they contained only
        // non-existent keys that were removed)
        if inset_keys.is_empty() {
          continue;
        }

        let mut realmems = Vec::new();
        for mem in inset_keys {
          realmems.push(section.get_citekey_alias(mem).unwrap_or(mem));
        }
        let inset_keys = realmems;
        be.set_datafield("entryset", $inset_keys);

        for inset_key in &inset_keys {
            debug!("Adding set member '{}' to the citekeys (section {})", inset_key, secnum);
          section.add_citekeys(inset_key);

          // Save graph information if requested
          if (crate::Config->getoption("output_format") == "dot") {
            crate::Config->set_graph("set", citekey, inset_key);
          }
        }

        // Set parents inherit first child member data so that they get sensible
        // sorting/labelling defaults. Most of these inherited fields will not be output
        // in the .bbl
        be.set_inherit_from(section.bibentry(inset_keys[0]), section);

        // warning for the old pre-Biber way of doing things
        if be.get_field("crossref") {
          biber_warn("Field "crossref" is no longer needed in set entries in Biber - ignoring in entry '$citekey'", be);
          be.del_field("crossref");
        }
      }
    }
  }

  /// This records the set information for use later
  ///
  /// ```
  /// $biber->preprocess_sets
  /// ```
  fn preprocess_sets(self) {
    let secnum = self.get_current_section();
    let section = self.sections().get_section(secnum);

    // Don't preprocess sets in tool mode unless told to
    if (crate::Config->getoption("tool") &&
        !crate::Config->getoption("output_resolve_sets")) {
      return;
    }

      debug!("Recording set information");

    for citekey in section.get_citekeys() {
      let be = section.bibentry(citekey);

      // Record set information
      // It's best to do this in the loop here as every entry needs the information
      // from all other entries in process_sets()
      if be.get_field("entrytype") == "set" {
        let entrysetkeys = be.get_field("entryset");
        if !($entrysetkeys) {
          biber_warn("Set entry '$citekey' has no entryset field, ignoring", $be);
          continue;
        }
        for member in ($entrysetkeys->@*) {
          section.set_set(citekey, member);

          // Instantiate any related entry clones we need from static set members
          section.bibentry(member).relclone();
        }
      }
    }
  }

  /// Ensures that crossrefs/xrefs that are directly cited or cross-referenced
  /// at least mincrossrefs/minxrefs times are included in the bibliography.
  ///
  /// ```
  /// $biber->calculate_interentry
  /// ```
  fn calculate_interentry(self) {
    let secnum = self.get_current_section();
    let section = self.sections().get_section(secnum);

      debug!("Calculating explicit and implicit xref/crossrefs for section {}", secnum);

    for citekey in section.get_citekeys() {
      let be = section.bibentry(citekey);

      // Loop over cited keys and count the cross/xrefs
      // Can't do this when parsing entries as this would count them
      // for potentially uncited children
      if (let $refkey = be.get_field("crossref")) {
          debug!("Incrementing crossrefkey count for entry '{}' via entry '{}'", refkey, citekey);

        // Don't increment if the crossref doesn't exist
        if section.bibentry(refkey) {
          crate::Config->incr_crossrefkey(refkey);
        }
      }

      if (let $refkey = be.get_field("xref")) {
          debug!("Incrementing xrefkey count for entry '{}' via entry '{}'", refkey, citekey);
        crate::Config->incr_xrefkey(refkey);
      }

      // Record xref inheritance for graphing if required
      if (crate::Config->getoption("output_format") == "dot" &&
          let $xref = be.get_field("xref")) {
        crate::Config->set_graph("xref", citekey, xref);
      }
    }

    // We make sure that crossrefs that are directly cited or cross-referenced
    // at least mincrossrefs times are included in the bibliography.
    for k in crate::Config->get_crossrefkeys()->@* {
      // If parent has been crossref'ed more than mincrossref times, upgrade it
      // to cited crossref status and add it to the citekeys list
      if (crate::Config->get_crossrefkey($k) >= crate::Config->getoption("mincrossrefs")) {
          debug!("cross key '{}' is crossref'ed >= mincrossrefs, adding to citekeys", k);
        // Don't add this flag if the entry is also cited directly
        if !section.has_citekey(k) {
          section.bibentry(k).set_field("crossrefsource", 1);
        }
        section.add_citekeys(k);
      }
    }

    // We make sure that xrefs that are directly cited or x-referenced
    // at least minxrefs times are included in the bibliography.
    for k in crate::Config->get_xrefkeys()->@*  {
      // If parent has been xref'ed more than minxref times, upgrade it
      // to cited xref status and add it to the citekeys list
      if (crate::Config->get_xrefkey($k) >= crate::Config->getoption("minxrefs")) {
          debug!("xref key '{}' is xref'ed >= minxrefs, adding to citekeys", k);
        // Don't add this flag if the entry is also cited directly
        if !section.has_citekey(k) {
          section.bibentry(k).set_field("xrefsource", 1);
        }
        section.add_citekeys(k);
      }
    }
  }

  /// Ensures proper inheritance of data from cross-references.
  ///
  /// ```
  /// $biber->process_interentry
  /// ```
  fn process_interentry(self) {
    let secnum = self.get_current_section();
    let section = self.sections().get_section(secnum);

    // Don't resolve crossrefs in tool mode unless told to
    if (crate::Config->getoption("tool") && !crate::Config->getoption("output_resolve_crossrefs")) {
      return;
    }

      debug!("Processing explicit and implicit xref/crossrefs for section {}", secnum);

    // This must come after doing implicit inclusion based on minref/mincrossref
    // otherwise cascading xref->crossref wont' work
    for citekey in section.get_citekeys() {
      let be = section.bibentry(citekey);

      // Do crossref inheritance
      if (let $cr = be.get_field("crossref")) {
        // Skip inheritance if we've already done it
        if crate::Config->get_inheritance("crossref", $cr, be.get_field("citekey")) {
          continue;
        }
        let parent = section.bibentry(cr);
          debug!("Entry {} inheriting fields from parent {}", citekey, cr);
        if !parent {
          biber_warn("Cannot inherit from crossref key '$cr' - does it exist?", be);
        }
        else {
          be.inherit_from(parent);
        }
      }
    }
  }

  /// Validate bib data according to a datamodel
  /// Note that we are validating the internal crate::Entries
  /// after they have been created from the datasources so this is
  /// datasource neutral, as it should be. It is here to enforce
  /// adherence to what biblatex expects.
  fn validate_datamodel(self) {
    let secnum = self.get_current_section();
    let section = self.sections().get_section(secnum);
    let $dm = crate::config::get_dm();

    if (crate::Config->getoption("validate_datamodel")) {
      info!("Datamodel validation starting");
      let $dmwe = if crate::Config->getoption("dieondatamodel") {
         \&biber_error
      } else {
         \&biber_warn
      };
      for citekey in section.get_citekeys() {
        let be = section.bibentry(citekey);
        let citekey = be.get_field("citekey");
        let mut et = be.get_field("entrytype");
        let ds = section.get_keytods(citekey);

        // default entrytype to MISC type if not a known type
        if !(dm.is_entrytype(et)) {
          dmwe(format!("Datamodel: Entry '{citekey}' ({ds}): Invalid entry type '{}' - defaulting to 'misc'", be.get_field("entrytype")), be);
          be.set_field("entrytype", "misc");
          et = "misc";           // reset this too
        }

        // Are all fields valid fields?
        // Each field must be:
        // * Valid because it's allowed for "ALL" entrytypes OR
        // * Valid field for the specific entrytype OR
        // * Valid because entrytype allows "ALL" fields
        if !(et == "xdata" || et == "set") { // XDATA/SET are generic containers for any field
          for ef in (be.datafields) {
            if !(dm.is_field_for_entrytype(et, ef)) {
              dmwe(format!("Datamodel: Entry '{citekey}' ({ds}): Invalid field '{ef}' for entrytype '{et}'"), be);
            }
          }
        }

        // Mandatory constraints
        for warning in (dm.check_mandatory_constraints(be)) {
          dmwe(warning, be);
        }

        // Conditional constraints
        for warning in (dm.check_conditional_constraints(be)) {
          dmwe(warning, be);
        }

        // Datamodel datatypes
        // This is a check on the datatypes of all fields in the datamodel
        for warning in (dm.check_datatypes(be)) {
          dmwe(warning, be);
        }

        // Data constraints
        for warning in (dm.check_data_constraints(be)) {
          dmwe(warning, be);
        }
      }
      info!("Datamodel validation complete");
    }
  }

  /// Generate name strings and disambiguation schema. Has to be in the context
  /// of a data list (reference context) because uniquenametemplate can be specified
  /// per-list/context
  fn process_namedis(&self, citekey: &str, dlist: &DataList) -> HashMap<(Id, Id), Unknown> {
    let secnum = self.get_current_section();
    let section = self.sections().get_section(secnum);
    let dmh = crate::config::get_dm_helpers();
      debug!("Processing names in entries in section {} to generate disambiguation data", secnum);
    // Use nameuniqueness template to construct uniqueness strings
    let mut untname = dlist.get_uniquenametemplatename();

    let be = section.bibentry(citekey);
    let bee = be.get_field("entrytype");

    let mut un = crate::Config->getblxoption(secnum, "uniquename", bee, citekey);
    let mut ul = crate::Config->getblxoption(secnum, "uniquelist", bee, citekey);

    // Can be per-entry
    untname = crate::Config->getblxoption(secnum, "uniquenametemplatename", None, citekey).or(untname);

    // Instead of setting this directly in here, we save the data and pass it out as we need
    // to use this method to get data without setting it in the list object (in uniqueprimaryauthor())
    let mut namedis = HashMap;

  'MAIN:  for pn in &dmh.namelistsall {
      let nl = be.get_field(pn);
      if !nl {
        continue;
      }
      let nlid = nl.get_id();

      // per-namelist uniquenametemplatename
      untname = nl.get_uniquenametemplatename().or(untname);

      // per-namelist uniquelist
      if let Spme(ulist) = nl.get_uniquelist() {
        ul = ulist;
      }

      // per-namelist uniquename
      un = nl.get_uniquename().or(un);

      for n in nl.names() {
        let nid = n.get_id();

        let namestring = String::new();
        let namestrings = Vec::new();
        let $namedisschema = [];

        // per-name uniquenametemplatename
        untname = n.get_uniquenametemplatename().or(untname);

        // Die if no uniquenametemplate found as this results in an infinite loop
        // in the disambiguation code
        if !(crate::Config->getblxoption(None, "uniquenametemplate")->{untname}) {
          biber_error("No uniquenametemplate called '$untname' found, cannot continue.");
        }

        // per-name uniquename
        un = n.get_uniquename().or(un);

        let $nameun = $un;

        // First construct base part ...
        let mut base = String::new(); // Might not be any base parts at all so make sure it's not undefined
        let mut baseparts = Vec::new();

        for np in (crate::Config->getblxoption(None, "uniquenametemplate")->{untname}->@*) {
          if !$np->{base} {
            continue;
          }
          let $npn = $np->{namepart};

          if (let $p = $n->get_namepart($npn)) {
            if ($np->{use}) {     // only ever defined as 1
              let $method = format!("get_use{npn}");
              let $useok = crate::Config->getblxoption(secnum, format!("use{npn}"), bee, citekey);
              // Override with per-namelist setting - only for extended name format
              if let Some(val) = nl->$method {
                $useok = val;
              }
              // Override with per-name setting - only for extended name format
              if let Some(val) = n->$method {
                $useok = val;
              }
              if !$useok {
                continue;
              }
            }
            base.push_str(p);
            baseparts.push(npn);
          }
        }

        namestring.push_str(&base);
        namestrings.push(base);
        if defined($baseparts) {
          push $namedisschema->@*, ["base" => $baseparts];
        }

        // ... then add non-base parts by incrementally adding to the last disambiguation level
        for np in (crate::Config->getblxoption(None, "uniquenametemplate")->{$untname}->@*) {
          if $np->{base} {
            continue;
          }
          if defined($np->{disambiguation}) && ($np->{disambiguation} == "none") {
            continue;
          }

          let $npn = $np->{namepart};

          let $level = $np->{disambiguation}.unwrap_or($UNIQUENAME_CONTEXTS{$un.unwrap_or("false")});
          let lastns = namestrings.last().unwrap();

          let p = n.get_namepart(npn);
          if !p.is_empty() {
            let pi = n.get_namepart_initial(npn);
            if ($np->{use}) {     // only ever defined as 1
              let $method = format!("get_use{npn}");
              let mut useok = crate::Config->getblxoption(secnum, &format!("use{npn}"), bee, citekey);
              // Override with per-namelist setting - only for extended name format
              if (defined($nl->$method)) {
                useok = $nl->$method;
              }
              // Override with per-name setting - only for extended name format
              if (defined($n->$method)) {
                useok = $n->$method;
              }
              if !$useok {
                continue;
              }
            }

            namestring.push_str(p);

            // per-namepart disambiguation level
            // Here we incrementally add disambiguation possibilities to an array and simultaneously
            // record a schema of what each incremental disambiguation is
            if unicase::eq(level, "full") { // only full disambiguation
              namestrings.push(format!("{lastns}{p}"));
              push $namedisschema->@*, [$npn => "fullonly"];
            }
            if unicase::eq(level, "initorfull") { // initials or full disambiguation
              namestrings.push(format!("{lastns}{}", pi.join(""));
              push $namedisschema->@*, [$npn => "init"];
              namestrings.push(format!("{lastns}{p}"));
              push $namedisschema->@*, [$npn => "full"];
            }
            else if unicase::eq(level, "init") { // inits only
              namestrings.push(format!("{lastns}{}", pi.join(""));
              push $namedisschema->@*, [$npn => "init"];
            }
          }
        }

          trace!("namestrings in '{citekey}': {}", namestrings.join(','));

        // namelistul is the option value of the effective uniquelist option at the level
        // of the list in which the name occurs. It's useful to know this where the results
        // of the sub are used
        namedis.insert((nlid, nid), {nameun        => $nameun,
                                  namelistul    => ul,
                                  namestring    => strip_nonamestring($namestring, nl.get_type()),
                                  namestrings   => namestrings.iter().map(|s| strip_nonamestring(s, nl.get_type())).collect::<Vec<_>>(),
                                  namedisschema => $namedisschema});
      }
    }

    namedis
  }

  /// Adds required per-entry options etc. to sets
  fn postprocess_sets(self) {
    let secnum = self.get_current_section();
    let section = self.sections().get_section(secnum);
    for citekey in section.get_citekeys() {

      // process set entries
      self.process_sets(citekey);
    }

    return;
  }

  /// Processing of entries which is not list-specific and which can therefore
  /// insert data directly into entries
  fn process_entries_static(self) {
    let secnum = self.get_current_section();
    let section = self.sections().get_section(secnum);
      debug!("Processing static entry information in section {}", secnum);
    for citekey in section.get_citekeys() {

      // generate nocite information
      self.process_nocite(citekey);

      // generate labelname name
      self.process_labelname(citekey);

      // generate labeldate name
      self.process_labeldate(citekey);

      // generate labeltitle name
      self.process_labeltitle(citekey);

      // generate fullhash
      self.process_fullhash(citekey);

      // push entry-specific presort fields into the presort state
      self.process_presort(citekey);
    }
  }

  /// Main processing operations, to generate metadata and entry information
  /// This method is automatically called by C<prepare>.
  /// Runs prior to uniqueness processing
  fn process_entries_pre(self, dlist: &mut DataList) {
    let secnum = self.get_current_section();
    let section = self.sections().get_section(secnum);
      debug!("Processing entries in section {} (before uniqueness)", secnum);
    for citekey in section.get_citekeys() {

      let be = section.bibentry(citekey);

      // process name disambiguation schemata
      let namedis = self.process_namedis(citekey, dlist);

      for ((nlid, nid), val) in namedis {
        // process_namedis() has to record uniquelist/uniquename as it has access to
        // namelist-scope and name-scope uniquelist/uniquename and makes this visible
        // here so that they can be checked
        // We only don't set name disambiguation data if both uniquelist/uniquename
        // effective options are "false". If either are not false, we need the information
        if (val.nameun == "false" && val.namelistul == "false") {
          continue;
        }
        dlist.set_namedis(nlid, nid, val.namestring, val.namestrings, val.namedisschema);
      }
    }

      debug!("Finished processing entries in section {} (before uniqueness)", secnum);

    return;
  }

  /// More processing operations, to generate things which require uniqueness
  /// information like namehash
  /// Runs after uniqueness processing
  fn process_entries_post(&self, dlist: &mut DataList) {
    let secnum = self.get_current_section();
    let section = self.sections().get_section(secnum);
      debug!("Postprocessing entries in section {} (after uniqueness)", secnum);
    for citekey in section.get_citekeys() {

      // generate labelalpha information
      self.process_labelalpha(citekey, dlist);

      // generate information for tracking extraalpha
      self.process_extraalpha(citekey, dlist);

      // generate information for tracking extradate
      self.process_extradate(citekey, dlist);

      // generate information for tracking extraname
      self.process_extraname(citekey, dlist);

      // generate information for tracking extratitle
      self.process_extratitle(citekey, dlist);

      // generate information for tracking extratitleyear
      self.process_extratitleyear(citekey, dlist);

      // generate information for tracking singletitle, uniquetitle, uniquebaretitle and uniquework
      self.process_workuniqueness(citekey, dlist);

      // generate namehash
      self.process_namehash(citekey, dlist);

      // generate per-name hashes
      self.process_pername_hashes(citekey, dlist);

      // generate information for tracking uniqueprimaryauthor
      self/process_uniqueprimaryauthor(citekey, dlist);

    }

      debug!("Finished processing entries in section {} (after uniqueness)", secnum);

    return;
  }

  /// Final processing operations which depend on all previous processing
  fn process_entries_final(&self, dlist: &mut DataList) {
    let secnum = self.get_current_section();
    let section = self.sections().get_section(secnum);
      debug!("Final processing for entries in section {}", secnum);
    for citekey in section.get_citekeys() {

      // Generate singletitle field if requested
      self.generate_singletitle(citekey, dlist);

      // Generate uniquetitle field if requested
      self.generate_uniquetitle(citekey, dlist);

      // Generate uniquebaretitle field if requested
      self.generate_uniquebaretitle(citekey, dlist);

      // Generate uniquework field if requested
      self.generate_uniquework(citekey, dlist);

      // Generate uniqueprimaryauthor if requested
      self.generate_uniquepa(citekey, dlist);
    }
  }

  /// Track seen primary author base names for generation of uniqueprimaryauthor
  fn process_uniqueprimaryauthor(&self, citekey: &str, dlist: &mut DataList) {
    let secnum = self.get_current_section();
    let section = self.sections().get_section(secnum);
    let be = section.bibentry(citekey);
    let bee = be.get_field("entrytype");

    if let Some(lni) = be.get_labelname_info().skip_empty() {
      if (crate::Config->getblxoption(None, "uniqueprimaryauthor", bee, citekey)) {
        let nl = be.get_field(lni);
          trace!("Creating uniqueprimaryauthor information for '{}'", citekey);

        let namedis = self.process_namedis(citekey, dlist);

        let val = namedis.get((nl.get_id(), nl.nth_name(1).get_id())).unwrap();
        let nds = val.namedisschema;
        let nss = val.namestrings;
        let mut pabase = None;

        for (i, se) in nds.iter().enumerate() {
          if se[0] == "base" {
            pabase = nss[i];
          }
        }

        dlist.set_entryfield(citekey, "seenprimaryauthor", pabase);
        dlist.incr_seenpa(pabase, nl.nth_name(1).get_hash());
      }
    }
  }

  /// Track seen work combination for generation of singletitle, uniquetitle, uniquebaretitle and
  /// uniquework
  fn process_workuniqueness(&self, citekey: &str, dlist: &mut DataList) {
    let secnum = self.get_current_section();
    let section = self.sections().get_section(secnum);
    let be = section.bibentry(citekey);
    let bee = be.get_field("entrytype");

    let identifier;
    let lni = be.get_labelname_info();
    let lti = be.get_labeltitle_info();

    // ignore settings from inheritance data?
    let ignore = crate::Config->get_uniq_ignore(citekey);

    // singletitle
    // Don't generate information for entries with no labelname or labeltitle
    // Use fullhash as this is not a test of uniqueness of only visible information
    if (lni && crate::Config->getblxoption(None, "singletitle", bee, citekey)) {
      identifier = self._getfullhash(citekey, be.get_field(lni));

      // Skip due to ignore settings?
      // Don't count towards singletitle being false if both labelname and labeltitle
      // were inherited
      // Put another way, if both labelname and labeltitle were inherited, singletitle
      // can still be true (in a mvbook for example, which is just a single "work")
      if !((lni && ignore.singletitle.iter().any(|&x| unicase::eq(lni, x))) &&
              (lti && ignore.singletitle.iter().any(|&x| unicase::eq(lti, x)))) {
        dlist.incr_seenname(identifier);
      }
      dlist.set_entryfield(citekey, "seenname", identifier);
    }

    // uniquetitle
    // Don't generate information for entries with no labeltitle
    if (lti && crate::Config->getblxoption(None, "uniquetitle", bee, citekey)) {
      identifier = be.get_field(lti);

      // Skip due to ignore settings?
      if !(ignore.uniquetitle.iter().any(|&x| unicase::eq(lti, x))) {
        dlist.incr_seentitle(identifier);
      }
      dlist.set_entryfield(citekey, "seentitle", identifier);
    }

    // uniquebaretitle
    // Don't generate information for entries with no labeltitle and with labelname
    if (lti && !lni && crate::Config->getblxoption(None, "uniquebaretitle", bee, citekey)) {
      identifier = be.get_field(lti);

      // Skip due to ignore settings?
      if !ignore.uniquebaretitle.iter().any(|&x| unicase::eq(lti, x)) {
        dlist.incr_seenbaretitle(identifier);
      }
      dlist.set_entryfield(citekey, "seenbaretitle", identifier);
    }

    // uniquework
    // Don't generate information for entries with no labelname and labeltitle
    // Should use fullhash this is not a test of uniqueness of only visible information
    if (lni && lti && crate::Config->getblxoption(None, "uniquework", bee, citekey)) {
      identifier = format!("{}{}", self._getfullhash(citekey, be.get_field(lni)), be.get_field(lti));

      // Skip due to ignore settings?
      if !(
        ignore.uniquework.iter().any(|&x| unicase::eq(lni, x))
         &&
         ignore.uniquework.iter().any(|&x| unicase::eq(lti, x))
        ) {
        dlist.incr_seenwork(identifier);
      }
      dlist.set_entryfield(citekey, "seenwork", identifier);
    }
  }

  /// Track labelname/date parts combination for generation of extradate
  fn process_extradate(&self, citekey: &str, dlist: &mut DataList) {
    let secnum = self.get_current_section();
    let section = self.sections().get_section(secnum);
    let be = section.bibentry(citekey);
    let bee = be.get_field("entrytype");

    // Generate labelname/year combination for tracking extradate
    // * If there is no labelname to use, use empty string
    // * If there is no date information to use, try year
    // * Don't increment the seen_namedateparts count if the name string is empty
    //   (see code in incr_seen_namedateparts method).
    // * Don't increment if skiplab is set

    if crate::Config->getblxoption(None, "labeldateparts", bee, citekey) {
      if crate::Config->getblxoption(secnum, "skiplab", bee, citekey) {
        return;
      }

        trace!("Creating extradate information for '{}'", citekey);

      let mut namehash = "";
      if let Some(lni) = be.get_labelname_info().skip_empty() {
        namehash = self._getnamehash_u(citekey, be.get_field(lni), dlist);
      }

      let mut datestring = String::new(); // Need a default empty string
      let edspec = crate::Config->getblxoption(None, "extradatespec");
      let edscope;
      // Look in each scope
      for scope in &edspec {
        // Use the first field in the scope which we find and ignore the rest
        for field in &scope {
          if let Some(f) = be.get_field(field) {
            datestring.push_str(f);
            edscope = field;
            break;
          }
        }
      }

      let tracking_string = format("{namehash},{datestring}");

      be.set_field("extradatescope", edscope);
      dlist.set_entryfield(citekey, "namedateparts", &tracking_string);
      dlist.incr_seen_namedateparts(namehash, datestring);
    }
  }

  /// Track labelname only for generation of extraname
  fn process_extraname(&self, citekey: &str, dlist: &mut DataList) {
    let secnum = self.get_current_section();
    let section = self.sections().get_section(secnum);
    let be = section.bibentry(citekey);
    let bee = be.get_field("entrytype");

    if (crate::Config->getblxoption(secnum, "skiplab", bee, citekey)) {
      return;
    }

      trace!("Creating extraname information for '{}'", citekey);

    let mut namehash = None;
    if let Some(lni) = be.get_labelname_info().skip_empty() {
      namehash = self._getnamehash_u(citekey, be.get_field(lni), dlist);
    }

    // Don't bother with extraname when there is no labelname
    if let Some(namehash) = namehash {
      dlist.set_entryfield(citekey, "labelnamehash", namehash);
      dlist.incr_seen_labelname(namehash);
    }
  }

  /// Track labelname/labeltitle combination for generation of extratitle
  fn process_extratitle(&self, citekey: &str, dlist: &mut DataList) {
    let secnum = self.get_current_section();
    let section = self.sections().get_section(secnum);
    let be = section.bibentry(citekey);
    let bee = be.get_field("entrytype");

    // Generate labelname/labeltitle combination for tracking extratitle
    // * If there is no labelname to use, use empty string
    // * If there is no labeltitle to use, use empty string
    // * Don't increment if skiplab is set

    // This is different from extradate in that we do track the information
    // if the labelname is empty as titles are much more unique than years

    if (crate::Config->getblxoption(None, "labeltitle", bee)) {
      if (crate::Config->getblxoption(secnum, "skiplab", bee, citekey)) {
        return;
      }

        trace!("Creating extratitle information for '{}'", citekey);

      let namehash = "";
      if let Some(lni) = be.get_labelname_info().skip_empty() {
        namehash = self._getnamehash_u(citekey, be.get_field(lni), dlist);
      }

      let lti = be.get_labeltitle_info();
      let title_string = be.get_field(lti).unwrap_or("");

      let nametitle_string = format!("{namehash},{title_string}");
        trace!("Setting nametitle to '{}' for entry '{}'", nametitle_string, citekey);

      dlist.set_entryfield(citekey, "nametitle", nametitle_string);

        trace!("Incrementing nametitle for '{}'", namehash);
      dlist.incr_seen_nametitle(namehash, title_string);
    }
  }

  /// Track labeltitle/labelyear combination for generation of extratitleyear
  fn process_extratitleyear(&self, citekey: &str, dlist: &mut DataList) {
    let secnum = self.get_current_section();
    let section = self.sections().get_section(secnum);
    let be = section.bibentry(citekey);
    let bee = be.get_field("entrytype");

    // Generate labeltitle/labelyear combination for tracking extratitleyear
    // * If there is no labeltitle to use, use empty string
    // * If there is no labelyear to use, use empty string
    // * Don't increment the seen_titleyear count if the labeltitle field is empty
    //   (see code in incr_seen_titleyear method).
    // * Don't increment if skiplab is set

    if (crate::Config->getblxoption(None, "labeltitleyear", bee, citekey)) {
      if (crate::Config->getblxoption($secnum, "skiplab", bee, citekey)) {
        return;
      }

        trace!("Creating extratitleyear information for '{}'", citekey);

      let lti = be.get_labeltitle_info();
      let title_string = be.get_field(lti).unwrap_or("");

      // Takes into account the labelyear which can be a range
      let year_string = be.get_field("labelyear") || be.get_field("year") || "";

      let titleyear_string = format!("{title_string},{year_string}");
        trace!("Setting titleyear to '{}' for entry '{}'", titleyear_string, citekey);

      dlist.set_entryfield(citekey, "titleyear", titleyear_string);

        trace!("Incrementing titleyear for '{}'", title_string);
      dlist.incr_seen_titleyear(title_string, year_string);
    }
  }

  /// Postprocess set entries
  ///
  /// Checks for common set errors and enforces "dataonly" options for set members.
  /// It's not necessary to set skipbib, skipbiblist in the OPTIONS field for
  /// the set members as these are automatically set by biblatex due to the \inset
  fn process_sets(self, citekey: &str) {
    let secnum = self.get_current_section();
    let section = self.sections().get_section(secnum);
    let be = section.bibentry(citekey);
    if (let @entrysetkeys = section.get_set_children(citekey)) {
      // Enforce Biber parts of virtual "dataonly" options for set members
      // Also automatically create an "entryset" field for the members
      for member in (@entrysetkeys) {
        let me = section.bibentry(member);
        process_entry_options($member, [ "skipbib", "skiplab", "skipbiblist", "uniquename=false", "uniquelist=false" ], secnum);

        // Use get_datafield() instead of get_field() because we add "entryset" below
        // and if the same entry is used in more than one set, it will pass this test
        // and generate an error if we use get_field()
        if (me.get_datafield("entryset")) {
          biber_warn("Field "entryset" is no longer needed in set member entries in Biber - ignoring in entry '$member'", $me);
          me.del_field("entryset");
        }
        // This ends up setting \inset{} in the bbl
        me.set_field("entryset", [ $citekey ]);
      }

      if !(@entrysetkeys) {
        biber_warn("No entryset found for entry $citekey of type 'set'", be);
      }
    }
    // Also set this here for any non-set keys which are in a set and which haven't
    // had skips set by being seen as a member of that set yet
    else {
      if (section.get_set_parents(citekey)) {
        process_entry_options(citekey, [ "skipbib", "skiplab", "skipbiblist", "uniquename=false", "uniquelist=false" ], $secnum);
      }
    }
  }

  /// Generate nocite information
  fn process_nocite(self, citekey: &str) {
    let secnum = self.get_current_section();
    let section = self.sections().get_section(secnum);
    let be = section.bibentry(citekey);
    // Either specifically nocited or \nocite{*} and not specifically cited without nocite
    if (section.is_nocite(citekey) ||
        (section.is_allkeys_nocite() && !section.is_specificcitekey(citekey))) {
      be.set_field("nocite", "1");
    }
  }

  /// Generate labelname information.
  fn process_labelname(self, citekey: &str) {
    let secnum = self.get_current_section();
    let section = self.sections().get_section(secnum);
    let be = section.bibentry(citekey);
    let bee = be.get_field("entrytype");
    let lnamespec = crate::Config->getblxoption(None, "labelnamespec", bee);
    let dm = crate::config::get_dm();
    let dmh = crate::config::get_dm_helpers();

    // First we set the normal labelname name
    for h_ln in &lnamespec {
      let ln = h_ln.content;
      let lnameopt = if ln.len() > 5 && ln.starts_with("short") {
        ln[5..].to_string()
      } else {
        ln.clone()
      };

      if !dmh->{namelistsall}.contains(ln) {
        biber_warn("Labelname candidate '$ln' is not a name field - skipping");
        continue;
      }

      // If there is a biblatex option which controls the use of this labelname info, check it
      if CONFIG_OPT_SCOPE_BIBLATEX.contains_left(&format!("use{lnameopt}")) &&
        !crate::Config->getblxoption(secnum, format!("use{lnameopt}"), bee, citekey) {
        continue;
      }

      if (be.get_field(ln)) {
        be.set_labelname_info(ln);
        break;
      }
    }

    // Then we loop again to set the labelname name for the fullhash generation code
    // This is because fullhash generation ignores SHORT* fields (section 4.2.4.1, BibLaTeX
    // manual)
    for h_ln in ($lnamespec->@*) {
      let ln = h_ln.content;
      if regex_is_match!(r"\Ashort(.+)\z"xms, ln) {
        continue;
      }

      // We have already warned about this above
      if !dmh->{namelistsall}.contains(ln) {
        continue;
      }

      // If there is a biblatex option which controls the use of this labelname info, check it
      if CONFIG_OPT_SCOPE_BIBLATEX.contains_left(&format!("use{ln}")) &&
        !crate::Config->getblxoption(secnum, &format!("use{ln}"), bee, citekey) {
        continue;
      }

      if (be.get_field(ln)) {
        be.set_labelnamefh_info(ln);
        break;
      }
    }

    if !(be.get_labelname_info()) {
        debug!("Could not determine the labelname source of entry {}", citekey);
    }
  }

  /// Generate labeldate information, including times
  fn process_labeldate(self, citekey: &str) {
    let secnum = self.get_current_section();
    let section = self.sections().get_section(secnum);
    let be = section.bibentry(citekey);
    let bee = be.get_field("entrytype");
    let $dm = crate::config::get_dm();

    if (crate::Config->getblxoption(None, "labeldateparts", bee, citekey)) {
      let $ldatespec = crate::Config->getblxoption(None, "labeldatespec", bee);
      for lds in ($ldatespec->@*) {
        let pseudodate = false;
        let $ld = $lds->{content};
        if ($lds->{"type"} == "field") { // labeldate field

          let ldy;
          let ldey;
          let ldm;
          let ldd;
          let ldhour;
          let ldmin;
          let ldsec;
          let ldtz;

          // resolve dates
          let datetype = if let Some(s) = ld.strip_suffix("date") {
            s
          } else {
            ld
          };

          if dm.field_is_datatype("date", ld) &&
              be.get_field(format!("{datetype}datesplit")) { // real EDTF dates
            ldy    = format!("{datetype}year");
            ldey   = format!("{datetype}endyear");
            ldm    = format!("{datetype}month");
            ldd    = format!("{datetype}day");
            ldhour = format!("{datetype}hour");
            ldmin  = format!("{datetype}minute");
            ldsec  = format!("{datetype}second");
            ldtz   = format!("{datetype}timezone");
          }
          else { // non-EDTF split date field so make a pseudo-year
            ldy = ld;
            pseudodate = true;
          }

          // Did we find a labeldate - this is equivalent to checking for a year/endyear
          // as that is always present if there is a labeldate
          if be.get_field(ldy).is_some() || be.get_field(ldey).is_some() {
            // set source to field or date field prefix for a real date field
            be.set_labeldate_info(LabelDateInfo::Field {
              year: ldy,
              month: ldm,
              day: ldd,
              hour: ldhour,
              minute: ldmin,
              second: ldsec,
              timezone: ldtz,
              pseudodate,
              source: if pseudodate { ldy } else { datetype }
            });
            break;
          }
        }
        else if ($lds->{"type"} == "string") { // labelyear fallback string
          be.set_labeldate_info(LabelDateInfo::String(ld));
          break;
        }
      }

      // Construct label*
      // Might not have been set due to skiplab
      if let Some(ldi) = be.get_labeldate_info() {
        match ldi {
          LabelDateInfo(df) => { // set labelyear to a field value
            let pseudodate = df.pseudodate;
            be.set_field("labelyear", be.get_field(df.year));
            if df.month {
              be.set_field("labelmonth", be.get_field(df.month));
            }
            if df.day {
              be.set_field("labelday", be.get_field(df.day));
            }
            if df.hour {
              be.set_field("labelhour", be.get_field(df.hour));
            }
            if df.minute {
              be.set_field("labelminute", be.get_field(df.minute));
            }
            if df.second {
              be.set_field("labelsecond", be.get_field(df.second));
            }
            if df.timezone {
              be.set_field("labeltimezone", be.get_field(df.timezone));
            }
            be.set_field("labeldatesource", df.source);

            // ignore endyear if it's the same as year
            let ytype = df.year.strip_suffix("year").unwrap_or(""); // Avoid undef warnings since no match above can make it undef

            // construct labelyear from start/end year field
            let f = format!("{ytype}endyear");
            if (be.field_exists(&f)
                && ((be.get_field(df.year).unwrap_or("")) != be.get_field(&f))) {
              be.set_field(
                "labelyear",
                format!(r"{}\bibdatedash {f}", be.get_field("labelyear").unwrap_or(""))
              );
            }
            if !pseudodate {
              // construct labelmonth from start/end month field
              let f = be.get_field(&format!("{ytype}endmonth"));
              if f && ((be.get_field(df.month).unwrap_or("")) != f) {
                be.set_field(
                  "labelmonth",
                  format!(r"{}\bibdatedash {f}", be.get_field("labelmonth").unwrap_or(""))
                );
              }
              // construct labelday from start/end month field
              let f = be.get_field(&format!("{ytype}endday"));
              if f && ((be.get_field(df.day).unwrap_or("")) != f) {
                be.set_field(
                  "labelday",
                  format!(r"{}\bibdatedash {f}", be.get_field("labelday").unwrap_or(""))
                );
              }
              // construct labelhour from start/end hour field
              let f = be.get_field(&format!("{ytype}endhour"));
              if f && ((be.get_field(df.hour).unwrap_or("")) != f) {
                be.set_field(
                  "labelhour",
                  format!(r"{}\bibdatedash {f}", be.get_field("labelhour").unwrap_or(""))
                );
              }
              // construct labelminute from start/end minute field
              let f = be.get_field(&format!("{ytype}endminute"));
              if f && ((be.get_field(df.minute).unwrap_or("")) != f) {
                be.set_field(
                  "labelminute",
                  format!(r"{}\bibdatedash {f}", be.get_field("labelminute").unwrap_or(""))
                );
              }
              // construct labelsecond from start/end second field
              let f = be.get_field(&format!("{ytype}endsecond"));
              if f && ((be.get_field(df.second).unwrap_or("")) != f) {
                be.set_field(
                  "labelsecond",
                  format!(r"{}\bibdatedash {f}", be.get_field("labelsecond").unwrap_or(""))
                );
              }
            }
          }
          LabelDateInfo::String(ys) { // set labeldatesource to a fallback string
            be.set_field("labeldatesource", ys);
          }
        }
      }
      else {
          debug!("labeldate information of entry {} is unset", citekey);
      }
    }
  }

  /// Generate labeltitle
  ///
  /// Note that this is not conditionalised on the biblatex "labeltitle"
  /// as labeltitle should always be output since all standard styles need it.
  /// Only extratitle is conditionalised on the biblatex "labeltitle" option.
  fn process_labeltitle(self, citekey: &str) {
    let secnum = self.get_current_section();
    let section = self.sections().get_section(secnum);
    let be = section.bibentry(citekey);
    let bee = be.get_field("entrytype");

    let $ltitlespec = crate::Config->getblxoption(None, "labeltitlespec", bee);

    for h_ltn in ($ltitlespec->@*) {
      let ltn = h_ltn.content;
      if (let $lt = be.get_field(ltn)) {
        be.set_labeltitle_info(ltn);
        be.set_field("labeltitle", lt);
        break;
      }
        debug!("labeltitle information of entry {} is unset", citekey);
    }
  }

  /// Generate fullhash
  fn process_fullhash(self, citekey: &str) {
    let secnum = self.get_current_section();
    let section = self.sections().get_section(secnum);
    let be = section.bibentry(citekey);
    let dmh = crate::config::get_dm_helpers();

    // fullhash is generated from the labelname but ignores SHORT* fields and
    // max/mincitenames settings
    // This can't be resolved nicely by biblatex because it depends on use* options
    // and also SHORT* fields etc.
    if (let $lnfhi = be.get_labelnamefh_info()) {
      if (let $lnfh = be.get_field(lnfhi)) {
        be.set_field("fullhash", self._getfullhash(citekey, lnfh));
      }
    }

    // Generate fullhash for all other name fields
    for n in ($dmh->{namelistsall}->@*) {
      let nv = be.get_field(n);
      if !nv {
        continue;
      }
      be.set_field(format!("{n}fullhash"), self._getfullhash(citekey, nv));
    }

    return;
  }

  /// Generate namehash
  fn process_namehash(&self, citekey: &str, dlist: &mut DataList) {
    let secnum = self.get_current_section();
    let section = self.sections().get_section(secnum);
    let be = section.bibentry(citekey);
    let $dmh = crate::config::get_dm_helpers();

    // namehash is generated from the labelname
    // This can't be resolved nicely by biblatex because it depends on use* options
    // and also SHORT* fields etc.
    if let Some(lni) = be.get_labelname_info().skip_empty() {
      if (let $ln = be.get_field(lni)) {
        dlist.set_entryfield(citekey, "namehash", self._getnamehash(citekey, ln, dlist, false));
        dlist.set_entryfield(citekey, "bibnamehash", self._getnamehash(citekey, ln, dlist, true));
      }
    }

    // Generate namehash for all other name fields
    for n in ($dmh->{namelistsall}->@*) {
      let nv = be.get_field(n);
      if !nv {
        continue;
      }
      dlist.set_entryfield(citekey, &format!("{n}namehash"), self._getnamehash(citekey, nv, dlist, false));
      dlist.set_entryfield(citekey, &format!("{n}bibnamehash"), self._getnamehash(citekey, nv, dlist, true));
    }

    return;
  }

  /// Generate per_name_hashes
  fn process_pername_hashes(self, citekey: &str, $dlist: &mut DataList) {
    let secnum = self.get_current_section();
    let section = self.sections().get_section(secnum);
    let be = section.bibentry(citekey);
    let dmh = crate::config::get_dm_helpers();

    for pn in ($dmh->{namelistsall}->@*) {
      let nl = be.get_field(pn);
      if !nl {
        continue;
      }
      for n in nl.names() {
        let $pnhash = $self->_genpnhash($citekey, $n);
        n.set_hash($pnhash);
        dlist.set_namehash(nl.get_id(), n.get_id(), pnhash);
      }
    }
    return;
  }

  /// Generate the visible name information.
  /// This is used in various places and it is useful to have it generated in one place.
  fn process_visible_names(self, $dlist) {
    let secnum = self.get_current_section();
    let section = self.sections().get_section(secnum);
    let $dmh = crate::config::get_dm_helpers();

      debug!("Postprocessing visible names for section {}", secnum);
    for citekey in section.get_citekeys() {
      let be = section.bibentry(citekey);
      let bee = be.get_field("entrytype");

      let $maxcn = crate::Config->getblxoption($secnum, "maxcitenames", bee, citekey);
      let $mincn = crate::Config->getblxoption($secnum, "mincitenames", bee, citekey);
      let $maxbn = crate::Config->getblxoption($secnum, "maxbibnames", bee, citekey);
      let $minbn = crate::Config->getblxoption($secnum, "minbibnames", bee, citekey);
      let $maxsn = crate::Config->getblxoption($secnum, "maxsortnames", bee, citekey);
      let $minsn = crate::Config->getblxoption($secnum, "minsortnames", bee, citekey);
      let $maxan = crate::Config->getblxoption($secnum, "maxalphanames", bee, citekey);
      let $minan = crate::Config->getblxoption($secnum, "minalphanames", bee, citekey);

      for n in ($dmh->{namelistsall}->@*) {
        let nl = be.get_field(n);
        if !nl {
          continue;
        }

        let $count = $nl->count;
        let $visible_names_cite;
        let $visible_names_bib;
        let $visible_names_sort;
        let $visible_names_alpha;

        // Cap min*names for this entry at $count. Why? Because imagine we have this:
        //
        // John Smith and Bill Jones
        //
        // and mincitenames=3. Then visibility will be set to 3 but there aren't 3 names to
        // get information from so looping over the visibility count would cause name methods
        // to operate on undef at index 3 and die
        let $l_mincn = $count < $mincn ? $count : $mincn;
        let $l_minbn = $count < $minbn ? $count : $minbn;
        let $l_minsn = $count < $minsn ? $count : $minsn;
        let $l_minan = $count < $minan ? $count : $minan;

        // If name list was truncated in bib with "and others", this means that the
        // name list has already been manually truncated to the correct visibility
        // and so the visibility is just the count of the explicit names

        // max/minalphanames doesn't care about uniquelist - labels are just labels
        if ($count > $maxan) {
          $visible_names_alpha = $l_minan;
        }
        else {
          $visible_names_alpha = $count;
        }

        // max/mincitenames
        if ($count > $maxcn) {
          // Visibility to the uniquelist point if uniquelist is requested
          // We know at this stage that if uniquelist is set, there are more than maxcitenames
          // names. We also know that uniquelist > mincitenames because it is a further
          // disambiguation on top of mincitenames so can't be less as you can't disambiguate
          // by losing information
          $visible_names_cite = dlist.get_uniquelist(nl.get_id()).unwrap_or($l_mincn);
        }
        else { // visibility is simply the full list
          $visible_names_cite = $count;
        }

        // If biblatex option "pluralothers" is true, then "et al" must replace more than one element
        // in a name list. This means that the visibility must be increased to the list length if
        // the visibility is one less than the list count. If we don't do this, extra* will be incremented
        // to disambiguate the list and this will be incorrect as biblatex.sty will detect "pluralothers"
        // and print an unambiguous list which will be disambiguated by extra* unnecessarily.
        if Biber::Config->getblxoption(None, "pluralothers") &&
            $count-$visible_names_cite==1 {
          $visible_names_cite = $count;
        }
  
        // max/minbibnames
        if ($count > $maxbn) {
          // Visibility to the uniquelist point if uniquelist is requested
          // We know at this stage that if uniquelist is set, there are more than maxbibnames
          // names. We also know that uniquelist > minbibnames because it is a further
          // disambiguation on top of minbibnames so can't be less as you can't disambiguate
          // by losing information
          $visible_names_bib = dlist.get_uniquelist(nl.get_id()).unwrap_or($l_minbn);
        }
        else { // visibility is simply the full list
          $visible_names_bib = $count;
        }

        // max/minsortnames
        if ($count > $maxsn) {
          // Visibility to the uniquelist point if uniquelist is requested
          // We know at this stage that if uniquelist is set, there are more than maxsortnames
          // names. We also know that uniquelist > minsortnames because it is a further
          // disambiguation on top of minsortnames so can't be less as you can't disambiguate
          // by losing information
          $visible_names_sort = dlist.get_uniquelist(nl.get_id()).unwrap_or($l_minsn);
        }
        else { // visibility is simply the full list
          $visible_names_sort = $count;
        }

          trace!("Setting visible names (cite) for key '{}' to '{}'", citekey, visible_names_cite);
          trace!("Setting visible names (bib) for key '{}' to '{}'", citekey, visible_names_bib);
          trace!("Setting visible names (sort) for key '{}' to '{}'", citekey, visible_names_sort);
          trace!("Setting visible names (alpha) for key '{}' to '{}'", citekey, visible_names_alpha);

        // Need to set these on all name forms
        let nlid = be.get_field(n).get_id();
        $dlist->set_visible_cite($nlid, $visible_names_cite);
        $dlist->set_visible_bib($nlid, $visible_names_bib);
        $dlist->set_visible_sort($nlid, $visible_names_sort);
        $dlist->set_visible_alpha($nlid, $visible_names_alpha);
      }
    }
  }

  /// Generate the labelalpha and also the variant for sorting
  fn process_labelalpha(&self, citekey: &str, dlist: &mut DataList) {
    let secnum = self.get_current_section();
    let section = self.sections().get_section(secnum);
    let be = section.bibentry(citekey);
    let bee = be.get_field("entrytype");
    // Don't add a label if skiplab is set for entry
    if (crate::Config->getblxoption($secnum, "skiplab", bee, citekey)) {
      return;
    }
    if ( let $la = crate::Config->getblxoption(None, "labelalpha", bee, citekey) ) {
      let (label, sortlabel) = $self->_genlabel(citekey, dlist)->@*;
      dlist.set_entryfield(citekey, "labelalpha", label);
      dlist.set_entryfield(citekey, "sortlabelalpha", sortlabel);
    }
  }

  /// Generate the extraalpha information
  fn process_extraalpha(&self, citekey: &str, dlist: &mut DataList) {
    let secnum = self.get_current_section();
    let section = self.sections().get_section(secnum);
    let be = section.bibentry(citekey);
    let bee = be.get_field("entrytype");
    if (crate::Config->getblxoption(None, "labelalpha", bee, citekey)) {
      if (let $la = dlist.get_entryfield(citekey, "labelalpha")) {
        dlist.incr_la_disambiguation(la);
      }
    }
  }

  /// Put presort fields for an entry into the main Biber bltx state
  /// so that it is all available in the same place since this can be
  /// set per-type and globally too.
  fn process_presort(self, citekey: &str) {
    let secnum = self.get_current_section();
    let section = self.sections().get_section(secnum);
    let be = section.bibentry(citekey);
    // We are treating presort as an option as it can be set per-type and globally too
    if (let $ps = be.get_field("presort")) {
      crate::Config->setblxoption($secnum, "presort", $ps, "ENTRY", citekey);
    }
  }

  /// Process a bibliography list
  fn process_lists(self) {
    let secnum = self.get_current_section();
    let section = self.sections().get_section(secnum);

    for list in ($self->datalists->get_lists_for_section($secnum)->@*) {
      let $lattrs = $list->get_attrs;
      let ltype = list.get_type();
      let lname = list.get_name();

      // sanitise state - essential in tests which call crate::prepare() multiple times
      $list->reset_state;

      // Last-ditch fallback in case we still don't have a sorting spec
      if !($list->get_sortingnamekeytemplatename) {
        $list->set_sortingnamekeytemplatename("global");
      }
      if list.get_uniquenametemplatename().skip_empty().is_none() {
        list.set_uniquenametemplatename("global");
      }
      if !($list->get_labelalphanametemplatename) {
        $list->set_labelalphanametemplatename("global");
      }
      list.set_keys([ section.get_citekeys() ]);
        debug!("Populated datalist '{}' of type '{}' with attributes '{}' in section {} with keys: {}", lname, ltype, lattrs, secnum, list.get_keys().join(", "));

      // A datalist represents a biblatex refcontext
      // and many things are refcontext specific and so we need to use the right data. For
      // example labelalphanametemplate and uniquenametemplate can be set per-list and much
      // processing uses these

      if !(crate::Config->getoption("tool")) {

        // Set this so that uniqueness processing starts
        list.set_unul_changed(true);

        // Main processing loop, part 1
        $self->process_entries_pre($list);

        // Generate uniqueness information
        $self->uniqueness($list);
      }

      // Generate visible names information for all entries
      $self->process_visible_names($list);

      if !(crate::Config->getoption("tool")) {
        // Main processing loop, part 2
        $self->process_entries_post($list);

        // Final processing loop
        $self->process_entries_final($list);
      }

      // Filtering - must come before sorting/labelling so that there are no gaps in e.g. extradate
      if (let $filters = list.get_filters()) {
        let $flist = [];
      'KEYLOOP: for k list.get_keys() {
          let be = section.bibentry(k);
          for f in filters {
            // Filter disjunction is ok if any of the checks are ok, hence the grep()
            if (ref $f == "ARRAY") {
              if !f.iter().any(|f| check_list_filter(k, f, be)) {
                continue 'KEYLOOP ;
              }
            }
            else {
              if !check_list_filter(k, f, be) {
                continue 'KEYLOOP;
              }
            }
          }
          flist.push(k);
        }
          debug!("Keys after filtering list '{lname}' in section {secnum}: {}", flist.join(", "));
        list.set_keys(flist); // Now save the sorted list in the list object
      }

      // Sorting
      self.generate_sortdataschema(list); // generate the sort schema information
      self.generate_sortinfo(list);       // generate the sort information
      self.sort_list(list);               // sort the list
      if !crate::Config->getoption("tool") {
        $self->generate_contextdata($list);
      }

    }
    return;
  }

  /// Run an entry through a list filter. Returns a boolean.
  fn check_list_filter(k: &str, filter: &Filter, fs, be: &Entry) -> bool {
    let Filter {
      typ: t,
      value: fs,
    } = filter;
      debug!("Checking key '{k}' against filter '{t}={fs}'");
    match t {
      FilterType::Type => {
        if (be.get_field("entrytype") == fs.to_lowercase())) {
            trace!("Key '{k}' passes against filter '{t}={fs}'");
        }
        else {
          return false;
        }
      }
      FilterType::NotType => {
        if (be.get_field("entrytype") == fs.to_lowercase()) {
          return false;
        }
        else {
            trace!("Key '{k}' passes against filter '{t}={fs}'");
        }
      }
      FilterType::Subtype => {
        if (be.field_exists("entrysubtype") &&
            be.get_field("entrysubtype") == fs.to_lowercase()) {
            trace!("Key '{k}' passes against filter '{t}={fs}'");
        }
        else {
          return false;
        }
      }
      FilterType::NotSubtype => {
        if (be.field_exists("entrysubtype") &&
            be.get_field("entrysubtype") == fs.to_lowercase()) {
          return false;
        }
        else {
            trace!("Key '{k}' passes against filter '{t}={fs}'");
        }
      }
      FilterType::Keyword => {
        if be.has_keyword(fs) {
            trace!("Key '{k}' passes against filter '{t}={fs}'");
        }
        else {
          return false;
        }
      }
      FilterType::NotKeyword => {
        if be.has_keyword(fs) {
          return false;
        }
        else {
            trace!("Key '{k}' passes against filter '{t}={fs}'");
        }
      }
      FilterType::Field => {
        if be.field_exists(fs) {
            trace!("Key '{k}' passes against filter '{t}={fs}'");
        }
        else {
          return false;
        }
      }
      FilterType::NotField => {
        if be.field_exists(fs) {
          return false;
        }
        else {
            trace!("Key '{k}' passes against filter '{t}={fs}'");
        }
      }
    }
    return true;
  }

  /// Generate sort data schema for Sort::Key from sort spec like this:
  ///
  /// ```
  /// spec   => [
  ///           [None, { presort => {} }],
  ///           [{ final => 1 }, { sortkey => {} }],
  ///           [
  ///             {"sort_direction"  => "descending"},
  ///             { sortname => {} },
  ///             { author => {} },
  ///             { editor => {} },
  ///             { translator => {} },
  ///             { sorttitle => {} },
  ///             { title => {} },
  ///           ],
  ///           [None, { sortyear => {} }, { year => {} }],
  ///           [None, { sorttitle => {} }, { title => {} }],
  ///           [None, { volume => {} }, { "0000" => {} }],
  ///          ],
  /// ```
  fn generate_sortdataschema(self, $list) {
    let $dm = crate::config::get_dm();
    let mut ds = Vec::new();

    // Check if sorting templatename for the list contains anything ...
    let $schema = if (keys crate::Config->getblxoption(None, "sortingtemplate")->{list.get_sortingtemplatename()}->%*) {
      crate::Config->getblxoption(None, "sortingtemplate")->{list.get_sortingtemplatename()}
    } else {
      // ... fall back to global default if named template does not exist
      crate::Config->getblxoption(None, "sortingtemplate")->{crate::Config->getblxoption(None, "sortingtemplatename")}
    };

    $list->set_sortingtemplate($schema); // link the sort schema into the list

    for sort in ($schema->{spec}->@*) {
      // Assume here that every item in a sorting spec section is the same datatype
      // See header for data structure
      let mut direction = String::new();
      for (sopt, val) in sort[0].iter() {
        if sopt == "sort_direction" && val == "descending" {
            direction = '-'.to_string();
        }
      }
      let $spec = $dm->{sortdataschema}->(sort[1].keys().next().unwrap());
      ds.push({"spec"  => format!("{direction}{spec}"),
                    $spec => 1}); // Speed shortcut for sortkey extraction sub

    }
    $list->set_sortdataschema($ds);
    return;
  }

  /// Generate information for sorting
  fn generate_sortinfo(self, $dlist) {

    for key dlist.get_keys() {
      $self->_generatesortinfo($key, $dlist);
    }
    return;
  }

  /// Generate the uniqueness information needed when creating .bbl
  fn uniqueness(self, $dlist) {
    let secnum = self.get_current_section();
    let section = self.sections().get_section(secnum);
    // Generate uniqueness information according to this algorithm:
    // 1. Generate uniquename if uniquename option is set
    // 2. if (uniquelist has never run before OR step 1 changed any uniquename values) {
    //      goto step 3
    //    } else { return }
    // 3. Completely regenerate uniquelist if uniquelist option is set
    // 4. if (step 3 changed any uniquelist values) {
    //      goto step 1
    //    } else { return }

    // uniquelist can never shorten to a list shorter than maxcitenames because:
    // * Shortening a list can't make it unique
    // * You can't lengthen it if the list is shorter than maxcitenames because there
    //   is no more information to add that you don't already have.
    // uniquelist cannot be less than mincitenames as the list is either unambiguous
    // at mincitenames or it isn't and uniquelist needs more information by adding items

    // Set a flag for first uniquelist pass. This is a special case as we always want to run
    // at least one uniquelist pass if requested, regardless of unul_done global flag.
    let mut first_ul_pass = true;

    // Generate uniquename information, if requested
    while ("true") {
      if !dlist.get_unul_done() {
          debug!("Entering uniquename processing");
        dlist.set_unul_changed(false); // reset state for global unul changed flag
        $self->create_uniquename_info($dlist);
        $self->generate_uniquename($dlist);
      }
      else {
        break; // uniquename/uniquelist disambiguation is finished as nothing changed
      }
      // Generate uniquelist information, if requested
      // Always run uniquelist at least once, if requested
      if first_ul_pass || !dlist.get_unul_done() {
          debug!("Entering uniquelist processing");
        dlist.set_unul_changed(false); // reset state for global unul changed flag
        first_ul_pass = false; // Ignore special case when uniquelist has run once
        $self->create_uniquelist_info($dlist);
        $self->generate_uniquelist($dlist);
      }
      else {
        break; // uniquename/uniquelist disambiguation is finished as nothing changed
      }
    }
    return;
  }

  /// Gather the uniquename information as we look through the names
  ///
  /// What is happening in here is the following: We are registering the
  /// number of occurrences of each name, name+init and fullname within a
  /// specific context. For example, the context is "global" with uniquename
  /// < mininit and "name list" for uniquename=mininit or minfull. The keys
  /// we store to count this are the most specific information for the
  /// context, so, for uniquename < mininit, this is the full name and for
  /// uniquename=mininit or minfull, this is the complete list of full names.
  /// These keys have values in a hash which are ignored. They serve only to
  /// accumulate repeated occurrences with the context and we don't care
  /// about this and so the values are a useful sinkhole for such repetition.
  ///
  /// For example, if we find in the global context a base name "Smith" in two different entries
  /// under the same form "Alan Smith", the data structure will look like:
  ///
  /// {Smith}->{global}->{Alan Smith} = 2
  ///
  /// We don't care about the value as this means that there are 2 "Alan Smith"s in the global
  /// context which need disambiguating identically anyway. So, we just count the keys for the
  /// base name "Smith" in the global context to see how ambiguous the base name itself is. This
  /// would be "1" and so "Alan Smith" would get uniquename=false because it's unambiguous as just
  /// "Smith".
  ///
  /// The same goes for "minimal" list context disambiguation for uniquename=mininit or minfull.
  /// For example, if we had the base name "Smith" to disambiguate in two entries with labelname
  /// "John Smith and Alan Jones", the data structure would look like:
  ///
  /// {Smith}->{Smith+Jones}->{John Smith+Alan Jones} = 2
  ///
  /// Again, counting the keys of the context for the base name gives us "1" which means we
  /// have uniquename=false for "John Smith" in both entries because it's the same list. This also
  /// works for repeated names in the same list "John Smith and Bert Smith". Disambiguating
  /// "Smith" in this:
  ///
  /// {Smith}->{Smith+Smith}->{John Smith+Bert Smith} = 2
  ///
  /// So both "John Smith" and "Bert Smith" in this entry get
  /// uniquename=false (of course, as long as there are no other "X Smith and
  /// Y Smith" entries where X != "John" or Y != "Bert").
  ///
  /// The values from biblatex.sty:
  ///
  /// false   = 0
  /// init    = 1
  /// true    = 2
  /// full    = 2
  /// allinit = 3
  /// allfull = 4
  /// mininit = 5
  /// minfull = 6
  fn create_uniquename_info(&self, dlist: &mut Datalist) {
    let secnum = self.get_current_section();
    let section = self.sections().get_section(secnum);
    let bibentries = section.bibentries();

    // Reset uniquename information as we have to generate it
    // again because uniquelist information might have changed
    dlist.reset_uniquenamecount();

    'MAIN: for citekey in section.get_citekeys() {
      let be = bibentries.entry(citekey);
      let bee = be.get_field("entrytype");
      let lni = match be.get_labelname_info() {
        None => continue, // only care about labelname
        Some(lni) => lni,
      }

      let nl = be.get_field(lni);
      let nlid = nl.get_id();

      let un = crate::Config->getblxoption(secnum, "uniquename", bee, citekey);

      // Per-namelist uniquename
      un = nl.get_uniquename().or(un);

        trace!("Generating uniquename information for '{}'", citekey);

      // Set the index limit beyond which we don't look for disambiguating information
      let ul = None;             // Not set
      if let Some(ulist) = dlist.get_uniquelist(nlid) {
        // If defined, $ul will always be >1, see comment in set_uniquelist() in Names.pm
        ul = Some(ulist);
      }
      let maxcn = crate::Config->getblxoption(secnum, "maxcitenames", bee, citekey);
      let mincn = crate::Config->getblxoption(secnum, "mincitenames", bee, citekey);

      // Note that we don't determine if a name is unique here -
      // we can't, were still processing entries at this point.
      // Here we are just recording seen combinations of the basename plus
      // non-basename parts in both initial and full formats.
      //
      // A name scope can be either a complete single name or a list of names
      // depending on whether uniquename=min* or not
      //
      // Anything which has more than one combination for a given basename+non-basenameparts
      // would be uniquename = 2 unless even the full name doesn't disambiguate
      // and then it is left at uniquename = 0

      let num_names = nl.count();
      let names = nl.names();

      // If name list was truncated in bib with "and others", this overrides maxcitenames
      let morenames = nl.get_morenames();

      let truncnames = HashSet::new();
      let basenames = Vec::new();
      let allnames = Vec::new();

      for n in names {
        let nid = n.get_id();

        // Per-name uniquename
        un = n.get_uniquename().or(un);

        if un == "false" {
          continue 'MAIN;
        }

        // We need to track two types of uniquename disambiguation here:
        //
        // 1. Information to disambiguate visible names from visible names
        //    where "visibility" is governed by uniquelist/max/mincitenames.
        //    This is the actual "uniquename" feature information.
        // 2. Information to disambiguate all names, regardless of visibility
        //    This is needed for uniquelist because it needs to construct
        //    hypothetical ambiguity information for every list position.

        // We want to record disambiguation information for visible names when:
        // uniquename = allinit or allfull
        // Uniquelist is set and a name appears before the uniquelist truncation
        // Uniquelist is not set and the entry has an explicit "and others" at the end
        //   since this means that every name is less than maxcitenames by definition
        // Uniquelist is not set and a name list is shorter than the maxcitenames truncation
        // Uniquelist is not set, a name list is longer than the maxcitenames truncation
        //   and the name appears before the mincitenames truncation

        if (un == "allinit" || $un == "allfull" ||
            (ul && n.get_index() <= ul) ||
            morenames ||
            num_names <= maxcn ||
            n.get_index() <= mincn) { // implicitly, $num_names > $maxcn here

          truncnames.insert(nid);
          if (un == "mininit" || un == "minfull") {
            basenames.push(dlist.get_basenamestring(nlid, nid));
            allnames.push(dlist.get_namestring(nlid, nid));
          }
        }
      }
      // Information for mininit or minfull, here the basename
      // and non-basename is all names in the namelist, not just the current name
      let mut min_basename;
      let mut min_namestring;
      if (un == "mininit" || un == "minfull") {
        min_basename = basenames.join("\x{10FFFD}");
        min_namestring = allnames.join("\x{10FFFD}");
        if (basenames.len() < num_names || morenames) {
          min_basename.push_str("\x{10FFFD}et al");     // if truncated, record this
          min_namestring.push_str("\x{10FFFD}et al");   // if truncated, record this
        }
      }

      for n in &names {
        let nid = n.get_id();
        let basename    = dlist.get_basenamestring(nlid, nid);
        let namestring  = dlist.get_namestring(nlid, nid);
        let namestrings = dlist.get_namestrings(nlid, nid);
        let namedisamiguationscope;
        let nskey;

        // Disambiguation scope and key depend on the uniquename setting
        if (un == "init" || un == "full" || un == "allinit" || un == "allfull") {
          namedisamiguationscope = "global";
          nskey = namestrings.join("\x{10FFFD}");
        }
        else if (un == "mininit" || un == "minfull") {
          namedisamiguationscope = min_basename;
          nskey = min_namestring;
          dlist.set_unmininfo(nlid, nid, min_basename);
        }

        if truncnames.contains(nid) {
          // Record uniqueness information entry for all name contexts
          // showing that they have been seen for this name key in this name scope
          for ns in namestrings {
            dlist.add_uniquenamecount(ns, namedisamiguationscope, nskey);
          }
        }

        // As above but here we are collecting (separate) information for all
        // names, regardless of visibility (needed to track uniquelist)
        let eul = crate::Config->getblxoption(secnum, "uniquelist", bee, citekey);

        // Per-namelist uniquelist
        let nl = be.get_field(lni);
        if lni.is_some() {
          if nl.get_uniquelist() {
            eul = nl.get_uniquelist();
          }
        }

        if (eul != "false") {
          for ns in namestrings {
            dlist.add_uniquenamecount_all(ns, namedisamiguationscope, nskey);
          }
        }
      }
    }

    return;
  }

  /// Generate the per-name uniquename values using the information
  /// harvested by create_uniquename_info()
  fn generate_uniquename(self, $dlist) {
    let secnum = self.get_current_section();
    let section = self.sections().get_section(secnum);
    let bibentries = section.bibentries();

    // Now use the information to set the actual uniquename information
  'MAIN:  for citekey in section.get_citekeys() {
      let be = bibentries.entry(citekey);
      let bee = be.get_field("entrytype");
      let lni = match be.get_labelname_info() {
        None => continue, // only care about labelname
        Some(lni) => lni,
      }

      let nl = be.get_field(lni);
      let nlid = nl.get_id();

      let un = crate::Config->getblxoption(secnum, "uniquename", bee, citekey);

      // Per-namelist uniquename
      un = nl.get_uniquename().or(un);

        trace!("Setting uniquename for '{}'", citekey);

      // Set the index limit beyond which we don't look for disambiguating information
      // If defined, $ul will always be >1, see comment in set_uniquelist() in Names.pm
      let ul = dlist.get_uniquelist(nlid);

      let maxcn = crate::Config->getblxoption(secnum, "maxcitenames", bee, citekey);
      let mincn = crate::Config->getblxoption(secnum, "mincitenames", bee, citekey);

      let num_names = nl.count();
      let names = nl.names();
      // If name list was truncated in bib with "and others", this overrides maxcitenames
      let morenames = nl.get_morenames();

      let truncnames = HashSet::new();

      for n in &names {
        let nid = n.get_id();

        // Per-name uniquename
        un = n.get_uniquename().or(un);

        if un == "false" {
          continue 'MAIN;
        }

        if (un == "allinit" || un == "allfull" ||
            (ul && n.get_index() <= ul) ||
            morenames ||
            num_names <= maxcn ||
            n.get_index() <= mincn) { // implicitly, $num_names > $maxcn here
          truncnames.insert(nid);
        }
        else {
          // Set anything now not visible due to uniquelist back to 0
          dlist.reset_uniquename(nlid, nid);
        }
      }

      for n in &names {
        let nid = n.get_id();
        let basename = dlist.get_basenamestring(nlid, nid);
        let namestrings = dlist.get_namestrings(nlid, nid);
        let namedisschema = dlist.get_namedisschema(nlid, nid);
        let mut namescope = "global"; // default

        if ($un == "mininit" || $un == "minfull") {
          namescope = dlist.get_unmininfo(nlid, nid);
        }

        if truncnames.contains(nid) {
          for (i, ns) in namestrings.iter().enumerate() {
            let nss = namedisschema[i];
            if dlist.get_numofuniquenames(ns, namescope) == 1 {
              dlist.set_uniquename(nlid, nid, nss);
              // We have found the most general disambiguation schema which disambiguates,
              // skip the rest since the schema array goes from most general to least general
              break;
            }
          }
          // Nothing disambiguates, set to just base of schema
          if dlist.get_uniquename(nlid, nid).is_none() {
            dlist.set_uniquename(nlid, nid, namedisschema[0]);
          }
        }

        let eul = crate::Config->getblxoption(secnum, "uniquelist", bee, citekey);
        // Per-namelist uniquelist
        let names = be.get_field(be.get_labelname_info());
        eul = names.get_uniquelist().or(eul);

        // As above but not just for visible names (needed for uniquelist)
        if (eul != "false") {
          for (i, ns) in namestrings.iter().enumerate() {
            let nss = namedisschema[i];
            if (dlist.get_numofuniquenames_all(ns, namescope) == 1) {
              dlist.set_uniquename_all(nlid, nid, nss);
              // We have found the most general disambiguation schema which disambiguates,
              // skip the rest since the schema array goes from most general to least general
              break;
            }
          }
          // Nothing disambiguates, set to just base of schema
          if dlist.get_uniquename_all(nlid, nid).is_none() {
            dlist.set_uniquename_all(nlid, nid, namedisschema[0]);
          }
            
        }
      }
    }
  }

  /// Gather the uniquelist information as we look through the names
  fn create_uniquelist_info(&self, dlist: &mut DataList) {
    let secnum = self.get_current_section();
    let section = self.sections().get_section(secnum);
    let bibentries = section.bibentries();

    // Reset uniquelist information as we have to generate it again because uniquename
    // information might have changed
    dlist.reset_uniquelistcount();

    for citekey in section.get_citekeys() {
      let be = bibentries.entry(citekey);
      let bee = be.get_field("entrytype");
      let maxcn = crate::Config->getblxoption(secnum, "maxcitenames", bee, citekey);
      let mincn = crate::Config->getblxoption(secnum, "mincitenames", bee, citekey);
      let lni = match be.get_labelname_info() {
        None => continue, // only care about labelname
        Some(lni) => lni,
      }
      let nl = be.get_field(lni);
      let nlid = nl.get_id();
      let labelyear = be.get_field("labelyear");

      let ul = crate::Config->getblxoption(secnum, "uniquelist", bee, citekey);

      // Per-namelist uniquelist
      ul = nl.get_uniquelist().or(ul);

      if ul == "false" {
        continue;
      }

        trace!("Generating uniquelist information for '{}'", citekey);

      let num_names = nl.count();
      let namelist = Vec::new();
      let ulminyear_namelist = Vec::new();

      for n in nl.names() {
        let nid = n.get_id();
        let basename = dlist.get_basenamestring(nlid, nid);
        let namestrings = dlist.get_namestrings(nlid, nid);
        let namedisschema = dlist.get_namedisschema(nlid, nid);
        let mut ulminyearflag = false;

        // uniquelist = minyear
        if ul == "minyear" {
          // minyear uniquename, we set based on the max/mincitenames list
          if (num_names > maxcn &&
              n.get_index() <= mincn) {
            ulminyearflag = true;
          }
        }

        let unall = dlist.get_uniquename_all(nlid, nid);

        // uniquename is not set so generate uniquelist based on just base name
        if (!defined(unall) || $unall->[0] == "base") {
          if let Some(basename) = basename {
            namelist.push(basename);
          }
          if ulminyearflag {
            ulminyear_namelist.push(basename);
          }
        }
        else {
          for (i, nss) in namedisschema.iter().enumerate() {
            if (Compare(nss, unall)) {
              if let Some(ns) = namestrings.get(i) {
                namelist.push(ns);
              }
              if ulminyearflag {
                ulminyear_namelist.push(namestrings[i]);
              }
            }
          }
        }

        dlist.add_uniquelistcount(namelist);
      }
      // We need to know the list uniqueness counts for the whole list separately otherwise
      // we will falsely "disambiguate" identical name lists from each other by setting
      // uniquelist to the full list because every part of each list will have more than
      // one count. We therefore need to distinguish counts which are of the final, complete
      // list of names. If there is more than one count for these, (meaning that there are
      // two or more identical name lists), we don't expand them at all as there is no point.
      dlist.add_uniquelistcount_final(namelist);

      // uniquelist=minyear needs tracking only of namelists in same labelyear
      if ul == "minyear" {
        dlist.add_uniquelistcount_final(namelist, labelyear);
      }

      // Add count for uniquelist=minyear
      if !(Compare(ulminyear_namelist, &[])) {
        dlist.add_uniquelistcount_minyear(ulminyear_namelist, labelyear, namelist);
      }
    }
  }

  /// Generate the per-namelist uniquelist values using the information
  /// harvested by create_uniquelist_info()
  fn generate_uniquelist(&self, dlist: &mut DataList) {
    let secnum = self.get_current_section();
    let section = self.sections().get_section(secnum);
    let bibentries = section.bibentries();

  'MAIN: for citekey in section.get_citekeys() {
      let be = bibentries.entry(citekey);
      let bee = be.get_field("entrytype");
      let labelyear = be.get_field("labelyear");
      let maxcn = crate::Config->getblxoption(secnum, "maxcitenames", bee, citekey);
      let mincn = crate::Config->getblxoption(secnum, "mincitenames", bee, citekey);
      let lni = match be.get_labelname_info() {
        None => continue, // only care about labelname
        Some(lni) => lni,
      }
      let nl = be.get_field(lni);
      let nlid = nl.get_id();

      let ul = crate::Config->getblxoption(secnum, "uniquelist", bee, citekey);
      // Per-namelist uniquelist
      ul = nl.get_uniquelist().or(ul);

      if ul == "false" {
        continue;
      }

        trace!("Creating uniquelist for '{}'", citekey);

      let mut namelist = Vec::new();
      let num_names = nl.count();

      for n in nl.names() {
        let nid = n.get_id();
        let basename = dlist.get_basenamestring(nlid, nid);
        let namestrings = dlist.get_namestrings(nlid, nid);
        let namedisschema = dlist.get_namedisschema(nlid, nid);

        let unall = dlist.get_uniquename_all(nlid, nid);

        // uniquename is not set so generate uniquelist based on just base name
        if unall.is_none() || unall[0] == "base" {
          if let Some(basename) = basename {
            namelist.push(basename);
          }
        }
        else {
          for (i, nss) in namedisschema.iter().enumerate() {
            if (Compare(nss, unall)) {
              if let Some(ns) = namestrings.get(i) {
                namelist.push(ns);
              }
            }
          }
        }

        // With uniquelist=minyear, uniquelist should not be set at all if there are
        // no other entries with the same max/mincitenames visible list and different years
        // to disambiguate from
        if (ul == "minyear" &&
            num_names > maxcn &&
            n.get_index() <= mincn &&
            dlist.get_uniquelistcount_minyear(namelist, labelyear) == 1) {
            trace!("Not setting uniquelist=minyear for '{}'", citekey);
          continue 'MAIN;
        }

        // list is unique after this many names so we set uniquelist to this point
        // Even if uniquelist=minyear, we record normal uniquelist information if
        // we didn't skip this key in the test above
        if dlist.get_uniquelistcount(namelist) == 1 {
          break;
        }
      }

        trace!("Setting uniquelist for '{}' using {}", citekey, namelist.join(","));

      dlist.set_uniquelist(nl, namelist, labelyear, ul, maxcn, mincn);
    }
  }

  /// Generate information for data which may changes per datalist
  fn generate_contextdata(&self, dlist: &mut DataList) {
    let secnum = self.get_current_section();
    let section = self.sections().get_section(secnum);
    let $dmh = crate::config::get_dm_helpers();

    // This loop critically depends on the order of the citekeys which
    // is why we have to do sorting before this
    for key in dlist.get_keys() {
      let be = section.bibentry(key);
      let bee = be.get_field("entrytype");
      let lni = be.get_labelname_info();

      // Sort any set members according to the list sorting order of the keys.
      // This gets the indices of the set elements in the sorted datalist, sorts
      // them numerically and then extracts the actual citekeys to make a new
      // entryset field value which we store in the list metadata until output time.
      if be.get_field("entrytype") == "set" {
        let @es;
        if (crate::Config->getblxoption(None, "sortsets")) {
          let setkeys: Vec<_> = be.get_field("entryset");
          let keys = dlist.get_keys();
          let sorted_setkeys = Vec::new();
          // Generate array of indices of set members in the main sorted datalist
          for elem in &setkeys {
            sorted_setkeys.push(keys.position(|v| v == elem));
          }
          // Sort the indices numerically (sorting has already been done so this is fine)
          // then get the actual citekeys using an array slice on the main sorted list
          @es = $keys->@[sort {$a <=> $b} @sorted_setkeys];
        }
        else {
          if be.get_field("entryset") {
            @es = be.get_field("entryset")->@*;
          }
        }
        dlist.set_entryfield(key, "entryset", \@es);
      }

      // Only generate extra* information if skiplab is not set.
      // Don't forget that skiplab is implied for set members
      if !(crate::Config->getblxoption(secnum, "skiplab", bee, key)) {
        // extraname
        if (let $labelnamehash = dlist.get_entryfield(key, "labelnamehash")) {
          if dlist.get_seen_labelname(labelnamehash) > 1 {
            let v = dlist.incr_seen_extraname(labelnamehash);
            dlist.set_extranamedata_for_key(key, v);
          }
        }
        // extradate
        if crate::Config->getblxoption(None, "labeldateparts", bee, key) {
          let namedateparts = dlist.get_entryfield(key, "namedateparts");
          if dlist.get_seen_namedateparts(namedateparts) > 1 {
              trace!("namedateparts for '{}': {}", namedateparts, dlist.get_seen_namedateparts(namedateparts));
            let v = dlist.incr_seen_extradate(namedateparts);
            dlist.set_extradatedata_for_key(key, v);
          }
        }
        // extratitle
        if crate::Config->getblxoption(None, "labeltitle", bee, key) {
          let nametitle = dlist.get_entryfield(key, "nametitle");
          if dlist.get_seen_nametitle(nametitle) > 1 {
              trace!("nametitle for '{}': {}", nametitle, dlist.get_seen_nametitle(nametitle));
            let v = dlist.incr_seen_extratitle(nametitle);
            dlist.set_extratitledata_for_key(key, v);
          }
        }
        // extratitleyear
        if crate::Config->getblxoption(None, "labeltitleyear", bee, key) {
          let titleyear = dlist.get_entryfield(key, "titleyear");
          if dlist.get_seen_titleyear(titleyear) > 1 {
              trace!("titleyear for '{}': {}", titleyear, dlist.get_seen_titleyear(titleyear));
            let v = dlist.incr_seen_extratitleyear(titleyear);
            dlist.set_extratitleyeardata_for_key(key, v);
          }
        }

        // labelalpha
        // This works because labelalpha field is regenerated per-list
        if (rate::Config->getblxoption(None, "labelalpha", bee, key) {
          dlist.set_labelalphadata_for_key(key, dlist.get_entryfield(key, "labelalpha"));
        }
        // extraalpha
        if crate::Config->getblxoption(None, "labelalpha", bee, key) {
          let la = dlist.get_entryfield(key, "labelalpha");
          if dlist.get_la_disambiguation(la) > 1 {
              trace!("labelalpha disambiguation for '{}': {}", la, dlist.get_la_disambiguation(la));
            let v = dlist.incr_seen_extraalpha(la);
            dlist.set_extraalphadata_for_key(key, v);
          }
        }
      }

      // uniquename
      for namefield in ($dmh->{namelists}->@*) {
        if (let nl = be.get_field(namefield)) {
          let nlid = nl.get_id();
          if !(lni.is_some() && lni == namefield) { // labelname only
            continue;
          }
          for n in nl.names() {
            let nid = n.get_id();
            let uniquename = dlist.get_uniquename(nlid, nid);
            if !uniquename {
              continue;
            }
            let namedisschema = dlist.get_namedisschema(nlid, nid);

            // Construct per-namepart uniquename value
            let pnun: HashMap<_, _>;
            for (i, nss) in namedisschema.iter().enumerate() {
              if (Compare(uniquename, nss)) {
                // Find where uniqueness is established, determine un settings up to this point
                let dis = namedisschema.[1..i-1].iter()
                  .filter(|v| v[0] != "base" && v[1] != "full")
                  .chain(namedisschema[i].iter());
                // normalise "fullonly" to "full" now that we have stripped all non-disambiguating elements
                pnun = dis.map(|v| (v[0], if v[1] == "fullonly" { "full" } else {v[1]})).collect();
                break;
              }
            }
            for np in n.get_nameparts() {
              let npun = UNIQUENAME_VALUES.get(pnun.get(np).unwrap_or("none"));
              let npun = npun.unwrap_or(0);
              dlist.set_unparts(nlid, nid, np, npun);
            }
          }
        }
      }
    }
  }

  /// Generate the singletitle field, if requested. The information for generating
  /// this is gathered in process_workuniqueness()
  fn generate_singletitle(&self, citekey: &str, dlist: &mut DataList) {
    let secnum = self.get_current_section();
    let section = self.sections().get_section(secnum);
    let bibentries = section.bibentries();
    let be = bibentries.entry(citekey);
    let bee = be.get_field("entrytype");

    if crate::Config->getblxoption(None, "singletitle", bee, citekey) {
      match dlist.get_entryfield(citekey, "seenname") {
        Some(sn) if dlist.get_seenname(sn) < 2 => {
          dlist.set_entryfield(citekey, "singletitle", 1);
        }
        _ => {}
      }
    }
  }

  /// Generate the uniquetitle field, if requested. The information for generating
  /// this is gathered in process_workuniqueness()
  fn generate_uniquetitle(&self, citekey: &str, dlist: &mut DataList) {
    let secnum = self.get_current_section();
    let section = self.sections().get_section(secnum);
    let bibentries = section.bibentries();
    let be = bibentries.entry(citekey);
    let bee = be.get_field("entrytype");

    if (crate::Config->getblxoption(None, "uniquetitle", bee, citekey)) {
      match dlist.get_entryfield(citekey, "seentitle") {
        Some(ut) if dlist.get_seentitle(ut) < 2 => {
          dlist.set_entryfield(citekey, "uniquetitle", 1);
        }
        _ => {}
      }
    }
  }

  /// Generate the uniquebaretitle field, if requested. The information for generating
  /// this is gathered in process_workuniqueness()
  fn generate_uniquebaretitle(&self, citekey: &str, &mut DataList) {
    let secnum = self.get_current_section();
    let section = self.sections().get_section(secnum);
    let bibentries = section.bibentries();
    let be = bibentries.entry(citekey);
    let bee = be.get_field("entrytype");

    if (crate::Config->getblxoption(None, "uniquebaretitle", bee, citekey)) {
      match dlist.get_entryfield(citekey, "seenbaretitle") {
        Some(ubt) if dlist.get_seenbaretitle(ubt) < 2 => {
          dlist.set_entryfield(citekey, "uniquebaretitle", 1);
        }
        _ => {}
      }
    }
  }

  /// Generate the uniquework field, if requested. The information for generating
  /// this is gathered in process_workuniqueness()
  fn generate_uniquework(&self, citekey: &str, &mut DataList) {
    let secnum = self.get_current_section();
    let section = self.sections().get_section(secnum);
    let bibentries = section.bibentries();
    let be = bibentries.entry(citekey);
    let bee = be.get_field("entrytype");

    if (crate::Config->getblxoption(None, "uniquework", bee, citekey)) {
      if dlist.get_entryfield(citekey, "seenwork") &&
          dlist.get_seenwork(dlist.get_entryfield(citekey, "seenwork")) < 2 {
          trace!("Setting uniquework for '{}'", citekey);
        dlist.set_entryfield(citekey, "uniquework", 1);
      }
      else {
          trace!("Not setting uniquework for '{}'", citekey);
      }
    }
  }

  /// Generate the uniqueprimaryauthor field, if requested. The information for generating
  /// this is gathered in create_uniquename_info()
  fn generate_uniquepa(&self, citekey: &str, dlist: &mut DataList) {
    let secnum = self.get_current_section();
    let section = self.sections().get_section(secnum);
    let bibentries = section.bibentries();
    let be = bibentries.entry(citekey);
    let bee = be.get_field("entrytype");

    if (crate::Config->getblxoption(None, "uniqueprimaryauthor", bee, citekey)) {
      if dlist.get_entryfield(citekey, "seenprimaryauthor") &&
          dlist.get_seenpa(dlist.get_entryfield(citekey, "seenprimaryauthor")) < 2 {
          trace!("Setting uniqueprimaryauthor for '{}'", citekey);
        dlist.set_entryfield(citekey, "uniqueprimaryauthor", 1);
      }
      else {
          trace!("Not setting uniqueprimaryauthor for '{citekey}'");
      }
    }
  }

  /// Sort a list using information in entries according to a certain sorting template.
  /// Use a flag to skip info messages on first pass
  fn sort_list(self, $dlist) {
    let $sortingtemplate = $dlist->get_sortingtemplate;
    let $lsds  = $dlist->get_sortdataschema;
    let keys = dlist.get_keys();
    let lstn = dlist.get_sortingtemplatename();
    let ltype = dlist.get_type();
    let lname = dlist.get_name();
    let $llocale = locale2bcp47($sortingtemplate->{locale} || crate::Config->getblxoption(None, "sortlocale"));
    let secnum = self.get_current_section();
    let section = self.sections().get_section(secnum);

      if (crate::Config->getoption("sortcase")) {
        debug!("Sorting is by default case-SENSITIVE");
      }
      else {
        debug!("Sorting is by default case-INSENSITIVE");
      }
      debug!("Keys before sort:\n");
      for k in keys {
        debug!("{} => {}", k,  dlist.get_sortdata_for_key(k)->[0]);
      }

      trace!("Sorting datalist '{lname}' of type '{ltype}' with sortingtemplate '{lstn}'. Scheme is\n-------------------\n{sortingtemplate:?}\n-------------------\n");
    // Set up locale. Order of priority is:
    // 1. locale value passed to Unicode::Collate::Locale->new() (Unicode::Collate sorts only)
    // 2. Biber sortlocale option
    // 3. Sorting "locale" option
    // 4. Global biblatex "sortlocale" option

    let $thislocale = crate::Config->getoption("sortlocale") || $llocale;
      debug!("Locale for sorting is '{}'", thislocale);

    if ( crate::Config->getoption("fastsort") ) {
      biber_warn("fastsort option no longer required/supported, defaulting to UCA");
    }

    let $collopts = crate::Config->getoption("collate_options");

    // UCA level 2 if case insensitive sorting is requested
    if !(crate::Config->getoption("sortcase")) {
      $collopts->{level} = 2;
    }

    // Add upper_before_lower option
    $collopts->{upper_before_lower} = crate::Config->getoption("sortupper");

    // Create collation object

    let $Collator = crate::UCollate->new($thislocale, $collopts->%*);

    let $UCAversion = $Collator->version();
    info!("Sorting list '{}' of type '{}' with template '{}' and locale '{}'", lname, ltype, lstn, thislocale);
      debug!("Sorting with Unicode::Collate ({}, UCA version: {}, Locale: {})", stringify_hash($collopts), UCAversion, $Collator->getlocale);

    // Log if U::C::L currently has no tailoring for used locale
    if ($Collator->getlocale == "default") {
      info!("No sort tailoring available for locale '{}'", thislocale);
    }

    // For collecting the collation object settings for retrieval in the sort key extractor
    let mut collateobjs = Vec::new();

    // Instantiate Sort::Key sorter with correct data schema
    let $sorter = multikeysorter(map {$_->{spec}} $lsds->@*);

    // Sorting cache to shortcut expensive UCA keygen
    let $cache;

    // Construct data needed for sort key extractor
    for sortset in ($sortingtemplate->{spec}->@*) {
      let mut fc = Vec::new();

      // Re-instantiate collation object if a different locale is required for this sort item.
      // This can't be done in a ->change() method, has to be a new object.
      let $sl = locale2bcp47($sortset->[0]{locale});
      let cobj = if (defined($sl) && $sl != $thislocale) {
        format!("crate::UCollate->new('{}','{}')", sl, ($collopts->%*).join("','"))
      }
      else {
        Collator.to_string()
      };

      // If the case or upper option on a field is not the global default
      // set it locally on the $Collator by constructing a change() method call
      let $sc = $sortset->[0]{sortcase};
      if (defined($sc) && $sc != crate::Config->getoption("sortcase")) {
        fc.push(if sc { "level => 4" } else { "level => 2" });
      }
      let $su = $sortset->[0]{sortupper};
      if (defined($su) && $su != crate::Config->getoption("sortupper")) {
        fc.push(if su { "upper_before_lower => 1" } else { "upper_before_lower => 0" });
      }

      let fc = if !fc.is_empty() {
        // This field has custom collation options
        format!("->change({})", fc.join(","))
      }
      else {
        // Reset collation options to global defaults if there are no field options
        // We have to do this as ->change modifies the Collation object
        format!("->change(level => {} ,upper_before_lower => {})", $collopts->{level}, $collopts->{upper_before_lower})
      };

      collateobjs.push(format!("{cobj}{fc}"));
    }

    // Sort::Key sort key extractor called on each element of array to be sorted and
    // returns an array of the sorting keys for each sorting field. We have to construct
    // the collator strings and then eval() because passing the collation
    // objects in directly by reference means that the wrong settings are present on some of them
    // since they point to the same object and the ->change() calls in later references
    // therefore change earlier sorting field sorts. So, we have to defer until actual use time.
    let $extract = || {
      let @d;
      let $key = $keys[$_];
      // Loop over all sorting fields
      for (i, sortfield) in dlist.get_sortdata_for_key($key)[1].iter().enumerate() {
        // Resolve real zeros back again
        if ($lsds->[$i]{int}) {
          // There are special cases to be careful of here in that "final" elements
          // in sorting copy themselves as strings to further sort fields and therefore
          // need coercing to 0 for int tests. Fallback of '0' for int fields should
          // be handled in the sorting spec otherwise this will be the default for missing
          // int fields. This means that entries with missing data for an int sort field will
          // always sort after int fields by default.

          // normalise all strings to a large int so that they sort after real ints
          // as a fallback
          push @d, looks_like_number($sortfield) ? $sortfield : 2000000000;
        }
        else {
          // Don't do '$sortfield' || "$sortfield" because it might contain quotes
          let $a = $collateobjs[$i] . "->getSortKey(q{$sortfield})";
          // Cache index is just the collation object opts and key gen call in string form
          // since this should be unique for a key/collopts combination
          push @d, $cache->{$a} ||= eval $a;
        }
      }
      return @d;
    };

    // We actually sort the indices of the keys array, as we need these in the extractor.
    // Then we extract the real keys with a map. This therefore follows the typical ST sort
    // semantics (plus an OM cache above due to expensive UCA key extraction).
    @keys = map {$keys[$_]} &$sorter($extract, 0..$#keys);

      debug!("Keys after sort:\n");
      for k in (@keys) {
        debug!("{} => {}", k, $dlist->get_sortdata_for_key($k)->[0]);
      }

    $dlist->set_keys([ @keys ]);

    return;
  }

  /// Preprocessing for options. Used primarily to perform process-intensive
  /// operations which can be done once instead of inside dense loops later.
  fn preprocess_options(&mut self) {

    // nosort - compile regexps
    if (let $nosort = crate::Config->getoption("nosort")) {
      for nsopt in ($nosort->@*) {
        let $re = $nsopt->{value};
        $nsopt->{value} = qr/$re/;
      }
    }

    // nonamestring - compile regexps
    if (let $nonamestring = crate::Config->getoption("nonamestring")) {
      for nnopt in ($nonamestring->@*) {
        let $re = $nnopt->{value};
        $nnopt->{value} = qr/$re/;
      }
    }

    // nolabel - compile regexps
    if (let $nolabel = crate::Config->getoption("nolabel")) {
      for nsopt in ($nolabel->@*) {
        let $re = $nsopt->{value};
        $nsopt->{value} = qr/$re/;
      }
    }

    // noinit - compile regexps
    if (let $noinit = crate::Config->getoption("noinit")) {
      for nsopt in ($noinit->@*) {
        let $re = $nsopt->{value};
        $nsopt->{value} = qr/$re/;
      }
    }

    return;
  }

  /// Do the main work.
  /// Process and sort all entries before writing the output.
  fn prepare(&mut self) {
    let out = self.get_output_obj();          // crate::Output object

    // Place to put global pre-processing things
    self.process_setup();

    for in section in &self.sections().get_sections() {
      // shortcut - skip sections that don't have any keys
      if section.get_citekeys().is_empty() && !section.is_allkeys() {
        continue;
      }
      let secnum = section.number();

      info!("Processing section {}", secnum);

      section.reset_caches();              // Reset the the section caches
      crate::config::_init();                // (re)initialise Config object
      self.set_current_section(secnum); // Set the section number we are working on
      self.preprocess_options();           // Preprocess any options
      self.fetch_data();                   // Fetch cited key and dependent data from sources
      self.process_citekey_aliases();      // Remove citekey aliases from citekeys
      self.instantiate_dynamic();          // Instantiate any dynamic entries (sets, related)
      self.resolve_alias_refs();           // Resolve xref/crossref/xdata aliases to real keys
      $self->resolve_xdata;                // Resolve xdata entries
      $self->cite_setmembers;              // Cite set members
      $self->preprocess_sets;              // Record set information
      $self->calculate_interentry;         // Calculate crossrefs/xrefs etc.
      $self->process_interentry;           // Process crossrefs/xrefs etc.
      $self->validate_datamodel;           // Check against data model
      $self->postprocess_sets;             // Add options to set members etc.
      $self->process_entries_static;       // Generate static entry data not dependent on lists
      $self->process_lists;                // Process the output lists
      $out->create_output_section;         // Generate and push the section output into the
                                          // output object ready for writing
    }
    out.create_output_misc();              // Generate and push the final misc bits of output
                                          // into the output object ready for writing
    return;
  }

  /// Do the main work for tool mode
  fn prepare_tool(&mut self) {
    let out = self.get_output_obj();          // crate::Output object
    out.clear_output_macros();                // Mostly for tool mode tests
    out.clear_output_comments();              // Mostly for tool mode tests

    // Place to put global pre-processing things
    self.process_setup_tool();

    // tool mode only has a section "99999"
    let secnum = 99999;
    let section = self.sections().get_section(secnum);

    section.reset_caches(); // Reset the the section caches (sorting, label etc.)
    crate::config::_init();   // (re)initialise Config object
    self.set_current_section(secnum); // Set the section number we are working on
    self.preprocess_options();           // Preprocess any options
    self.fetch_data();      // Fetch cited key and dependent data from sources

    self.resolve_alias_refs();   // Resolve xref/crossref/xdata aliases to real keys
    self.preprocess_sets();      // Record set information
    self.calculate_interentry(); // Calculate crossrefs/xrefs etc.
    self.process_interentry();   // Process crossrefs/xrefs etc.
    self.resolve_xdata();        // Resolve xdata entries

    self.validate_datamodel();   // Check against data model
    self.process_lists();        // process the output lists (sort and filtering)
    out.create_output_section(); // Generate and push the section output into the
                                // into the output object ready for writing
    return;
  }

  /// Fetch citekey and dependents data from section datasources
  /// Expects to find datasource packages named:
  ///
  /// crate::Input::<type>::<datatype>
  ///
  /// and one defined subroutine called:
  ///
  /// crate::Input::<type>::<datatype>::extract_entries
  ///
  /// which takes args:
  ///
  /// 1: Biber object
  /// 2: Datasource name
  /// 3: Reference to an array of cite keys to look for
  ///
  /// and returns an array of the cite keys it did not find in the datasource
  fn fetch_data(&mut self) {
    let secnum = self.get_current_section();
    let section = self.sections().get_section(secnum);
    let $dm = crate::config::get_dm();
    // Only looking for static keys, dynamic key entries are not in any datasource ...
    let citekeys = section.get_static_citekeys();
    no strict "refs"; // symbolic references below ...

    // Clear all T::B macro definitions between sections if asked as T::B never clears these
    if (crate::Config->getoption("clrmacros")) {
        debug!("Clearing Text::BibTeX macros definitions");
      Text::BibTeX::delete_all_macros();
    }

    // (Re-)define the old BibTeX month macros to what biblatex wants unless user stops this
    if !(crate::Config->getoption("nostdmacros")) {
      for (mon, monn) in &MONTHS {
        Text::BibTeX::delete_macro(mon);
        Text::BibTeX::add_macro_text(mon, monn);
      }
    }

    // First we look for the directly cited keys in each datasource
    let @remaining_keys = @citekeys;
      debug!("Looking for directly cited keys: {}", remaining_keys.join(", "));

    // Process datasource globs
    let mut ds = Vec::new();
    for datasource in section.get_datasources() {
      if datasource.typ != DataSourceType::File {
        ds.push(datasource);
      }
      for gds in (glob_data_file(datasource.name, datasource.glob)) {
        ds.push(DataSource {
          typ: datasource.typ,
          name: gds,
          datatype: datasource.datatype,
          encoding: datasource.encoding,
          glob: None,
        });
      }
    }
    section.set_datasources(ds);

    // Now actually fetch data with expanded list of data sources
    for datasource in section.get_datasources() {
      // shortcut if we have found all the keys now
      if !(@remaining_keys || section.is_allkeys()) {
        break;
      }
      let typ = datasource.typ;
      let name = datasource.name;
      let encoding = datasource.encoding;
      let datatype = datasource.datatype;
      if datatype == InputFormat::BibLaTeXML {
        let $outfile;
        if (crate::Config->getoption("tool")) {
          let $exts = DS_EXTENSIONS.values().join("|");
          $outfile = crate::Config->getoption("dsn") =~ s/\.(?:$exts)$/.rng/r;
        }
        else {
          $outfile = crate::Config->getoption("bcf") =~ s/bcf$/rng/r;
        }

        // Generate schema for datasource
        if !(crate::Config->getoption("no_bltxml_schema")) {
          dm.generate_bltxml_schema(outfile);
        }

        if (crate::Config->getoption("validate_bltxml")) {
          validate_biber_xml($name, "bltx", "http://biblatex-biber.sourceforge.net/biblatexml", $outfile);
        }
      }
      let $package = "crate::Input::" . typ . "::" . $datatype;
      if !(eval "require $package") {

        let (vol, mut dir, _) = File::Spec->splitpath( $INC{"Biber.pm"} );
        if let Some(d) = dir.strip_suffix('/') {
          dir = d.into(); // splitpath sometimes leaves a trailing '/'
        }        

        // Use Windows style globbing on Windows
        if ($^O =~ /Win/) {
          debug!("Enabling Windows-style globbing");
          require File::DosGlob;
          File::DosGlob->import("glob");
        }

        let mut vts = Vec::new();
        for t in (glob("$vol$dir/Biber/Input/*")) {
          let (_, _, tleaf) = File::Spec->splitpath(t);
          for dt in (map {s/\.pm$//r} glob("$vol$dir/Biber/Input/$tleaf/*.pm")) {
            let (_, _, dtleaf) = File::Spec->splitpath(dt);
            vts.push(format!("{tleaf}/{dtleaf}"));
          }
        }

        biber_error("Error loading data source package '{package}' for '{datatype}' '{typ}' datasource. Valid type/datatypes are: {}", vts.join(","));

      }

      // Slightly different message for tool mode
      if (crate::Config->getoption("tool")) {
        info!("Looking for {datatype} {typ} '{name}'");
      }
      else {
        info!("Looking for {datatype} {typ} '{name}' for section {secnum}");
      }

      @remaining_keys = "${package}::extract_entries"->(locate_data_file($name), $encoding, \@remaining_keys);
    }

    // error reporting
      debug!("Directly cited keys not found for section '{secnum}': {}", remaining_keys.join(","));

    for citekey in (@remaining_keys) {
      biber_warn("I didn't find a database entry for '{citekey}' (section {secnum})");
      section.del_citekey(citekey);
      section.add_undef_citekey(citekey);
    }

      debug!("Building dependents for keys: {}", section.get_citekeys().join(","));

    // dependent key list generation - has to be a sub as it's recursive to catch
    // nested crossrefs, xdata etc.
    // We still do this even in tool mode which is implicitly allkeys=1 because it
    // prunes things like missing crossrefs etc. which otherwise would cause problems
    // later on
    self.get_dependents(section.get_citekeys());
      debug!("Citekeys for section '{}' after fetching data: {}", secnum, section.get_citekeys().join(", "));
    return;
  }

  /// Get dependents of the entries for a given list of citekeys. Is called recursively
  /// until there are no more dependents to look for.
  fn get_dependents(self, keys: &[String], $keyswithdeps, $missing) {
    let secnum = self.get_current_section();
    let section = self.sections().get_section(secnum);
    let new_deps = Vec::new();

    $keyswithdeps = $keyswithdeps.unwrap_or([]);
    $missing = $missing.unwrap_or([]);

    no strict "refs"; // symbolic references below ...

    for citekey in keys {
      // aliases need resolving here and are treated as dependents
      if (let $real = section.get_citekey_alias(citekey)) {
          debug!("Alias '{}' requires real key '{}'", citekey, real);
        new_deps.push(real);
        if !keyswithdeps.contains(real) {
          keyswithdeps.push(real);
        }
      }
      // Dynamic sets don't exist yet but their members do
      else if (let @dmems = section.get_dynamic_set(citekey)) {
        // skip looking for dependent if it's already there
        for dm in (@dmems) {
          if !section.bibentry(dm) {
            new_deps.push(dm);
            if !keyswithdeps.contains(citekey) {
              keyswithdeps.push(citekey);
            }
          }
        }
          debug!("Dynamic set entry '{}' has members: {}", citekey, dmems.join(", "));
      }
      else {
        // This must exist for all but dynamic sets
        let be = section.bibentry(citekey);

        // xdata
        if (let $xdata = be.get_xdata_refs()) {
          for xdatum in ($xdata->@*) {
            for xdref in ($xdatum->{xdataentries}->@*) {
              // skip looking for dependent if it's already there (loop suppression)
              if !section.bibentry(xdref) {
                new_deps.push(xdref);
              }
                debug!("Entry '{}' has xdata '{}'", citekey, xdref);
              if !keyswithdeps.contains(citekey) {
                keyswithdeps.push(citekey);
              }
            }
          }
        }

        // xrefs
        if (let $refkey = be.get_field("xref")) {
          // skip looking for dependent if it's already there (loop suppression)
          if !section.bibentry(refkey) {
            new_deps.push(refkey);
          }
            debug!("Entry '{}' has xref '{}'", citekey, refkey);
          if !keyswithdeps.contains(citekey) {
            keyswithdeps.push(citekey);
          }
        }

        // crossrefs
        if (let $refkey = be.get_field("crossref")) {
          // skip looking for dependent if it's already there (loop suppression)
          if !section.bibentry(refkey) {
            new_deps.push(refkey);
          }
            debug!("Entry '{}' has crossref '{}'", citekey, refkey);
          if !keyswithdeps.contains(citekey) {
            keyswithdeps.push(citekey);
          }
        }

        // static sets
        if be.get_field("entrytype") == "set" {
          let smems = be.get_field("entryset");
          // skip looking for dependent if it's already there (loop suppression)
          for sm in smems {
            if !section.has_citekey(sm) {
              new_deps.push(sm);
              if !keyswithdeps.contains(citekey) {
                keyswithdeps.push(citekey);
              }
            }
          }
            debug!("Static set entry '{}' has members: {}", citekey, smems.join(", "));
        }

        // Related entries
        if (let $relkeys = be.get_field("related")) {
          // skip looking for dependent if it's already there (loop suppression)
          for rm in relkeys {
            if !(section.has_citekey(rm) || section.is_related(rm)) {
              // record that $rm is used as a related entry key
              section.add_related(rm);
              new_deps.push(rm);
              if !keyswithdeps.contains(citekey) {
                keyswithdeps.push(citekey);
              }
            }
          }
            debug!("Entry '{}' has related entries: {}", citekey, relkeys.join(", "));
        }
      }
    }

    // Remove repeated keys which are dependents of more than one entry
    $new_deps->@* = uniq $new_deps->@*;

    if !new_deps.is_empty() {
      // Now look for the dependents of the directly cited keys
        debug!("Looking for dependent keys: {}", new_deps.join(", "));

      // No need to go back to the datasource if allkeys, just see if the keys
      // are in section
      if section.is_allkeys() {
        for dk in &new_deps {
          if !section.has_citekey(dk) {
            missing.push(dk);
          }
        }
      }
      else {
        $missing->@* = $new_deps->@*;
        for datasource in section.get_datasources() {
          // shortcut if we have found all the keys now
          if missing.is_empty() {
            break;
          }
          let $type = $datasource->{type};
          let $name = $datasource->{name};
          let $encoding = $datasource->{encoding};
          let $datatype = $datasource->{datatype};
          let $package = "crate::Input::" . $type . "::" . $datatype;
          eval "require $package" ||
            biber_error("Error loading data source package '$package': $@");
          $missing->@* = "${package}::extract_entries"->(locate_data_file($name), $encoding, $missing);
        }
      }

        debug!("Dependent keys not found for section '{}': {}", secnum, missing.join(", "));
      for missing_key in ($missing->@*) {
        // Remove the missing key from the list to recurse with
        new_deps.retain(|k| k!= $missing_key);
      }
    }

    // recurse if there are more things to find
      trace!("Recursing in get_dependents with: {}", new_deps.join(", "));

    if !new_deps.is_empty() {
      self.get_dependents(new_deps, $keyswithdeps);
    }

    // Now remove any missing entries from various places in all entries we have flagged
    // as having dependendents. If we don't do this, many things fail later like clone creation
    // for related entries etc.
    for keywithdeps in ($keyswithdeps->@*) {
      for missing_key in ($missing->@*) {
        $self->remove_undef_dependent($keywithdeps, $missing_key);
      }
    }

    return; // bottom of recursion
  }

  /// Remove undefined dependent keys from an entry using a map of
  /// dependent keys to entries
  fn remove_undef_dependent(&mut self, citekey: &str, missing_key: &str) {
    let secnum = self.get_current_section();
    let section = self.sections().get_section(secnum);
      debug!("Removing dependency on missing key '{}' from '{}' in section '{}'", missing_key, citekey, secnum);

    // remove from any dynamic keys
    let dmems = section.get_dynamic_set(citekey);
    if !dmems.is_empty() {
      if dmems.contains(missing_key) {
        section.set_dynamic_set(citekey, dmems.iter().filter(|s| s != missing_key).map(|s| s.as_str()));
          trace!("Removed dynamic set dependency for missing key '{}' from '{}' in section '{}'", missing_key, citekey, secnum);
        biber_warn("I didn't find a database entry for dynamic set member '$missing_key' - ignoring (section $secnum)");
      }
    }
    else {
      let be = section.bibentry(citekey);

      // remove any xrefs
      if be.get_field("xref") && (be.get_field("xref") == missing_key) {
        biber_warn("I didn't find a database entry for xref '$missing_key' in entry '$citekey' - ignoring (section $secnum)");

          trace!("Removed xref dependency for missing key '{}' from '{}' in section '{}'", missing_key, citekey, secnum);

        if (!crate::Config->getoption("tool_noremove_missing_dependants")) {
          be.del_field("xref");
        }
      }

      // remove any crossrefs
      if be.get_field("crossref") && (be.get_field("crossref") == missing_key) {
        biber_warn("I didn't find a database entry for crossref '$missing_key' in entry '$citekey' - ignoring (section $secnum)");

          trace!("Removed crossref dependency for missing key '{}' from '{}' in section '{}'", missing_key, citekey, secnum);

        if (!crate::Config->getoption("tool_noremove_missing_dependants")) {
          be.del_field("crossref");
        }
      }

      // remove xdata
      if (let xdata = be.get_field("xdata")) {
        if xdata.contains(missing_key) {
          biber_warn("I didn't find a database entry for xdata entry '$missing_key' in entry '$citekey' - ignoring (section $secnum)");
        }

          trace!("Removed xdata dependency for missing key '$missing_key' from '$citekey' in section '$secnum'");

        if (!crate::Config->getoption("tool_noremove_missing_dependants")) {
          be.set_datafield("xdata", xdata.iter().filter(|k| k != missing_key));
        }
      }

      // remove static sets
      if (be.get_field("entrytype") == "set") {
        let $smems = be.get_field("entryset");
        if smems.contains(missing_key) {
          be.set_datafield("entryset", smems.iter().filter(|k| k != missing_key));
            trace!("Removed static set dependency for missing key '{}' from '{}' in section '{}'", missing_key, citekey, secnum);
          biber_warn("I didn't find a database entry for static set member '$missing_key' in entry '$citekey' - ignoring (section $secnum)");
        }
      }

      // remove related entries
      if (let relkeys = be.get_field("related")) {
        if relkeys.contains(missing_key) {
          be.set_datafield("related", relkeys.iter().filter(|k| k != missing_key));
          // If no more related entries, remove the other related fields
          if !(be.get_field("related")) {
            be.del_field("relatedtype");
            be.del_field("relatedstring");
              trace!("Removed related entry dependency for missing key '{}' from '{}' in section '{}'", missing_key, citekey, secnum);
          }
          biber_warn("I didn't find a database entry for related entry '$missing_key' in entry '$citekey' - ignoring (section $secnum)");
        }
      }
    }
      return;
  }

  /// Convenience sub to parse a .bcf sorting section and return nice
  /// sorting object
  fn _parse_sort(root_obj) {
    let $sorting;

    for sort in (sort {$a->{order} <=> $b->{order}} $root_obj->{sort}->@*) {
      let $sortingitems;

      // Generate sorting pass structures
      for sortitem in (sort {$a->{order} <=> $b->{order}} $sort->{sortitem}->@*) {
        let $sortitemattributes = {};
        if (defined($sortitem->{substring_side})) { // Found sorting substring side attribute
          $sortitemattributes->{substring_side} = $sortitem->{substring_side};
        }
        if (defined($sortitem->{substring_width})) { // Found sorting substring length attribute
          $sortitemattributes->{substring_width} = $sortitem->{substring_width};
        }
        if (defined($sortitem->{pad_width})) { // Found sorting pad length attribute
          $sortitemattributes->{pad_width} = $sortitem->{pad_width};
        }
        if (defined($sortitem->{pad_char})) { // Found sorting pad char attribute
          $sortitemattributes->{pad_char} = $sortitem->{pad_char};
        }
        if (defined($sortitem->{pad_side})) { // Found sorting pad side attribute
          $sortitemattributes->{pad_side} = $sortitem->{pad_side};
        }
        if (defined($sortitem->{literal})) { // Found literal attribute
          $sortitemattributes->{literal} = $sortitem->{literal};
        }
        push $sortingitems->@*, {$sortitem->{content} => $sortitemattributes};
      }

      // Only push a sortitem if defined.
      // Also, we only push the sort attributes if there are any sortitems otherwise
      // we end up with a blank sort
      let $sopts;
      if defined($sort->{final}) {
        $sopts->{final} = $sort->{final};
      }
      if defined($sort->{sort_direction}) {
        $sopts->{sort_direction} = $sort->{sort_direction};
      }
      if defined($sort->{sortcase}) {
        $sopts->{sortcase} = $sort->{sortcase};
      }
      if defined($sort->{sortupper}) {
        $sopts->{sortupper} = $sort->{sortupper};
      }
      if defined($sort->{locale}) {
        $sopts->{locale} = $sort->{locale};
      }
      if (defined($sortingitems)) {
        sortingitems.insert(0, sopts);
        sorting.push(sortingitems);
      }
    }

    return {locale => locale2bcp47($root_obj->{locale} || crate::Config->getblxoption(None, "sortlocale")),
            spec   => $sorting};
  }

  /// Dump the biber object with Data::Dump for debugging
  fn _filedump(self, $file) {
    let $fh = IO::File->new($file, '>').expect(format!("Can't open file {} for writing", file));
    print $fh Data::Dump::pp($self);
    close $fh;
    return
  }

  fn _stringdump(self) {
    return Data::Dump::pp($self);
  }
}