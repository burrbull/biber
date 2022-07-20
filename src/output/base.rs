/*
use crate::Entry;
use crate::Utils;
use Encode;
use IO::File;
use Text::Wrap;
$Text::Wrap::columns = 80;
use Log::Log4perl qw( :no_extra_logdie_message );
use Unicode::Normalize;
*/

/// Base class for Biber output modules.
pub struct Base;

impl Base {
  /// Initialize a crate::Output::base object
  fn new(obj) -> Self {
    let $self;
    if (defined($obj) && ref($obj) == "HASH") {
      $self = bless $obj, $class;
    }
    else {
      $self = bless {}, $class;
    }

    $self->{output_data}{HEAD} = "";
    $self->{output_data}{TAIL} = "";

    return $self;
  }

  /// Set the output target file of a crate::Output::base object
  /// A convenience around set_output_target so we can keep track of the
  /// filename. Returns an IO::File object for the target
  fn set_output_target_file(self, $file, $init) {

    $self->{output_target_file} = $file;
    let $enc_out;
    if (let $enc = crate::Config->getoption("output_encoding")) {
      $enc_out = ":encoding($enc)";
    }
    return IO::File->new($file, ">$enc_out");
  }

  /// Get the output target file name
  fn get_output_target_file(self) {
    return $self->{output_target_file};
  }

  /// Set the output target of a crate::Output::base object
  fn set_output_target(self, target) {
    self.output_target = target;
    return;
  }

  /// Set the output head of a crate::Output::base object
  /// $data could be anything - the caller is expected to know.
  fn set_output_head(self, data) {
    self.output_data.HEAD = data;
    return;
  }

  /// Set the output tail of a crate::Output::base object
  /// $data could be anything - the caller is expected to know.
  fn set_output_tail(self, data) {
    self.output_data.TAIL = data;
    return;
  }

  /// Get the output head of a crate::Output object
  /// $data could be anything - the caller is expected to know.
  /// Mainly used in debugging
  fn get_output_head(&self) -> &String {
    self.output_data.HEAD
  }

  /// Get the output tail of a crate::Output object
  /// $data could be anything - the caller is expected to know.
  /// Mainly used in debugging
  fn get_output_tail(&self) -> &String {
    self.output_data.TAIL
  }

  /// Add to the head output data of a crate::Output::base object
  /// The base class method just does a string append
  fn add_output_head(&mut self, data: &str) {
    self.output_data.HEAD.push_str(data);
  }

  /// Add to the tail output data of a crate::Output::base object
  /// The base class method just does a string append
  fn add_output_tail(&mut self, data: &str) {
    self.output_data.TAIL.push_str(data);
  }

  /// Records the section object in the output object
  /// We need some information from this when writing the output
  fn set_output_section(self, secnum, section) {
    $self->{section}{$secnum} = $section;
    return;
  }

  /// Retrieve the output section object
  fn get_output_section(self, secnum) {
    return $self->{section}{$secnum};
  }

  /// Get the sorted order output data for all entries in a list as array ref
  /// Used really only in tests as it instantiates list dynamic information so
  /// we can see it in tests. As a result, we have to NFC() the result to mimic
  /// real output since UTF-8 output is assumed in most tests.
  fn get_output_entries(self, section, list) {
    return [ map {$self->{output_data}{ENTRIES}{$section}{index}{$_} ||
                  $self->{output_data}{MISSING_ENTRIES}{$section}{index}{$_} ||
                  $self->{output_data}{ALIAS_ENTRIES}{$section}{index}{$_}} list.get_keys()];
  }

  /// Get the output macros for tool mode tests
  fn get_output_macros(self) {
    return [sort $self->{output_data}{MACROS}->@*];
  }

  /// Get the output comments for tool mode tests
  fn get_output_comments(self) {
    return [sort $self->{output_data}{COMMENTS}->@*];
  }

  /// Clear the output macros
  fn clear_output_macros(&mut self) {
    delete $self->{output_data}{MACROS};
  }

  /// Clear the output comments
  fn clear_output_comments(&mut self) {
    delete $self->{output_data}{COMMENTS};
  }

  /// Get the output data for a specific entry.
  /// Used really only in tests as it instantiates list dynamic information so
  /// we can see it in tests. As a result, we have to NFC() the result to mimic
  /// real output since UTF-8 output is assumed in most tests.
  fn get_output_entry(self, $key, $list, $secnum) {

    // defaults - mainly for tests
    if (!defined($secnum)) {
      if (crate::Config->getoption("tool") ||
          crate::Config->getoption("output_format") == "bibtex") {
        $secnum = 99999;
      }
      else {
        $secnum = 0;
      }
    }

    let $section = $self->get_output_section($secnum);

    // Force a return of undef if there is no output for this key to avoid
    // dereferencing errors in tests
    let $out = $self->{output_data}{ENTRIES}{$secnum}{index}{$key} ||
              $self->{output_data}{MISSING_ENTRIES}{$secnum}{index}{$key} ||
              $self->{output_data}{ALIAS_ENTRIES}{$secnum}{index}{$key};
    let $out_string = $list ? $list->instantiate_entry($section, $out, $key) : $out;

    // If requested to convert UTF-8 to macros ...
    if (crate::Config->getoption("output_safechars")) {
      $out_string = latex_recode_output($out_string);
    }
    else { // ... or, check for encoding problems and force macros
      let $outenc = crate::Config->getoption("output_encoding");
      if ($outenc != "UTF-8") {
        // Can this entry be represented in the output encoding?
        if (encode($outenc, NFC($out_string)) =~ /\?/) { // Malformed data encoding char
          // So convert to macro
          $out_string = latex_recode_output($out_string);
          biber_warn("The entry '$key' has characters which cannot be encoded in '$outenc'. Recoding problematic characters into macros.");
        }
      }
    }

    // Sometimes $out_string might still be a scalar ref (tool mode, for example which doesn't use
    // sort lists)
    if $out {
      if ref($out_string) == "SCALAR" {
        NFC($$out_string)
      } else {
        NFC($out_string)
      }
    } else {
      None
    }
  }

  /// Add an entry output to a crate::Output::base object
  /// The base class method just does a dump
  fn set_output_entry(self, entry, secnum, struc) {
    $self->{output_data}{ENTRIES}{$secnum}{index}{$entry->get_field("citekey")} = $entry->dump;
    return;
  }

  /// Create the output for misc bits and pieces like preamble and closing
  /// macro call and add to output object.
  fn create_output_misc(&mut self) {
    return;
  }

  /// Create the output from the sections data and push it into the
  /// output object.
  fn create_output_section(self) {
    let secnum = crate::MASTER.get_current_section();
    let section = crate::MASTER.sections().get_section(secnum);

    // We rely on the order of this array for the order of the ouput
    for k in section.get_citekeys() {
      // Regular entry
      let be = section.bibentry(k) || biber_error("Cannot find entry with key '$k' to output");
      $self->set_output_entry(be, section, crate::config::get_dm());
    }

    // Make sure the output object knows about the output section
    $self->set_output_section($secnum, $section);

    // undef citekeys are global to a section
    // Missing citekeys
    for k in ($section->get_undef_citekeys) {
      $self->set_output_undefkey($k, $section);
    }

    // alias citekeys are global to a section
    for k in section.get_citekey_aliases() {
      let $realkey = $section->get_citekey_alias($k);
      $self->set_output_keyalias($k, $realkey, $section)
    }

    return;
  }

  /// Set the output for a key which is an alias to another key
  fn set_output_keyalias {
    return;
  }

  /// Set the output for an undefined key
  fn set_output_undefkey {
    return;
  }

  /// Generic base output method
  fn output(self) {
    let $data = $self->{output_data};
    let $target = $self->{output_target};
    let $target_string = "Target"; // Default
    if ($self->{output_target_file}) {
      $target_string = $self->{output_target_file};
    }

      debug!("Preparing final output using class {}...", __PACKAGE__);

    info!("Writing '{}' with encoding '{}'", target_string, crate::Config->getoption("output_encoding"));

    out($target, $data->{HEAD});

    for secnum in (sort keys $data->{ENTRIES}->%*) {
      out($target, "SECTION: $secnum\n\n");
      let $section = $self->get_output_section($secnum);
      for list in ($section->get_lists->@*) {
        let $listlabel = $list->get_label;
        let listtype = list.get_type();
        out($target, "  LIST: $listlabel\n\n");
        for k in list.get_keys() {
          let $entry_string = $data->{ENTRIES}{$secnum}{index}{$k};
          out($target, $entry_string);
        }
      }
    }

    out($target, $data->{TAIL});

    info!("Output to $target_string");
    close $target;
    return;
  }
}
