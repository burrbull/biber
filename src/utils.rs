//! Various utility subs used in Biber

use std::collections::{HashMap, HashSet};

use crate::Id;
use lazy_regex::{regex, regex_is_match, regex_replace, regex_replace_all, regex_captures};
use unicode_normalization::UnicodeNormalization;
use unicode_segmentation::UnicodeSegmentation;

/* TODO
use parent qw(Exporter);

use constant {
  EXIT_OK => 0,
  EXIT_ERROR => 2
};

use Carp;
use Encode;
use File::Find;
use File::Spec;
use IPC::Cmd qw( can_run );
use IPC::Run3; // This works with PAR::Packer and Windows. IPC::Run doesn't
use crate::Constants;
use crate::LaTeX::Recode;
use crate::Entry::Name;
use List::AllUtils qw( first );
use Log::Log4perl qw(:no_extra_logdie_message);
use Scalar::Util qw(looks_like_number);
use Text::Balanced qw(extract_bracketed);
use Text::CSV;
use Text::Roman qw(isroman roman2int);
use Unicode::Normalize;
 */
pub fn NFC(s: &str) -> String {
  s.nfc().collect()
}

pub fn NFD(s: &str) -> String {
  s.nfd().collect()
}

pub fn NFKD(s: &str) -> String {
  s.nfkd().collect()
}
/* TODO
/// Expands a data file glob to a list of filenames
pub fn glob_data_file($source, $globflag) -> Vec<String> {
  let sources = Vec::new();

  // No globbing unless requested. No globbing for remote datasources.
  if regex_is_match!(r"\A(?:http|ftp)(s?):\/\/"xms, source) || !_bool_norm($globflag) {
    sources.push(source);
    return @sources;
  }

  info!("Globbing data source '{source}'");

  if ($^O =~ /Win/) {
    debug!("Enabling Windows-style globbing");
    require Win32;
    require File::DosGlob;
    File::DosGlob->import("glob");
  }

  push @sources, map {biber_decode_utf8($_)} glob NFC(qq("$source"));

  info!("Globbed data source '{source}' to '{}'", sources.join(","));
  sources
}

/// Use different read encoding/slurp interfaces for Windows due to its
/// horrible legacy codepage system
pub fn slurp_switchr($filename, $encoding) {
  let $slurp;
  $encoding = encoding.unwrap_or("UTF-8");
  if ($^O =~ /Win/ && !crate::Config->getoption("winunicode")) {
    require Win32::Unicode::File;
    let $fh = Win32::Unicode::File->new('<', NFC($filename));
    $fh->binmode(":encoding($encoding)");
    // 100MB block size as the loop over the default 1MB block size seems to fail for
    // files > 1Mb
    $slurp = $fh->slurp({blk_size => 1024*1024*100});
    $fh->close;
  }
  else {
    $slurp = File::Slurper::read_text($filename, $encoding);
  }
  return \$slurp;
}

/// Use different write encoding/slurp interfaces for Windows due to its
/// horrible legacy codepage system
pub fn slurp_switchw($filename, $string) {
  if ($^O =~ /Win/ && !crate::Config->getoption("winunicode")) {
    require Win32::Unicode::File;
    let $fh = Win32::Unicode::File->new('>', NFC($filename));
    $fh->binmode(":encoding(UTF-8)");
    $fh->write($string);
    $fh->flush;
    $fh->close;
  }
  else {
    File::Slurper::write_text($filename, NFC($string));
  }
  return;
}

/// Searches for a data file by
///
/// The exact path if the filename is absolute
/// In the input_directory, if defined
/// In the output_directory, if defined
/// Relative to the current directory
/// In the same directory as the control file
/// Using kpsewhich, if available
pub fn locate_data_file($source) {
  let $sourcepath = $source; // default if nothing else below applies
  let $foundfile;

  if regex_is_match!(r"\A(?:http|ftp)(s?):\/\/"xms, source) {
    info!("Data source '{}' is a remote BibTeX data source - fetching ...", source);
    if (let $cf = $REMOTE_MAP{$source}) {
      info!("Found '{}' in remote source cache", source);
      $sourcepath = $cf;
    }
    else {
      if ($1) { // HTTPS/FTPS
        // use IO::Socket::SSL qw(debug99); // useful for debugging SSL issues
        // We have to explicitly set the cert path because otherwise the https module
        // can't find the .pem when PAR::Packer'ed
        // Have to explicitly try to require Mozilla::CA here to get it into %INC below
        // It may, however, have been removed by some biber unpacked dists
        if (!exists($ENV{PERL_LWP_SSL_CA_FILE}) &&
            !exists($ENV{PERL_LWP_SSL_CA_PATH}) &&
            !defined(crate::Config->getoption("ssl-nointernalca")) &&
            eval {require Mozilla::CA}) {
          // we assume that the default CA file is in .../Mozilla/CA/cacert.pem
          let (vol, dir, _) = File::Spec->splitpath( $INC{"Mozilla/CA.pm"} );
          $dir =~ s/\/$//;      // splitpath sometimes leaves a trailing '/'
          $ENV{PERL_LWP_SSL_CA_FILE} = File::Spec->catpath($vol, "$dir/CA", "cacert.pem");
        }

        // fallbacks for, e.g., linux
        if !(exists($ENV{PERL_LWP_SSL_CA_FILE})) {
          for ca_bundle in (qw{
                                     /etc/ssl/certs/ca-certificates.crt
                                     /etc/pki/tls/certs/ca-bundle.crt
                                     /etc/ssl/ca-bundle.pem
                                 }) {
            if ! -e $ca_bundle {
              continue;
            }
            $ENV{PERL_LWP_SSL_CA_FILE} = $ca_bundle;
            break;
          }
          for ca_path in (qw{
                                   /etc/ssl/certs/
                                   /etc/pki/tls/
                               }) {
            if ! -d $ca_path {
              continue;
            }
            $ENV{PERL_LWP_SSL_CA_PATH} = $ca_path;
            break;
          }
        }

        if (defined(crate::Config->getoption("ssl-noverify-host"))) {
          $ENV{PERL_LWP_SSL_VERIFY_HOSTNAME} = 0;
        }

        require LWP::Protocol::https;
      }

      require LWP::UserAgent;
      // no need to unlink file as tempdir will be unlinked. Also, the tempfile
      // will be needed after this sub has finished and so it must not be unlinked
      // by going out of scope
      let $tf = File::Temp->new(TEMPLATE => "biber_remote_data_source_XXXXX",
                               DIR => crate::MASTER.biber_tempdir(),
                               SUFFIX => ".bib",
                               UNLINK => 0);

      // Pretend to be a browser otherwise some sites refuse the default LWP UA string
      let $ua = LWP::UserAgent->new;  // we create a global UserAgent object
      $ua->agent("Mozilla/5.0");
      $ua->env_proxy;
      let $request = HTTP::Request->new("GET", $source, ["Zotero-Allowed-Request" => "1"]);
      let $response = $ua->request($request, $tf->filename);

      if !($response->is_success) {
        biber_error("Could not fetch '$source' (HTTP code: " . $response->code. ")");
      }
      $sourcepath = $tf->filename;
      // cache any remote so it persists and so we don't fetch it again
      $REMOTE_MAP{$source} = $sourcepath;
    }
  }

  // If input_directory is set, perhaps the file can be found there so
  // construct a path to test later
  if (let $indir = crate::Config->getoption("input_directory")) {
    $foundfile = File::Spec->catfile($indir, $sourcepath);
  }
  // If output_directory is set, perhaps the file can be found there so
  // construct a path to test later
  else if (let $outdir = crate::Config->getoption("output_directory")) {
    $foundfile = File::Spec->catfile($outdir, $sourcepath);
  }

  // Filename is absolute
  if (File::Spec->file_name_is_absolute($sourcepath) && let $f = file_exist_check($sourcepath)) {
    return $f;
  }

  // File is input_directory or output_directory
  if (defined($foundfile) && let $f = file_exist_check($foundfile)) {
    return $f;
  }

  if (let $f = file_exist_check($sourcepath)) {
    return $f;
  }

  // File is where control file lives
  if (let $cfp = crate::config::get_ctrlfile_path()) {
    let (ctlvolume, ctldir, _) = File::Spec->splitpath($cfp);
    if ($ctlvolume) { // add vol sep for windows if volume is set and there isn't one
      if !($ctlvolume =~ /:\z/) {
        ctlvolume.push(':') ;
      }
    }
    if ($ctldir) { // add path sep if there isn't one
      if !($ctldir =~ /\/\z/) {
        ctldir.push('/') ;
      }
    }

    let $path = "$ctlvolume$ctldir$sourcepath";

    if (let $f = file_exist_check($path)) {
      return $f;
    }
  }

  // File is in kpse path
  if (can_run("kpsewhich")) {
      debug!("Looking for file '{}' via kpsewhich", sourcepath);
    let $found;
    let $err;
    run3  [ "kpsewhich", $sourcepath ], \undef, \$found, \$err, { return_if_system_error => 1};
    if ($?) {
        debug!("kpsewhich returned error: $err ($!)");
    }
      trace!("kpsewhich returned '{}'", found);
    if ($found) {
        debug!("Found '{}' via kpsewhich", sourcepath);
      chomp $found;
      found = regex!(r"\cM\z"xms).replace(&found, ""); // kpsewhich in cygwin sometimes returns ^M at the end
      // filename can be UTF-8 and run3() isn't clever with UTF-8
      let $f = file_exist_check(decode_utf8($found));
      return $f;
    }
    else {
        debug!("Could not find '{}' via kpsewhich", sourcepath);
    }
  }

  // Not found
  biber_error("Cannot find '$source'!")
}

/// Check existence of NFC/NFD file variants and return correct one.
/// Account for windows file encodings
fn file_exist_check($filename) {
  if ($^O =~ /Win/ && !crate::Config->getoption("winunicode")) {
    require Win32::Unicode::File;
    if (Win32::Unicode::File::statW(NFC($filename))) {
      return NFC($filename);
    }
    if (Win32::Unicode::File::statW(NFD($filename))) {
      return NFD($filename);
    }
  }
  else {
    if (-e NFC("$filename")) {
      return NFC("$filename");
    }
    if (-e NFD("$filename")) {
      return NFD("$filename");
    }
  }

  return None;
}

/// Wrapper around empty check to deal with Win32 Unicode filenames
pub fn check_empty($filename) {
  if ($^O =~ /Win/ && !crate::Config->getoption("winunicode")) {
    require Win32::Unicode::File;
    return (Win32::Unicode::File::file_size(NFC($filename))) ? 1 : 0;
  }
  else {
    return (-s $filename) ? 1 : 0;
  }
}

/// Wrapper around exists check to deal with Win32 Unicode filenames
pub fn check_exists($filename) {
  if ($^O =~ /Win/ && !crate::Config->getoption("winunicode")) {
    require Win32::Unicode::File;
    return Win32::Unicode::File::statW(NFC($filename)) ? 1 : 0;
  }
  else {
    return (-e $filename) ? 1 : 0;
  }
}

/// Wrapper around various warnings bits and pieces.
/// Add warning to the list of .bbl warnings and the master list of warnings
pub fn biber_warn($warning, $entry) {
  if $entry {
    $entry->add_warning($warning);
  }
  push $crate::MASTER->{warnings}->@*, $warning;
  return;
}

/// Wrapper around error logging
/// Forces an exit.
pub fn biber_error($error, nodie: bool) {
  error!($error);
  $crate::MASTER->{errors}++;
  // exit unless user requested not to for errors
  if !(nodie || crate::Config->getoption("nodieonerror")) {
    $crate::MASTER.display_end();
    exit EXIT_ERROR;
  }
}
*/
/* UNUSED
/// Given a crate::Names object, return an underscore normalised
/// concatenation of all of the full name strings.
pub fn makenamesid($names) {
  let @namestrings;
  for name in names.names() {
    push @namestrings, $name->get_namestring;
  }
  let $tmp = namestrings.join(" ");
  return normalise_string_underscore($tmp);
}

/// Given a crate::Name object, return an underscore normalised
/// concatenation of the full name strings.
pub fn makenameid($name) {
  return normalise_string_underscore($name->get_namestring);
}
*/
/*
/// Tries to convert UTF-8 to TeX macros in passed string
pub fn latex_recode_output($string) {
  return crate::LaTeX::Recode::latex_encode($string);
};

/// Removes elements which are not to be considered during initials generation
/// in names
pub fn strip_noinit(string) {
  if !($string) {
    return ""; // Sanitise missing data
  }
  if !(let $noinit = crate::Config->getoption("noinit")) {
    return $string;
  }
  for opt in ($noinit->@*) {
    let $re = $opt->{value};
    string = regex_xms(&format("{re}")).unwrap().replace_all(&string, "");
  }
  // remove latex macros (assuming they have only ASCII letters)
  $string =~ s{\\[A-Za-z]+\s*(\{([^\}]*)?\})?}{defined($2)?$2:q{}}eg;
  $string =~ s/^\{\}$//; // Special case if only braces are left
  return $string;
}

/// Removes elements which are not to be used in sorting a name from a string
pub fn strip_nosort(string: &str, fieldname: &str) -> String {
  no autovivification;
  if string.is_empty() {
    return ""; // Sanitise missing data
  }
  if let Some(nosort) = crate::Config->getoption("nosort").skip_empty() {
    let mut restrings = Vec::new();

    for nsopt in nosort {
      // Specific fieldnames override sets
      if (unicase::eq(nsopt.name, fieldname)) {
        restrings.push(nsopt.value);
      } else if (let $set = $DATAFIELD_SETS{nsopt.name.to_lowercase()} ) {
        if set.iter().any(|v| unicase::eq(v, fieldname)) {
          restrings.push(nsopt.value);
        }
      }
    }

    // If no nosort to do, just return string
    if restrings.is_empty() {
      return string;
    }

    for re in &restrings {
      string = regex_xms(&format("{re}")).unwrap().replace_all(&string, "");
    }
  }
  string
}

/// Removes elements which are not to be used in certain name-related operations like:
///
/// * fullhash generation
/// * uniquename generation
///
/// from a name
pub fn strip_nonamestring(string, fieldname) {
  no autovivification;
  if !($string) {
    return ""; // Sanitise missing data
  }
  if !(let $nonamestring = crate::Config->getoption("nonamestring")) {
    return $string;
  }

  let $restrings;

  for nnopt in ($nonamestring->@*) {
    // Specific fieldnames override sets
    if (unicase::eq($nnopt->{name}, fieldname)) {
      push $restrings->@*, $nnopt->{value};
    } else if (let $set = $DATAFIELD_SETS{nnopt.name.to_lowercase()} ) {
      if (first {unicase::eq($_, fieldname)} $set->@*) {
        push $restrings->@*, $nnopt->{value};
      }
    }
  }

  // If no nonamestring to do, just return string
  if !($restrings) {
    return $string;
  }

  for re in ($restrings->@*) {
    let r = regex_xms(&format!(r"{re}")).unwrap();
    string = r.replace_all(string, "");
  }
  return $string;
}

/// Remove some things from a string for label generation. Don't strip \p{Dash}
/// as this is needed to process compound names or label generation.
pub fn normalise_string_label(string: &str) -> String {
  if string.is_empty() {
    return ""; // Sanitise missing data
  }
  let $nolabels = crate::Config->getoption("nolabel");
  string = regex!(r"\\[A-Za-z]+").replace_all("");   // remove latex macros (assuming they have only ASCII letters)
  // Replace ties with spaces or they will be lost
  string = regex_replace_all!(r"([^\\])~", string, |_, word| format!("{word} ")); // Foo~Bar -> Foo Bar
  for nolabel in nolabels {
    let re = nolabel.value;
    let r = regex_xms(&format!(r"{re}")).unwrap();
    string = r.replace_all(string, "");              // remove nolabel items
  }
  string = regex!(r"(?:^\s+|\s+$)").replace_all(""); // Remove leading and trailing spaces
  regex!(r"\s+").replace_all(" ")                    // collapse spaces
}

/// Removes LaTeX macros, and all punctuation, symbols, separators
/// as well as leading and trailing whitespace for sorting strings.
/// Control chars don't need to be stripped as they are completely ignorable in DUCET
pub fn normalise_string_sort(string: &str, fieldname: &str) -> String {
  if string.is_empty() {
    return ""; // Sanitise missing data
  }
  // First strip nosort REs
  let s = strip_nosort(string, fieldname);
  // Then replace ties with spaces or they will be lost
  let s = regex_replace_all!(r"([^\\])~", s, |_, word| format!("{word} ")); // Foo~Bar -> Foo Bar
  // Don't use normalise_string_common() as this strips out things needed for sorting
  let s = regex!(r"\\[A-Za-z]+").replace_all(s, ""); // remove latex macros (assuming they have only ASCII letters)
  let s = regex!(r"[{}]+").replace_all(s, "");       // remove embedded braces
  let s = regex!(r"^\s+|\s+$").replace_all(s, "");   // Remove leading and trailing spaces
  regex!(r"\s+").replace_all(s, " ")                 // collapse spaces

}
*/
/// Some string normalisation for bblxml output
pub fn normalise_string_bblxml(s: &str) -> String {
  if s.is_empty() {
    return String::new(); // Sanitise missing data
  }
  // remove latex macros (assuming they have only ASCII letters)
  let s = regex!(r"\\[A-Za-z]+").replace_all(s, "");
  // remove pointless braces
  let s = regex_replace_all!(r"\{([^\{\}]+)\}", &s, |_, word| format!("{word}"));
  // replace ties with spaces
  s.replace('~', " ")
}

/// Removes LaTeX macros, and all punctuation, symbols, separators and control characters,
/// as well as leading and trailing whitespace for sorting strings.
/// Only decodes LaTeX character macros into Unicode if output is UTF-8
pub fn normalise_string(s: &str) -> String {
  if s.is_empty() {
    return String::new(); // Sanitise missing data
  }
  // First replace ties with spaces or they will be lost
  // Foo~Bar -> Foo Bar
  let s = regex_replace_all!(r"([^\\])~", s, |_, word| format!("{word} "));
  normalise_string_common(&s).into()
}

/// Common bit for normalisation
fn normalise_string_common(s: &str) -> String {
  // remove latex macros (assuming they have only ASCII letters)
  let s = regex!(r"\\[A-Za-z]+").replace_all(s, "");
  // remove punctuation, symbols and control
  let s = regex!(r"[\p{P}\p{S}\p{C}]+").replace_all(&s, "");
  // Remove leading and trailing spaces
  let s = regex!(r"^\s+|\s+$").replace_all(&s, "");
  // collapse spaces
  regex!(r"\s+").replace_all(&s, " ").into()
}

/// Normalise strings used for hashes. We collapse LaTeX macros into a vestige
/// so that hashes are unique between things like:
///
/// Smith
/// {\v S}mith
///
/// we replace macros like this to preserve their vestiges:
///
/// \v S -> v:
/// \" -> 34:
pub fn normalise_string_hash(s: &str) -> String {
  if s.is_empty() {
    return String::new(); // Sanitise missing data
  }
  // remove tex macros
  let s = regex_replace_all!(r"\\(\p{L}+)\s*", s, |_, name| format!("{name}:"));
  // remove accent macros like \"a
  let s = regex_replace_all!(r"\\([^\p{L}])\s*", &s, |_, name| format!("{}:", ord(name)));
  // Remove braces, ties, dots, spaces
  regex!(r"[\{\}~\.\s]+").replace_all(&s, "").into()
}

fn ord(s: &str) -> u32 {
  todo!()
  // https://perldoc.perl.org/functions/ord
}

/// Like normalise_string, but also substitutes ~ and whitespace with underscore.
pub fn normalise_string_underscore(s: &str) -> String {
  if s.is_empty() {
    return String::new(); // Sanitise missing data
  }
  // Foo~Bar -> Foo Bar
  let s = regex_replace_all!(r"([^\\])~", s, |_, word| format!("{word} "));
  let s = normalise_string(&s);
  regex!(r"\s+").replace_all(&s, "_").into()
}

/// Escapes a few special character which might be used in labels
pub fn escape_label(s: &str) -> String {
  if s.is_empty() {
    return String::new(); // Sanitise missing data
  }
  let s = regex_replace_all!(r"([_\^\$\#%\&])", &s, |_, symbol| format!("\\{symbol}"));
  s.replace('~', "{\\textasciitilde}")
   .replace('>', "{\\textgreater}")
   .replace('<', "{\\textless}")
}
/// Unscapes a few special character which might be used in label but which need
/// sorting without escapes
pub fn unescape_label(s: &str) -> String {
  if s.is_empty() {
    return String::new(); // Sanitise missing data
  }
  let s = regex_replace_all!(r"\\([_\^\$\#%\&])", &s, |_, symbol| format!("{symbol}"));
  s.replace("{\\textasciitilde}", "~")
   .replace("{\\textgreater}", ">")
   .replace("{\\textless}", "<")
}

/// reduce_array(\@a, \@b) returns all elements in @a that are not in @b
pub fn reduce_array<T: Clone + Eq + std::hash::Hash>(a: impl Iterator<Item=T>, b: impl Iterator<Item=T>) -> Vec<T> {
  let countb = HashSet::<T>::from_iter(b);
  let mut result = Vec::<T>::new();
  for elem in a {
    if !countb.contains(&elem) {
      result.push(elem.clone())
    }
  }
  result
}
/*
/// Remove surrounding curly brackets:
///     "{string}" -> "string"
/// but not
///     "{string} {string}" -> "string} {string"
///
/// Return (boolean if stripped, string)
pub fn remove_outer(s: &str) -> (bool, String) {
  /*if regex_is_match!(r"\}\s*\{", s) {
    (false, s.into())
  }*/
  let @check = extract_bracketed(s, "{}");
  if (!defined($check[0]) or $check[0] != s) {// Not balanced outer braces, ignore
    return (0, s);
  }
  if s.len() > 2 && s.starts_with('{') && s.ends_with('}') {
    (true, s[1..s.len()-1].to_string())
  } else {
    (false, s.into())
  }
}
*/
/// Return (boolean if surrounded in braces
pub fn has_outer(s: &str) -> bool {
  if regex_is_match!(r"\}\s*\{", s) {
    return false;
  }
  s.len() > 2 && s.starts_with('{') &&  s.ends_with('}')
}

/// Add surrounding curly brackets:
/// "string" -> "{string}"
pub fn add_outer(s: &str) -> String {
    format!("{{{}}}", s)
}
/// upper case of initial letters in a string
pub fn ucinit(s: &str) -> String {
  let s = s.to_lowercase();
  regex_replace_all!(r"\b(\p{Ll})", &s, |_, symbol| format!(r"\u{symbol}")).into()
}

/* TODO
/// Checks for undefness of arbitrary things, including
/// composite method chain calls which don't reliably work
/// with defined() (see perldoc for defined())
/// This works because we are just testing the value passed
/// to this sub. So, for example, this is randomly unreliable
/// even if the resulting value of the arg to defined() is "undef":
///
/// defined($thing->method($arg)->method)
///
/// whereas:
///
/// is_undef($thing->method($arg)->method)
///
/// works since we only test the return value of all the methods
/// with defined()
pub fnb is_undef($val) -> bool {
  !defined($val)
}

/// Checks for definedness in the same way as is_undef()
pub fn is_def($val) -> bool {
  defined($val)
}

/// Checks for undef or nullness (see is_undef() above)
pub fn is_undef_or_null($val) -> bool {
  if is_undef($val) {
    return true;
  }
  !val
}

/// Checks for def and unnullness (see is_undef() above)
pub fn is_def_and_notnull($arg) -> bool {
  defined($arg) && is_notnull($arg)
}

/// Checks for def and nullness (see is_undef() above)
pub fn is_def_and_null($arg) -> bool {
  defined($arg) && is_null($arg)
}

/// Checks for nullness
pub fn is_null($arg) {
  !is_notnull($arg)
}

/// Checks for notnullness
pub fn is_notnull($arg) -> bool {
  if !defined($arg) {
    return None;
  }
  let $st = is_notnull_scalar($arg);
  if (defined($st) && $st) { return true; }
  let $at = is_notnull_array($arg);
  if (defined($at) && $at) { return true; }
  let $ht = is_notnull_hash($arg);
  if (defined($ht) && $ht) { return true; }
  let $ot = is_notnull_object($arg);
  if (defined($ot) && $ot) { return true; }
  false
}

/// Checks for notnullness of a scalar
fn is_notnull_scalar($arg) -> bool {
  if !(ref \$arg == "SCALAR") {
    return None;
  }
  arg != ""
}

/// Checks for notnullness of an array (passed by ref)
fn is_notnull_array($arg) -> bool {
  if !(ref $arg == "ARRAY") {
    return None;
  }
  let @arr = $arg->@*;
  return $#arr > -1 ? 1 : 0;
}

/// Checks for notnullness of an hash (passed by ref)
fn is_notnull_hash($arg) {
  if !(ref $arg == "HASH") {
    return None;
  }
  let @arr = keys $arg->%*;
  return $#arr > -1 ? 1 : 0;
}

/// Checks for notnullness of an object (passed by ref)
fn is_notnull_object($arg) {
  if !(ref($arg) =~ m/\Acrate::/xms) {
    return None;
  }
  return $arg->notnull ? 1 : 0;
}

/// Turns a hash into a string of keys and values
pub fn stringify_hash($hashref) {
  let $string;
  while (let ($k,$v) = each $hashref->%*) {
    string.push_str(format!("{k} => {v}, "));
  }
  // Take off the trailing comma and space
  chop $string;
  chop $string;
  return $string;
}

/// Normalise any UTF-8 encoding string immediately to exactly what we want
/// We want the strict perl utf8 "UTF-8"
pub fn normalise_utf8 {
  if crate::Config->getoption("input_encoding")
  .filter(|enc| regex_is_match!(r"\Autf-?8\z"xmsi, enc)).is_some() {
    crate::Config->setoption("input_encoding", "UTF-8");
  }
  if crate::Config->getoption("output_encoding")
   .filter(|enc| regex_is_match!(r"\Autf-?8\z"xmsi, enc)).is_some() {
    crate::Config->setoption("output_encoding", "UTF-8");
  }
}
*/
/*
/// We turn the initials into an array so we can be flexible with them later
/// The tie here is used only so we know what to split on. We don't want to make
/// any typesetting decisions in Biber, like what to use to join initials so on
/// output to the .bbl, we only use BibLaTeX macros.
pub fn inits(istring: &str) -> impl Iterator<Item=&str> {
  // Remove any spurious braces left by btparse inits routines
  let istring = regex!(r"[{}]").replace(istring, "");
  // The map {} is there to remove broken hyphenated initials returned from btparse
  // For example, in the, admittedly strange 'al- Hassan, John', we want the 'al-'
  // interpreted as a prefix (because of the following space) but because of the
  // hypen, this is intialised as "a-" by btparse. So we correct such edge cases here by
  // removing any trailing dashes in initials
  fancy_regex::Regex::new(r"(?<!\\)~").unwrap().split(istring).map(|s| regex!(r"\p{Pd}$").replace(s, ""))
}*/

/// Replace all join typsetting elements in a name part (space, ties) with BibLaTeX macros
/// so that typesetting decisions are made in BibLaTeX, not hard-coded in Biber
pub fn join_name(nstring: &str) -> String {
  let nstring = fancy_regex::Regex::new(r"(?xms)(?<!\\\S)\s+").unwrap().replace_all(nstring, "\\bibnamedelimb "); // Don't do spaces in char macros
  let nstring = fancy_regex::Regex::new(r"(?xms)(?<!\\)~A").unwrap().replace_all(&nstring, "\\bibnamedelima "); // Don't do '\~'
  // Special delim after name parts ending in period
  fancy_regex::Regex::new(r"(?xms)(?<=\.)\\bibnamedelim[ab]").unwrap().replace_all(&nstring, "\\bibnamedelimi").into()
}
/*
/// Process any per_entry option transformations which are necessary on output
pub fn filter_entry_options(secnum: u32, be: &Entry) {
  let bee = be.get_field("entrytype");
  let citekey = be.get_field("citekey");
  let mut roptions = Vec::new();

  for opt in (sort crate::Config->getblxentryoptions(secnum, citekey)) {

    let $val = crate::Config->getblxoption(secnum, opt, None, citekey);
    let $cfopt = $CONFIG_BIBLATEX_OPTIONS{ENTRY}{$opt}{OUTPUT};
    $val = map_boolean($opt, $val, "tostring");

    // By this point, all entry meta-options have been expanded by expand_option_input
    if ($cfopt) { // suppress only explicitly ignored output options
      
      roptions.push(if val { format!("{opt}={val}") } else { opt });
    }
  }
  roptions
}

/// Do an interpolating (neg)match using a match RE and a string passed in as variables
/// Using /g on matches so that $1,$2 etc. can be populated from repeated matches of
/// same capture group as well as different groups
pub fn imatch($value, $val_match, $negmatch, $ci) {
  if !($val_match) {
    return 0;
  }
  if ($ci) {
    $val_match = qr/$val_match/i;
  }
  else {
    $val_match = qr/$val_match/;
  }
  if ($negmatch) {// "!~" doesn't work here as we need an array returned
    return $value =~ m/$val_match/xmsg ? () : (1);
  }
  else {
    return $value =~ m/$val_match/xmsg;
  }
}

/// Do an interpolating match/replace using a match RE, replacement RE
/// and string passed in as variables
pub fn ireplace($value, $val_match, $val_replace, $ci) {
  if !($val_match) {
    return $value;
  }
  if ($ci) {
    $val_match = qr/$val_match/i;
  }
  else {
    $val_match = qr/$val_match/;
  }
  // Tricky quoting because of later evals
  $val_replace = '"' . $val_replace . '"';
  $value =~ s/$val_match/$val_replace/eegxms;
  return $value;
}

/// Validate a biber/biblatex XML metadata file against an RNG XML schema
pub fn validate_biber_xml($file, $type, $prefix, $schema) {
  require XML::LibXML;

  // Set up XML parser
  let $xmlparser = XML::LibXML->new();
  $xmlparser->line_numbers(1); // line numbers for more informative errors

  // Set up schema
  let $xmlschema;

  // Deal with the strange world of Par::Packer paths
  // We might be running inside a PAR executable and @INC is a bit odd in this case
  // Specifically, "Biber.pm" in @INC might resolve to an internal jumbled name
  // nowhere near to these files. You know what I mean if you've dealt with pp
  if !($schema) {
    // we assume that unspecified schema files are in the same dir as Biber.pm:
    (let $vol, let $biber_path, _) = File::Spec->splitpath( $INC{"Biber.pm"} );
    biber_path = regex_replace!(r"\/$", &biber_path, ""); // splitpath sometimes leaves a trailing '/'

    if ($biber_path =~ m|/par\-| && $biber_path !~ m|/inc|) { // a mangled PAR @INC path
      $schema = File::Spec->catpath($vol, "$biber_path/inc/lib/Biber", "${type}.rng");
    }
    else {
      $schema = File::Spec->catpath($vol, "$biber_path/Biber", "${type}.rng");
    }
  }

  if (check_exists($schema)) {
    $xmlschema = XML::LibXML::RelaxNG->new( location => $schema )
  }
  else {
    biber_warn("Cannot find XML::LibXML::RelaxNG schema '$schema'. Skipping validation : $!");
    return;
  }

  // Parse file
  let $doc = $xmlparser->load_xml(location => $file);

  // XPath context
  if ($prefix) {
    let $xpc = XML::LibXML::XPathContext->new($doc);
    $xpc->registerNs($type, $prefix);
  }

  // Validate against schema. Dies if it fails.
  eval { $xmlschema->validate($doc) };
  if (ref($@)) {
      debug!( $@->dump() );
    biber_error("'$file' failed to validate against schema '$schema'");
  }
  else if ($@) {
    biber_error("'$file' failed to validate against schema '$schema'\n$@");
  }
  else {
    info!("'{}' validates against schema '{}'", file, schema);
  }
  undef $xmlparser;
}

/// Convert booleans between strings and numbers. Because standard XML "boolean"
/// datatype considers "true" and "1" the same etc.
pub fn map_boolean($bn, $bv, $dir) {
  // Ignore non-booleans
  if !exists($CONFIG_OPTTYPE_BIBLATEX{$bn}) {
    return $bv;
  }
  if $CONFIG_OPTTYPE_BIBLATEX{$bn} != "boolean" {
    return $bv;
  }

  let $b = bv.to_lowercase();

  let %map = (true  => 1,
             false => 0,
            );
  if ($dir == "tonum") {
    if looks_like_number($b) {
      return $b;
    }
    return $map{$b};
  }
  else if ($dir == "tostring") {
    if !looks_like_number($b) {
      return $b;
    }
    %map = reverse %map;
    return $map{$b};
  }
}

/// Set per-entry options
pub fn process_entry_options($citekey, $options, $secnum) {
  if !($options) {
    return;       // Just in case it's null
  }
  for opt in options {
    let opt = regex!(r"\s+=\s+").replace_all(opt, "="); get rid of spaces around any "="
    let (_, one, two) = regex_captures!(r"^([^=]+)=?(.+)?$", opt).unwrap();
    let val = two.unwrap_or(1); // bare options are just boolean numerals
    let $oo = expand_option_input(one, $val, $CONFIG_BIBLATEX_OPTIONS{ENTRY}{one.to_lowercase()}{INPUT});

    for o in ($oo->@*) {
      crate::Config->setblxoption($secnum, $o->[0], $o->[1], "ENTRY", $citekey);
    }
  }
  return;
}
*/
/// Merge entry options, dealing with conflicts
pub fn merge_entry_options(opts: Option<&[&str]>, overrideopts: Option<&[&str]>) -> Vec<String> {
  if overrideopts.is_none() {
    return opts.unwrap().into_iter().map(|&s| s.into()).collect();
  }
  let overrideopts = overrideopts.unwrap();
  if opts.is_none() {
    return overrideopts.into_iter().map(|&s| s.into()).collect();
  }
  let opts = opts.unwrap();
  let mut merged = Vec::new();
  let mut used_overrides = Vec::new();

  for &ov in opts.into_iter() {
    let mut or = false;
    let (_, o, e, v) = regex_captures!(r"^([^=]+)(=?)(.*)$", ov).unwrap();
    for &oov in overrideopts.into_iter() {
      let (_, oo, eo, vo) = regex_captures!(r"^([^=]+)(=?)(.*)$", oov).unwrap();
      if o == oo {
        or = true;
        let oropt = format!("{oo}{}{}", eo, vo);
        merged.push(oropt.clone());
        used_overrides.push(oropt);
        break;
      }
    }
    if !or {
      merged.push(format!("{o}{}{}", e, v));
    }
  }

  // Now push anything in the overrides array which had no conflicts
  for oov in overrideopts {
    let oov = oov.to_string();
    if !used_overrides.contains(&oov) {
      merged.push(oov);
    }
  }

  merged
}
/*
/// Expand options such as meta-options coming from biblatex
pub fn expand_option_input($opt, $val, $cfopt) {
  let $outopts;

  // Coerce $val to integer so we know what to test with later
  $val = map_boolean($opt, $val, "tonum");

  // Standard option
  if (!defined($cfopt)) { // no special input meta-option handling
    push $outopts->@*, [$opt, $val];
  }
  // Set all split options
  else if (ref($cfopt) == "ARRAY") {
    for k in ($cfopt->@*) {
      push $outopts->@*, [$k, $val];
    }
  }
  // ASSUMPTION - only biblatex booleans resolve to hashes (currently, only dataonly)
  // Specify values per all splits
  else if (ref($cfopt) == "HASH") {
    for k in (keys $cfopt->%*) {
      let $subval = map_boolean($k, $cfopt->{$k}, "tonum");

      // meta-opt $val is 0/false - invert any boolean sub-options and ignore others
      // for example, if datonly=false in a per-entry option:
      // skipbib => false
      // skiplab => false
      // skipbiblist => false
      // uniquename => DON'T SET ANYTHING (picked up from higher scopes)
      // uniquelist => DON'T SET ANYTHING (picked up from higher scopes)
      if !($val) {
        if (exists($CONFIG_OPTTYPE_BIBLATEX{$k}) &&
            $CONFIG_OPTTYPE_BIBLATEX{$k} == "boolean") {

          // The defaults for the sub-options are for when $val=true
          // invert booleans when $val=false
          push $outopts->@*, [$k, $subval ? 0 : 1];
        }
      }
      else {
        push $outopts->@*, [$k, $subval];
      }
    }
  }

  return $outopts;
}

/// Parse of ISO8601 date range
pub fn parse_date_range(bibentry: &mut Entry, datetype: &str, datestring: &str) {
  let ($sd, $sep, $ed) = $datestring =~ m|^([^/]+)?(/)?([^/]+)?$|;

  // Very bad date format, something like '2006/05/04' catch early
  if !($sd || $ed) {
    return (None, None, None, None);
  }

  let $unspec;
  if ($sd =~ /X/) {// ISO8601-2 4.3 unspecified format
    ($sd, $sep, $ed, $unspec) = parse_date_unspecified($sd);
  }
  // Set start date unknown flag
  if ($sep && !$sd) {
    bibentry.set_field(&format!("{datetype}dateunknown"), true);
  }
  // Set end date unknown flag
  if ($sep && !$ed) {
    bibentry.set_field(format!("{datetype}enddateunknown"), true);
  }
  return (parse_date_start($sd), parse_date_end($ed), $sep, $unspec);
}
*/
/// Parse of ISO8601-2:2016 4.3 unspecified format into date range
/// Returns range plus specification of granularity of unspecified
fn parse_date_unspecified(d: &str) -> Option<(String, char, String, &str)> {

  // 199X -> 1990/1999
  if let Some((_, decade)) = regex_captures!(r"^(\d{3})X$", d) {
    return Some((format!("{decade}0"), '/', format!("{decade}9"), "yearindecade"));
  }
  // 19XX -> 1900/1999
  else if let Some((_, century)) = regex_captures!(r"^(\d{2})XX$", d) {
    return Some((format!("{century}00"), '/', format!("{century}99"), "yearincentury"));
  }
  // 1999-XX -> 1999-01/1999-12
  else if let Some((_, year)) = regex_captures!(r"^(\d{4})\p{Dash}XX$", d) {
    return Some((format!("{year}-01"), '/', format!("{year}-12"), "monthinyear"));
  }
  // 1999-01-XX -> 1999-01-01/1999-01-31
  // (understands different months and leap years)
  else if let Some((_, year, month)) = regex_captures!(r"^(\d{4})\p{Dash}(\d{2})\p{Dash}XX", d) {

    fn leapyear(year: i32) -> bool {
      ((year % 4 == 0) && (year % 100 != 0)) || (year % 400 == 0)
    }

    let mut monthdays = HashMap::new();
    for m in 1..12 {
      monthdays.insert(format!("{m:02}"), "31");
    }
    for m in ["09", "04", "06", "11"] {
      monthdays.insert(m.into(), "30");
    }
    monthdays.insert("02".into(), if leapyear(year.parse().unwrap()) {"29"} else {"28"});

    return Some((format!("{year}-{month}-01"), '/', format!("{year}-{month}-{}", monthdays.get(month).unwrap()), "dayinmonth"));
  }
  // 1999-XX-XX -> 1999-01-01/1999-12-31
  else if let Some((_, year)) = regex_captures!(r"^(\d{4})\p{Dash}XX\p{Dash}XX", d) {
    return Some((format!("{year}-01-01"), '/', format!("{year}-12-31"), "dayinyear"));
  }
  None
}
/*
/// Convenience wrapper
pub fn parse_date_start(date) {
  return parse_date($CONFIG_DATE_PARSERS{start}, date);
}

/// Convenience wrapper
pub fn parse_date_end(date) {
  return parse_date($CONFIG_DATE_PARSERS{end}, date);
}

/// Parse of iso8601-2 dates
fn parse_date($obj, $string) {
  // Must do this to make sure meta-information from sub-class crate::Date::Format is reset
  $obj->init();
  if !($string) {
    return 0;
  }
  if $string == ".." { // ISO8601-2 4.4 (open date)
    return 0;
  }

  let $dt = eval {$obj->parse_datetime($string)};
  if !($dt) {
    return $dt; // bad parse, don't do anything else
  }

  // Check if this datetime is before the Gregorian start date. If so, return Julian date
  // instead of Gregorian/astronomical
  // This conversion is only done if the date is not missing month or day since the Julian
  // Gregorian difference is usually a matter of only days and therefore a bare year like
  // "1565" could be "1564" or "1565" in Julian, depending on the month/day of the Gregorian date
  // For example, "1565-01-01" (which is what DateTime will default to for bare years), is
  // "1564-12-22" Julian but 1564-01-11" and later is "1565" Julian year.
  if (crate::Config->getblxoption(None,"julian") &&
      !$obj->missing("month") &&
      !$obj->missing("day")) {

    // There is guaranteed to be an end point since biblatex has a default
    let $gs = crate::Config->getblxoption(None,"gregorianstart");
    let ($gsyear, $gsmonth, $gsday) = $gs =~ m/^(\d{4})\p{Dash}(\d{2})\p{Dash}(\d{2})$/;
    let $dtgs = DateTime->new( year  => $gsyear,
                              month => $gsmonth,
                              day   => $gsday );

    // Datetime is not before the Gregorian start point
    if (DateTime->compare($dt, $dtgs) == -1) {
      // Override with Julian conversion
      $dt = DateTime::Calendar::Julian->from_object( object => $dt );
      $obj->set_julian;
    }
  }

  return $dt;
}

/// Force month/day to ISO8601-2:2016 format with leading zero
pub fn date_monthday($md) {
  if md { format!("{md:02}") } else { None };
}

/// Perform NFD form conversion as well as UTF-8 conversion. Used to normalize
/// bibtex input as the T::B interface doesn't allow a neat whole file slurping.
pub fn biber_decode_utf8(s) {
  return NFD(decode_utf8(s));// Unicode NFD boundary
}

/// Output to target. Outputs NFC UTF-8 if output is UTF-8
pub fn out($fh, $string) {
  print $fh NFC($string);// Unicode NFC boundary
}

/// Fix up some problems with comments after being processed by btparse
pub fn process_comment($comment) {
  // Fix up structured Jabref comments by re-instating line breaks. Hack.
  if ($comment =~ m/jabref-meta:/) {
    $comment =~ s/([:;])\s(\d)/$1\n$2/xmsg;
    $comment =~ s/\z/\n/xms;
  }
  return $comment;
}
*/
/// Map babel/polyglossia language options to a sensible CLDR (bcp47) locale default
/// Return input string if there is no mapping
pub fn locale2bcp47<'a>(localestr: &'a str) -> &'a str {
  if localestr.is_empty() {
    return "";
  }
  return crate::constants::LOCALE_MAP.get(localestr).copied().unwrap_or(localestr);
}

/// Map CLDR (bcp47) locale to a babel/polyglossia locale
/// Return input string if there is no mapping
pub fn bcp472locale<'a>(localestr: &'a str) -> &'a str {
  if localestr.is_empty() {
    return "";
  }
  return crate::constants::LOCALE_MAP_R.get(localestr).copied().unwrap_or(localestr);
}
/* TODO
/// Calculate the length of a range field
/// Range fields are an array ref of two-element array refs [range_start, range_end]
/// range_end can be be empty for open-ended range or undef
/// Deals with Unicode and ASCII roman numerals via the magic of Unicode NFKD form
///
/// m-n -> [m, n]
/// m   -> [m, None]
/// m-  -> [m, ""]
/// -n  -> ["", n]
/// -   -> ["", None]
pub fn rangelen($rf) {
  let $rl = 0;
  for f in ($rf->@*) {
    let $m = $f->[0];
    let $n = $f->[1];
    // m is something that's just numerals (decimal Unicode roman or ASCII roman)
    if ($m && $m =~ /^[\p{Nd}\p{Nl}iIvVxXlLcCdDmM]+$/) {
      // This magically decomposes Unicode roman chars into ASCII compat
      $m = NFKD($m);
      // n is something that's just numerals (decimal Unicode roman or ASCII roman)
      if ($n && $n =~ /^[\p{Nd}\p{Nl}iIvVxXlLcCdDmM]+$/) {
        // This magically decomposes Unicode roman chars into ASCII compat
        $n = NFKD($n);
        $m = isroman($m) ? roman2int($m) : $m;
        $n = isroman($n) ? roman2int($n) : $n;
        // If still not an int at this point, it's probably some non-int page number that
        // isn't a roman numeral so give up
        if !(looks_like_number($n) && looks_like_number($m)) {
          return -1;
        }
        // Deal with not so explicit ranges like 22-4 or 135-38
        // Done by turning numbers into string arrays, reversing and then filling in blanks
        if ($n < $m) {
          let @m = reverse split(//,$m);
          let @n = reverse split(//,$n);
          for (i, mi) in m.iter().enumerate() {
            if $n[$i] {
              continue;
            }
            $n[$i] = mi;
          }
          $n = n.iter().reverse().join("");
        }
        $rl += (($n - $m) + 1);
      }
      // n is ""
      else if (defined($n)) {
        // open-ended range can't be calculated, just return -1
        return -1;
      }
      // n is undef, single item
      else {
        $rl += 1;
      }
    }
    else {
      // open-ended range can't be calculated, just return -1
      return -1;
    }
  }
  return $rl;
}

/// Return array ref of array refs of matches and start indices of matches
/// for provided array of compiled regexps into string
pub fn match_indices($regexes, $string) {
  let @ret;
  let $relen = 0;
  for regex in ($regexes->@*) {
    let $len = 0;
    while ($string =~ /$regex/g) {
      let $gcs = Unicode::GCString->new($string)->substr($-[0], $+[0]-$-[0]);
      push @ret, [ $gcs->as_string, $-[0] - $relen ];
      $len = $gcs->length;
    }
    $relen += $len;
  }
  // Return last index first so replacements can be done without recalculating
  // indices changed by earlier index replacements
  if scalar(@ret) { [reverse @ret] } else { None }
}

/// Parses a range of values into a two-value array ref.
/// Ranges with no starting value default to "1"
/// Ranges can be open-ended and it's up to surrounding code to interpret this
/// Ranges can be single figures which is shorthand for 1-x
pub fn parse_range($rs) {
  $rs =~ m/\A\s*(\P{Pd}+)?\s*(\p{Pd})*\s*(\P{Pd}+)?\s*\z/xms;
  if ($2) {
    return [$1.unwrap_or(1), $3];
  }
  else {
    return [1, $1];
  }
}

/// Removes annotation marker from a field name
pub fn strip_annotation($string) {
  let $ann = $CONFIG_META_MARKERS{annotation};
  let $nam = $CONFIG_META_MARKERS{namedannotation};
  return $string =~ s/$ann$nam?.*$//r;
}

/// Parses a range of values into a two-value array ref.
/// Either start or end can be undef and it's up to surrounding code to interpret this
pub fn parse_range_alt($rs) {
  $rs =~ m/\A\s*(\P{Pd}+)?\s*(\p{Pd})*\s*(\P{Pd}+)?\s*\z/xms;
  if ($2) {
    return [$1, $3];
  }
  else {
    return None;
  }
}

/// Replace loop markers with values.
pub fn maploopreplace($string, $maploop) {
  // $MAPUNIQVAL is lexical here
  no strict "vars";
  if !defined($string) {
    return None;
  }
  if !($maploop) {
    return $string;
  }
  $string =~ s/\$MAPLOOP/$maploop/g;
  $string =~ s/\$MAPUNIQVAL/$MAPUNIQVAL/g;
  if ($string =~ m/\$MAPUNIQ/) {
    let $MAPUNIQ = Id::new();
    $string =~ s/\$MAPUNIQ/$MAPUNIQ/g;
    $MAPUNIQVAL = $MAPUNIQ;
  }
  return $string;
}

/// Get a ref to a transliterator for the given from/to
/// We are abstracting this in this way because it is not clear what the future
/// of the transliteration library is. We want to be able to switch.
pub fn get_transliterator(target: &str, from: &str, to: &str) {
  let target = target.to_lowercase();
  let from = from.to_lowercase();
  let to = to.to_lowercase();
  let valid_from = ["iast", "russian"];
  let valid_to   = ["devanagari", "ala-lc", "bgn/pcgn-standard")]
  if !(valid_from.contains(&from) &&
          valid_to.contains(&to)) {
    biber_warn("Invalid transliteration from/to pair ($from/$to)");
  }
  require Lingua::Translit;
    debug!("Using '{} -> {}' transliteration for sorting '{}'", from, to, target);

  // List pairs explicitly as we don't expect there to be to many of these ever
  match (&from, &to) {
    ("iast", "devanagari") =>{
       new Lingua::Translit("IAST Devanagari");
    }
    ("russian", "ala-lc") => {
      new Lingua::Translit("ALA-LC RUS");
    }
    ("russian", "bgn/pcgn-standard") => {
      new Lingua::Translit("BGN/PCGN RUS Standard");
    }
    _ => None,
  }
}

/// Run a transliterator on passed text. Hides call semantics of transliterator
/// so we can switch engine in the future.
pub fn call_transliterator($target, $from, $to, $text) {
  if (let $tr = get_transliterator($target, $from, $to)) {
    // using Lingua::Translit, NFC boundary as we are talking to external module
    return $tr->translit(NFC($text));
  }
  else {
    return $text;
  }
}
*/
/// Passed an array of strings, returns an array of initials
pub fn gen_initials<'a>(strings: impl Iterator<Item=&'a str>) -> Vec<String> {
  let mut strings_out = Vec::new();
  for mut string in strings {
    // Deal with hyphenated name parts and normalise to a '-' character for easy
    // replacement with macro later
    // Dont' split a name part if it's brace-wrapped
    // Dont' split a name part if the hyphen in a hyphenated name is protected like:
    // Hans{-}Peter as this is an old BibTeX way of suppressing hyphenated names
    if !regex_is_match!(r"^\{.+\}$", string) && regex_is_match!(r"[^{]\p{Dash}[^}]", string) {
      strings_out.push(
        gen_initials(regex!(r"\p{Dash}").split(string))
        .join("-"));
    }
    else {
      // remove any leading braces and backslash from latex decoding or protection
      let string = regex!(r"^\{+").replace(string, "");
      let chr = string.graphemes(true).next().unwrap();
      // Keep diacritics with their following characters
      if regex_is_match!(r"^\p{Dia}", chr) {
        strings_out.push(string.graphemes(true).take(2).collect());
      }
      else {
        strings_out.push(chr.into());
      }
    }
  }
  strings_out
}

/// Joins name parts using BibTeX tie algorithm. Ties are added:
///
/// 1. After the first part if it is less than three characters long
/// 2. Before the family part
pub fn join_name_parts<'a>(parts: impl Iterator<Item=&'a str>) -> String {
  let parts: Vec<_> = parts.collect();
  if parts.len() == 1 { // special case - 1 part
    parts[0].into()
  } else if parts.len() == 2 { // special case - 2 parts
    format!("{}~{}", parts[0], parts[1])
  } else {
    format!(
      "{}{}{}~{}",
      parts[0],
      if parts[0].graphemes(true).count() < 3 { '~' } else { ' ' },
      parts[1..parts.len()-1].join(" "),
      parts[parts.len()-1]
    )
  }
}
/*
/// Split an xsv using Text::CSV because it is fast and can handle quoting
pub fn split_xsv($string, $sep) {
  if ($sep) {
    $CONFIG_CSV_PARSER->sep_char($sep);
  }
  $CONFIG_CSV_PARSER->parse($string);
  return $CONFIG_CSV_PARSER->fields();
}

/// Add a macro sep for minutes in timezones
pub fn tzformat($tz) {
  if ($tz =~ m/^([+-])(\d{2}):?(\d{2})?/) {
    return "$1$2" . ($3 ? "\\bibtzminsep $3" : "");
  }
  else if ($tz == "UTC") {
    return 'Z';
  }
  else {
    return $tz;
  }
}

/// Wrapper to enforce map_appendstrict
pub fn appendstrict_check($step, $orig, $val) {
  // Strict append?
  if ($step->{map_appendstrict}) {
    if ($orig) {
      return $orig . $val;
    }
    else { // orig is empty, don't append
      return "";
    }
  }
  // Normal append, don't care if orig is empty
  else {
    return $orig . $val;
  }
}

/// Process backendin attribute from .bcf
pub fn process_backendin($bin) -> Option<Unknown> {
  if !($bin) {
    return None;
  } */
//  let $opts = [split(/\s*,\s*/, $bin)];
//  if (grep {/=/} $opts->@*) {
//    let $hopts;
//    for o in opts {
//      let ($k, $v) = $o =~ m/\s*([^=]+)=(.+)\s*/;
/* TODO 
      $hopts->{$k} = $v;
    }
    return $hopts;
  }
  else {
    return $opts;
  }
  Nones
}

/// Replace xnamesep/xdatasep with output variants
/// Some datasource formats don't need the marker (biblatexml)
pub fn xdatarefout(xdataref: &str, implicitmarker: bool) {
  let $xdmi = crate::Config->getoption("xdatamarker");
  let $xdmo = crate::Config->getoption("output_xdatamarker");
  let $xnsi = crate::Config->getoption("xnamesep");
  let $xnso = crate::Config->getoption("output_xnamesep");
  let $xdsi = crate::Config->getoption("xdatasep");
  let $xdso = crate::Config->getoption("output_xdatasep");
  if implicitmarker { // Don't want output marker at all
    $xdataref =~ s/^$xdmi$xnsi//x;
  }
  else {
    $xdataref =~ s/^$xdmi(?=$xnsi)/$xdmo/x; // Should be only one
    $xdataref =~ s/$xnsi/$xnso/xg;
  }
  $xdataref =~ s/$xdsi/$xdso/xg;
  return $xdataref;
}

/// Check an output value for an xdata ref and replace output markers if necessary.
pub fn xdatarefcheck(val: &str, implicitmarker: bool) -> Option<String> {
  if !(val) {
    return None;
  }
  let $xdmi = crate::Config->getoption("xdatamarker");
  let $xnsi = crate::Config->getoption("xnamesep");
  if ($val =~ m/^\s*$xdmi(?=$xnsi)/) {
    return Some(xdatarefout($val, implicitmarker));
  }
  return None;
}

fn _bool_norm($b) -> bool {
  if !$b {
    return false;
  }
  if $b =~ m/(?:true|1)/i {
    return true;
  }
  false;
}
*/

pub fn regex_xms(re: &str) -> Result<regex::Regex, regex::Error> {
  regex::RegexBuilder::new(re)
  .multi_line(true)
  .dot_matches_new_line(true)
  .ignore_whitespace(true)
  .build()
}