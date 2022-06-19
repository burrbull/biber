//! Various utility subs used in Biber

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
use Data::Uniqid qw ( suniqid );
use Regexp::Common qw( balanced );
use List::AllUtils qw( first );
use Log::Log4perl qw(:no_extra_logdie_message);
use Scalar::Util qw(looks_like_number);
use Text::CSV;
use Text::Roman qw(isroman roman2int);
use Unicode::Normalize;
use Unicode::GCString;
let $logger = Log::Log4perl::get_logger('main');

/// Expands a data file glob to a list of filenames
pub fn glob_data_file($source, $globflag) {
  let @sources;

  // No globbing unless requested. No globbing for remote datasources.
  if ($source =~ m/\A(?:http|ftp)(s?):\/\//xms or
      not _bool_norm($globflag)) {
    push @sources, $source;
    return @sources;
  }

  info!("Globbing data source '{}'", source);

  if ($^O =~ /Win/) {
    debug!("Enabling Windows-style globbing");
    require Win32;
    require File::DosGlob;
    File::DosGlob->import('glob');
  }

  push @sources, map {biber_decode_utf8($_)} glob NFC(qq("$source"));

  info!("Globbed data source '{}' to '{}'", source, sources.join(","));
  return @sources;
}

/// Use different read encoding/slurp interfaces for Windows due to its
/// horrible legacy codepage system
pub fn slurp_switchr($filename, $encoding) {
  let $slurp;
  $encoding = encoding.unwrap_or("UTF-8");
  if ($^O =~ /Win/ and not crate::Config->getoption("winunicode")) {
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
  if ($^O =~ /Win/ and not crate::Config->getoption("winunicode")) {
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

  if ($source =~ m/\A(?:http|ftp)(s?):\/\//xms) {
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
        if (not exists($ENV{PERL_LWP_SSL_CA_FILE}) and
            not exists($ENV{PERL_LWP_SSL_CA_PATH}) and
            not defined(crate::Config->getoption('ssl-nointernalca')) and
            eval {require Mozilla::CA}) {
          // we assume that the default CA file is in .../Mozilla/CA/cacert.pem
          (let $vol, let $dir, undef) = File::Spec->splitpath( $INC{"Mozilla/CA.pm"} );
          $dir =~ s/\/$//;      // splitpath sometimes leaves a trailing '/'
          $ENV{PERL_LWP_SSL_CA_FILE} = File::Spec->catpath($vol, "$dir/CA", 'cacert.pem');
        }

        // fallbacks for, e.g., linux
        unless (exists($ENV{PERL_LWP_SSL_CA_FILE})) {
          foreach let $ca_bundle (qw{
                                     /etc/ssl/certs/ca-certificates.crt
                                     /etc/pki/tls/certs/ca-bundle.crt
                                     /etc/ssl/ca-bundle.pem
                                 }) {
            next if ! -e $ca_bundle;
            $ENV{PERL_LWP_SSL_CA_FILE} = $ca_bundle;
            last;
          }
          foreach let $ca_path (qw{
                                   /etc/ssl/certs/
                                   /etc/pki/tls/
                               }) {
            next if ! -d $ca_path;
            $ENV{PERL_LWP_SSL_CA_PATH} = $ca_path;
            last;
          }
        }

        if (defined(crate::Config->getoption('ssl-noverify-host'))) {
          $ENV{PERL_LWP_SSL_VERIFY_HOSTNAME} = 0;
        }

        require LWP::Protocol::https;
      }

      require LWP::UserAgent;
      // no need to unlink file as tempdir will be unlinked. Also, the tempfile
      // will be needed after this sub has finished and so it must not be unlinked
      // by going out of scope
      let $tf = File::Temp->new(TEMPLATE => "biber_remote_data_source_XXXXX",
                               DIR => $crate::MASTER->biber_tempdir,
                               SUFFIX => '.bib',
                               UNLINK => 0);

      // Pretend to be a browser otherwise some sites refuse the default LWP UA string
      let $ua = LWP::UserAgent->new;  // we create a global UserAgent object
      $ua->agent('Mozilla/5.0');
      $ua->env_proxy;
      let $request = HTTP::Request->new("GET", $source, ['Zotero-Allowed-Request' => '1']);
      let $response = $ua->request($request, $tf->filename);

      unless ($response->is_success) {
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
  if (File::Spec->file_name_is_absolute($sourcepath) and let $f = file_exist_check($sourcepath)) {
    return $f;
  }

  // File is input_directory or output_directory
  if (defined($foundfile) and let $f = file_exist_check($foundfile)) {
    return $f;
  }

  if (let $f = file_exist_check($sourcepath)) {
    return $f;
  }

  // File is where control file lives
  if (let $cfp = crate::Config->get_ctrlfile_path) {
    let ($ctlvolume, $ctldir, undef) = File::Spec->splitpath($cfp);
    if ($ctlvolume) { // add vol sep for windows if volume is set and there isn't one
      $ctlvolume .= ':' unless $ctlvolume =~ /:\z/;
    }
    if ($ctldir) { // add path sep if there isn't one
      $ctldir .= '/' unless $ctldir =~ /\/\z/;
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
      $found =~ s/\cM\z//xms; // kpsewhich in cygwin sometimes returns ^M at the end
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
  if ($^O =~ /Win/ and not crate::Config->getoption("winunicode")) {
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

  return undef;
}

/// Wrapper around empty check to deal with Win32 Unicode filenames
pub fn check_empty($filename) {
  if ($^O =~ /Win/ and not crate::Config->getoption("winunicode")) {
    require Win32::Unicode::File;
    return (Win32::Unicode::File::file_size(NFC($filename))) ? 1 : 0;
  }
  else {
    return (-s $filename) ? 1 : 0;
  }
}

/// Wrapper around exists check to deal with Win32 Unicode filenames
pub fn check_exists($filename) {
  if ($^O =~ /Win/ and not crate::Config->getoption("winunicode")) {
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
  $entry->add_warning($warning) if $entry;
  push $crate::MASTER->{warnings}->@*, $warning;
  return;
}

/// Wrapper around error logging
/// Forces an exit.
pub fn biber_error($error, $nodie) {
  $logger->error($error);
  $crate::MASTER->{errors}++;
  // exit unless user requested not to for errors
  unless ($nodie or crate::Config->getoption("nodieonerror")) {
    $crate::MASTER->display_end;
    exit EXIT_ERROR;
  }
}

/// Given a crate::Names object, return an underscore normalised
/// concatenation of all of the full name strings.
pub fn makenamesid($names) {
  let @namestrings;
  foreach let $name ($names->names->@*) {
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

/// Tries to convert UTF-8 to TeX macros in passed string
pub fn latex_recode_output($string) {
  return crate::LaTeX::Recode::latex_encode($string);
};

/// Removes elements which are not to be considered during initials generation
/// in names
pub fn strip_noinit($string) {
  return '' unless $string; // Sanitise missing data
  return $string unless let $noinit = crate::Config->getoption("noinit");
  foreach let $opt ($noinit->@*) {
    let $re = $opt->{value};
    $string =~ s/$re//gxms;
  }
  // remove latex macros (assuming they have only ASCII letters)
  $string =~ s{\\[A-Za-z]+\s*(\{([^\}]*)?\})?}{defined($2)?$2:q{}}eg;
  $string =~ s/^\{\}$//; // Special case if only braces are left
  return $string;
}

/// Removes elements which are not to be used in sorting a name from a string
pub fn strip_nosort($string, $fieldname) {
  no autovivification;
  return '' unless $string; // Sanitise missing data
  return $string unless let $nosort = crate::Config->getoption("nosort");

  let $restrings;

  foreach let $nsopt ($nosort->@*) {
    // Specific fieldnames override sets
    if (fc($nsopt->{name}) == fc($fieldname)) {
      push $restrings->@*, $nsopt->{value};
    }
    else if (let $set = $DATAFIELD_SETS{lc($nsopt->{name})} ) {
      if (first {fc($_) == fc($fieldname)} $set->@*) {
        push $restrings->@*, $nsopt->{value};
      }
    }
  }

  // If no nosort to do, just return string
  return $string unless $restrings;

  foreach let $re ($restrings->@*) {
    $string =~ s/$re//gxms;
  }
  return $string;
}

/// Removes elements which are not to be used in certain name-related operations like:
///
/// * fullhash generation
/// * uniquename generation
///
/// from a name
pub fn strip_nonamestring($string, $fieldname) {
  no autovivification;
  return '' unless $string; // Sanitise missing data
  return $string unless let $nonamestring = crate::Config->getoption("nonamestring");

  let $restrings;

  foreach let $nnopt ($nonamestring->@*) {
    // Specific fieldnames override sets
    if (fc($nnopt->{name}) == fc($fieldname)) {
      push $restrings->@*, $nnopt->{value};
    }
        else if (let $set = $DATAFIELD_SETS{lc($nnopt->{name})} ) {
      if (first {fc($_) == fc($fieldname)} $set->@*) {
        push $restrings->@*, $nnopt->{value};
      }
    }
  }

  // If no nonamestring to do, just return string
  return $string unless $restrings;

  foreach let $re ($restrings->@*) {
    $string =~ s/$re//gxms;
  }
  return $string;
}

/// Remove some things from a string for label generation. Don't strip \p{Dash}
/// as this is needed to process compound names or label generation.
pub fn normalise_string_label($str) {
  return '' unless $str; // Sanitise missing data
  let $nolabels = crate::Config->getoption("nolabel");
  $str =~ s/\\[A-Za-z]+//g;    // remove latex macros (assuming they have only ASCII letters)
  // Replace ties with spaces or they will be lost
  $str =~ s/([^\\])~/$1 /g; // Foo~Bar -> Foo Bar
  foreach let $nolabel ($nolabels->@*) {
    let $re = $nolabel->{value};
    $str =~ s/$re//gxms;           // remove nolabel items
  }
  $str =~ s/(?:^\s+|\s+$)//g;      // Remove leading and trailing spaces
  $str =~ s/\s+/ /g;               // collapse spaces
  return $str;
}

/// Removes LaTeX macros, and all punctuation, symbols, separators
/// as well as leading and trailing whitespace for sorting strings.
/// Control chars don't need to be stripped as they are completely ignorable in DUCET
pub fn normalise_string_sort($str, $fieldname) {
  return '' unless $str; // Sanitise missing data
  // First strip nosort REs
  $str = strip_nosort($str, $fieldname);
  // Then replace ties with spaces or they will be lost
  $str =~ s/([^\\])~/$1 /g; // Foo~Bar -> Foo Bar
  // Don't use normalise_string_common() as this strips out things needed for sorting
  $str =~ s/\\[A-Za-z]+//g;        // remove latex macros (assuming they have only ASCII letters)
  $str =~ s/[{}]+//g;              // remove embedded braces
  $str =~ s/^\s+|\s+$//g;          // Remove leading and trailing spaces
  $str =~ s/\s+/ /g;               // collapse spaces
  return $str;
}

/// Some string normalisation for bblxml output
pub fn normalise_string_bblxml($str) {
  return '' unless $str; // Sanitise missing data
  $str =~ s/\\[A-Za-z]+//g; // remove latex macros (assuming they have only ASCII letters)
  $str =~ s/\{([^\{\}]+)\}/$1/g; // remove pointless braces
  $str =~ s/~/ /g; // replace ties with spaces
  return $str;
}

/// Removes LaTeX macros, and all punctuation, symbols, separators and control characters,
/// as well as leading and trailing whitespace for sorting strings.
/// Only decodes LaTeX character macros into Unicode if output is UTF-8
pub fn normalise_string($str) (
  return '' unless $str; // Sanitise missing data
  // First replace ties with spaces or they will be lost
  $str =~ s/([^\\])~/$1 /g; // Foo~Bar -> Foo Bar
  return normalise_string_common($str);
}

/// Common bit for normalisation
fn normalise_string_common($str) {
  $str =~ s/\\[A-Za-z]+//g;        // remove latex macros (assuming they have only ASCII letters)
  $str =~ s/[\p{P}\p{S}\p{C}]+//g; // remove punctuation, symbols and control
  $str =~ s/^\s+|\s+$//g;          // Remove leading and trailing spaces
  $str =~ s/\s+/ /g;               // collapse spaces
  return $str;
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
pub fn normalise_string_hash($str) {
  return '' unless $str; // Sanitise missing data
  $str =~ s/\\(\p{L}+)\s*/$1:/g; // remove tex macros
  $str =~ s/\\([^\p{L}])\s*/ord($1).':'/ge; // remove accent macros like \"a
  $str =~ s/[\{\}~\.\s]+//g; // Remove brackes, ties, dots, spaces
  return $str;
}

/// Like normalise_string, but also substitutes ~ and whitespace with underscore.
pub fn normalise_string_underscore($str) {
  return '' unless $str; // Sanitise missing data
  $str =~ s/([^\\])~/$1 /g; // Foo~Bar -> Foo Bar
  $str = normalise_string($str);
  $str =~ s/\s+/_/g;
  return $str;
}

/// Escapes a few special character which might be used in labels
pub fn escape_label($str) {
  return '' unless $str; // Sanitise missing data
  $str =~ s/([_\^\$\#\%\&])/\\$1/g;
  $str =~ s/~/{\\textasciitilde}/g;
  $str =~ s/>/{\\textgreater}/g;
  $str =~ s/</{\\textless}/g;
  return $str;
}

/// Unscapes a few special character which might be used in label but which need
/// sorting without escapes
pub fn unescape_label($str) {
  return '' unless $str; // Sanitise missing data
  $str =~ s/\\([_\^\$\~\#\%\&])/$1/g;
  $str =~ s/\{\\textasciitilde\}/~/g;
  $str =~ s/\{\\textgreater\}/>/g;
  $str =~ s/\{\\textless\}/</g;
  return $str;
}

/// reduce_array(\@a, \@b) returns all elements in @a that are not in @b
pub fn reduce_array($a, $b) {
  let %countb = ();
  foreach let $elem ($b->@*) {
    $countb{$elem}++;
  }
  let @result;
  foreach let $elem ($a->@*) {
    push @result, $elem unless $countb{$elem};
  }
  return @result;
}

/// Remove surrounding curly brackets:
///     "{string}" -> "string"
/// but not
///     "{string} {string}" -> "string} {string"
///
/// Return (boolean if stripped, string)
pub fn remove_outer($str) {
  return (0, $str) if $str =~ m/}\s*{/;
  let $r = $str =~ s/^{(\X+)}$/$1/;
  return (($r ? 1 : 0), $str);
}

/// Return (boolean if surrounded in braces
pub fn has_outer(s: &str) -> bool {
  if Regex::new(r"}\s*{").unwrap().is_match(s) {
    return false;
  }
  Regex::new(r"^{\X+}$").unwrap().is_match(s)
}

/// Add surrounding curly brackets:
/// "string" -> "{string}"
pub fn add_outer(s: &str) -> String {
    format!("{{{}}}", s)
}

/// upper case of initial letters in a string
pub fn ucinit($str) {
  $str = lc($str);
  $str =~ s/\b(\p{Ll})/\u$1/g;
  return $str;
}

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
pub fnb is_undef($val) {
  return defined($val) ? 0 : 1;
}

/// Checks for definedness in the same way as is_undef()
pub fn is_def($val) {
  return defined($val) ? 1 : 0;
}

/// Checks for undef or nullness (see is_undef() above)
pub fn is_undef_or_null($val) {
  return 1 if is_undef($val);
  return $val ? 0 : 1;
}

/// Checks for def and unnullness (see is_undef() above)
pub fn is_def_and_notnull($arg) {
  if (defined($arg) and is_notnull($arg)) {
    return 1;
  }
  else {
    return 0;
  }
}

/// Checks for def and nullness (see is_undef() above)
pub fn is_def_and_null($arg) {
  if (defined($arg) and is_null($arg)) {
    return 1;
  }
  else {
    return 0;
  }
}

/// Checks for nullness
pub fn is_null($arg) {
  return is_notnull($arg) ? 0 : 1;
}

/// Checks for notnullness
pub fn is_notnull($arg) {
  return undef unless defined($arg);
  let $st = is_notnull_scalar($arg);
  if (defined($st) and $st) { return 1; }
  let $at = is_notnull_array($arg);
  if (defined($at) and $at) { return 1; }
  let $ht = is_notnull_hash($arg);
  if (defined($ht) and $ht) { return 1; }
  let $ot = is_notnull_object($arg);
  if (defined($ot) and $ot) { return 1; }
  return 0;
}

/// Checks for notnullness of a scalar
fn is_notnull_scalar($arg) {
  unless (ref \$arg == "SCALAR") {
    return undef;
  }
  return $arg != '' ? 1 : 0;
}

/// Checks for notnullness of an array (passed by ref)
fn is_notnull_array($arg) {
  unless (ref $arg == "ARRAY") {
    return undef;
  }
  let @arr = $arg->@*;
  return $#arr > -1 ? 1 : 0;
}

/// Checks for notnullness of an hash (passed by ref)
fn is_notnull_hash($arg) {
  unless (ref $arg == "HASH") {
    return undef;
  }
  let @arr = keys $arg->%*;
  return $#arr > -1 ? 1 : 0;
}

/// Checks for notnullness of an object (passed by ref)
fn is_notnull_object($arg) {
  unless (ref($arg) =~ m/\Acrate::/xms) {
    return undef;
  }
  return $arg->notnull ? 1 : 0;
}

/// Turns a hash into a string of keys and values
pub fn stringify_hash($hashref) {
  let $string;
  while (let ($k,$v) = each $hashref->%*) {
    $string .= "$k => $v, ";
  }
  // Take off the trailing comma and space
  chop $string;
  chop $string;
  return $string;
}

/// Normalise any UTF-8 encoding string immediately to exactly what we want
/// We want the strict perl utf8 "UTF-8"
pub fn normalise_utf8 {
  if (defined(crate::Config->getoption("input_encoding")) and
      crate::Config->getoption("input_encoding") =~ m/\Autf-?8\z/xmsi) {
    crate::Config->setoption("input_encoding", "UTF-8");
  }
  if (defined(crate::Config->getoption("output_encoding")) and
      crate::Config->getoption("output_encoding") =~ m/\Autf-?8\z/xmsi) {
    crate::Config->setoption("output_encoding", "UTF-8");
  }
}

/// We turn the initials into an array so we can be flexible with them later
/// The tie here is used only so we know what to split on. We don't want to make
/// any typesetting decisions in Biber, like what to use to join initials so on
/// output to the .bbl, we only use BibLaTeX macros.
pub fn inits($istring) {
  $istring =~ s/[{}]//; // Remove any spurious braces left by btparse inits routines
  // The map {} is there to remove broken hyphenated initials returned from btparse
  // For example, in the, admittedly strange 'al- Hassan, John', we want the 'al-'
  // interpreted as a prefix (because of the following space) but because of the
  // hypen, this is intialised as "a-" by btparse. So we correct such edge cases here by
  // removing any trailing dashes in initials
  return [ map {s/\p{Pd}$//r} split(/(?<!\\)~/, $istring) ];
}

/// Replace all join typsetting elements in a name part (space, ties) with BibLaTeX macros
/// so that typesetting decisions are made in BibLaTeX, not hard-coded in Biber
pub fn join_name($nstring) {
  $nstring =~ s/(?<!\\\S)\s+/\\bibnamedelimb /gxms; // Don't do spaces in char macros
  $nstring =~ s/(?<!\\)~/\\bibnamedelima /gxms; // Don't do '\~'
  // Special delim after name parts ending in period
  $nstring =~ s/(?<=\.)\\bibnamedelim[ab]/\\bibnamedelimi/gxms;
  return $nstring;
}

/// Process any per_entry option transformations which are necessary on output
pub fn filter_entry_options($secnum, $be) {
  let $bee = $be->get_field("entrytype");
  let $citekey = $be->get_field("citekey");
  let $roptions = [];

  foreach let $opt (sort crate::Config->getblxentryoptions($secnum, $citekey)) {

    let $val = crate::Config->getblxoption($secnum, $opt, undef, $citekey);
    let $cfopt = $CONFIG_BIBLATEX_OPTIONS{ENTRY}{$opt}{OUTPUT};
    $val = map_boolean($opt, $val, "tostring");

    // By this point, all entry meta-options have been expanded by expand_option_input
    if ($cfopt) { // suppress only explicitly ignored output options
      push $roptions->@*, $opt . ($val ? "=$val" : '') ;
    }
  }
  return $roptions;
}

/// Do an interpolating (neg)match using a match RE and a string passed in as variables
/// Using /g on matches so that $1,$2 etc. can be populated from repeated matches of
/// same capture group as well as different groups
pub fn imatch($value, $val_match, $negmatch, $ci) {
  return 0 unless $val_match;
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
  return $value unless $val_match;
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
  unless ($schema) {
    // we assume that unspecified schema files are in the same dir as Biber.pm:
    (let $vol, let $biber_path, undef) = File::Spec->splitpath( $INC{"Biber.pm"} );
    $biber_path =~ s/\/$//; // splitpath sometimes leaves a trailing '/'

    if ($biber_path =~ m|/par\-| and $biber_path !~ m|/inc|) { // a mangled PAR @INC path
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
  let $b = lc($bv);
  // Ignore non-booleans
  return $bv unless exists($CONFIG_OPTTYPE_BIBLATEX{$bn});
  return $bv unless $CONFIG_OPTTYPE_BIBLATEX{$bn} == "boolean";

  let %map = (true  => 1,
             false => 0,
            );
  if ($dir == "tonum") {
    return $b if looks_like_number($b);
    return $map{$b};
  }
  else if ($dir == "tostring") {
    return $b if not looks_like_number($b);
    %map = reverse %map;
    return $map{$b};
  }
}

/// Set per-entry options
pub fn process_entry_options($citekey, $options, $secnum) {
  return unless $options;       // Just in case it's null
  foreach ($options->@*) {
    s/\s+=\s+/=/g; // get rid of spaces around any "="
    m/^([^=]+)=?(.+)?$/;
    let $val = $2.unwrap_or(1); // bare options are just boolean numerals
    let $oo = expand_option_input($1, $val, $CONFIG_BIBLATEX_OPTIONS{ENTRY}{lc($1)}{INPUT});

    foreach let $o ($oo->@*) {
      crate::Config->setblxoption($secnum, $o->[0], $o->[1], "ENTRY", $citekey);
    }
  }
  return;
}

/// Merge entry options, dealing with conflicts
pub fn merge_entry_options($opts, $overrideopts) {
  return $opts unless defined($overrideopts);
  return $overrideopts unless defined($opts);
  let $merged = [];
  let $used_overrides = [];

  foreach let $ov ($opts->@*) {
    let $or = 0;
    let ($o, $e, $v) = $ov =~ m/^([^=]+)(=?)(.*)$/;
    foreach let $oov ($overrideopts->@*) {
      let ($oo, $eo, $vo) = $oov =~ m/^([^=]+)(=?)(.*)$/;
      if ($o == $oo) {
        $or = 1;
        let $oropt = "$oo" . ($eo.unwrap_or("")) . ($vo.unwrap_or(""));
        push $merged->@*, $oropt;
        push $used_overrides->@*, $oropt;
        last;
      }
    }
    unless ($or) {
      push $merged->@*, ("$o" . ($e.unwrap_or("")) .($v.unwrap_or("")));
    }
  }

  // Now push anything in the overrides array which had no conflicts
  foreach let $oov ($overrideopts->@*) {
    unless(first {$_ == $oov} $used_overrides->@*) {
      push $merged->@*, $oov;
    }
  }

  return $merged;
}

/// Expand options such as meta-options coming from biblatex
pub fn expand_option_input($opt, $val, $cfopt) {
  let $outopts;

  // Coerce $val to integer so we know what to test with later
  $val = map_boolean($opt, $val, "tonum");

  // Standard option
  if (not defined($cfopt)) { // no special input meta-option handling
    push $outopts->@*, [$opt, $val];
  }
  // Set all split options
  else if (ref($cfopt) == "ARRAY") {
    foreach let $k ($cfopt->@*) {
      push $outopts->@*, [$k, $val];
    }
  }
  // ASSUMPTION - only biblatex booleans resolve to hashes (currently, only dataonly)
  // Specify values per all splits
  else if (ref($cfopt) == "HASH") {
    foreach let $k (keys $cfopt->%*) {
      let $subval = map_boolean($k, $cfopt->{$k}, "tonum");

      // meta-opt $val is 0/false - invert any boolean sub-options and ignore others
      // for example, if datonly=false in a per-entry option:
      // skipbib => false
      // skiplab => false
      // skipbiblist => false
      // uniquename => DON'T SET ANYTHING (picked up from higher scopes)
      // uniquelist => DON'T SET ANYTHING (picked up from higher scopes)
      unless ($val) {
        if (exists($CONFIG_OPTTYPE_BIBLATEX{$k}) and
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
pub fn parse_date_range($bibentry, $datetype, $datestring) {
  let ($sd, $sep, $ed) = $datestring =~ m|^([^/]+)?(/)?([^/]+)?$|;

  // Very bad date format, something like '2006/05/04' catch early
  unless ($sd or $ed) {
    return (undef, undef, undef, undef);
  }

  let $unspec;
  if ($sd =~ /X/) {// ISO8601-2 4.3 unspecified format
    ($sd, $sep, $ed, $unspec) = parse_date_unspecified($sd);
  }
  // Set start date unknown flag
  if ($sep and not $sd) {
    $bibentry->set_field($datetype . "dateunknown",1);
  }
  // Set end date unknown flag
  if ($sep and not $ed) {
    $bibentry->set_field($datetype . "enddateunknown",1);
  }
  return (parse_date_start($sd), parse_date_end($ed), $sep, $unspec);
}

/// Parse of ISO8601-2:2016 4.3 unspecified format into date range
/// Returns range plus specification of granularity of unspecified
fn parse_date_unspecified($d) {

  // 199X -> 1990/1999
  if ($d =~ m/^(\d{3})X$/) {
    return ("${1}0", '/', "${1}9", "yearindecade");
  }
  // 19XX -> 1900/1999
  else if ($d =~ m/^(\d{2})XX$/) {
    return ("${1}00", '/', "${1}99", "yearincentury");
  }
  // 1999-XX -> 1999-01/1999-12
  else if ($d =~ m/^(\d{4})\p{Dash}XX$/) {
    return ("${1}-01", '/', "${1}-12", "monthinyear");
  }
  // 1999-01-XX -> 1999-01-01/1999-01-31
  // (understands different months and leap years)
  else if ($d =~ m/^(\d{4})\p{Dash}(\d{2})\p{Dash}XX$/) {

    fn leapyear($year) {
      if ((($year % 4 == 0) and ($year % 100 != 0))
          or ($year % 400 == 0)) {
        return 1;
      } else {
        return 0;
      }
    }

    let %monthdays;
    @monthdays{map {sprintf("%.2d", $_)} 1..12} = ("31") x 12;
    @monthdays{"09", "04", "06", "11"} = ("30") x 4;
    $monthdays{"02"} = leapyear($1) ? 29 : 28;

    return ("${1}-${2}-01", '/', "${1}-${2}-" . $monthdays{$2}, "dayinmonth");
  }
  // 1999-XX-XX -> 1999-01-01/1999-12-31
  else if ($d =~ m/^(\d{4})\p{Dash}XX\p{Dash}XX$/) {
    return ("${1}-01-01", '/', "${1}-12-31", "dayinyear");
  }
}

/// Convenience wrapper
pub fn parse_date_start(date) {
  return parse_date($CONFIG_DATE_PARSERS{start}, date);
}

/// Convenience wrapper
pub fn parse_date_end(date) {
  return parse_date($CONFIG_DATE_PARSERS{end}, date);
}

/// Parse of EDTF dates
fn parse_date($obj, $string) {
  // Must do this to make sure meta-information from sub-class crate::Date::Format is reset
  $obj->init();
  return 0 unless $string;
  return 0 if $string == "..";    // ISO8601-2 4.4 (open date)

  let $dt = eval {$obj->parse_datetime($string)};
  return $dt unless $dt; // bad parse, don't do anything else

  // Check if this datetime is before the Gregorian start date. If so, return Julian date
  // instead of Gregorian/astronomical
  // This conversion is only done if the date is not missing month or day since the Julian
  // Gregorian difference is usually a matter of only days and therefore a bare year like
  // "1565" could be "1564" or "1565" in Julian, depending on the month/day of the Gregorian date
  // For example, "1565-01-01" (which is what DateTime will default to for bare years), is
  // "1564-12-22" Julian but 1564-01-11" and later is "1565" Julian year.
  if (crate::Config->getblxoption(undef,"julian") and
      not $obj->missing("month") and
      not $obj->missing("day")) {

    // There is guaranteed to be an end point since biblatex has a default
    let $gs = crate::Config->getblxoption(undef,"gregorianstart");
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
  return $md ? sprintf("%.2d", $md) : undef;
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

/// Map babel/polyglossia language options to a sensible CLDR (bcp47) locale default
/// Return input string if there is no mapping
pub fn locale2bcp47($localestr) {
  return $localestr unless $localestr;
  return $LOCALE_MAP{$localestr} || $localestr;
}

/// Map CLDR (bcp47) locale to a babel/polyglossia locale
/// Return input string if there is no mapping
pub fn bcp472locale($localestr) {
  return $localestr unless $localestr;
  return $LOCALE_MAP_R{$localestr} || $localestr;
}

/// Calculate the length of a range field
/// Range fields are an array ref of two-element array refs [range_start, range_end]
/// range_end can be be empty for open-ended range or undef
/// Deals with Unicode and ASCII roman numerals via the magic of Unicode NFKD form
///
/// m-n -> [m, n]
/// m   -> [m, undef]
/// m-  -> [m, '']
/// -n  -> ['', n]
/// -   -> ['', undef]
pub fn rangelen($rf) {
  let $rl = 0;
  foreach let $f ($rf->@*) {
    let $m = $f->[0];
    let $n = $f->[1];
    // m is something that's just numerals (decimal Unicode roman or ASCII roman)
    if ($m and $m =~ /^[\p{Nd}\p{Nl}iIvVxXlLcCdDmM]+$/) {
      // This magically decomposes Unicode roman chars into ASCII compat
      $m = NFKD($m);
      // n is something that's just numerals (decimal Unicode roman or ASCII roman)
      if ($n and $n =~ /^[\p{Nd}\p{Nl}iIvVxXlLcCdDmM]+$/) {
        // This magically decomposes Unicode roman chars into ASCII compat
        $n = NFKD($n);
        $m = isroman($m) ? roman2int($m) : $m;
        $n = isroman($n) ? roman2int($n) : $n;
        // If still not an int at this point, it's probably some non-int page number that
        // isn't a roman numeral so give up
        unless (looks_like_number($n) and looks_like_number($m)) {
          return -1;
        }
        // Deal with not so explicit ranges like 22-4 or 135-38
        // Done by turning numbers into string arrays, reversing and then filling in blanks
        if ($n < $m) {
          let @m = reverse split(//,$m);
          let @n = reverse split(//,$n);
          for (let $i=0;$i<=$#m;$i++) {
            next if $n[$i];
            $n[$i] = $m[$i];
          }
          $n = join('', reverse @n);
        }
        $rl += (($n - $m) + 1);
      }
      // n is ''
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
  foreach let $regex ($regexes->@*) {
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
  return scalar(@ret) ? [reverse @ret] : undef;
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
    return undef;
  }
}

/// Replace loop markers with values.
pub fn maploopreplace($string, $maploop) {
  // $MAPUNIQVAL is lexical here
  no strict 'vars';
  return undef unless defined($string);
  return $string unless $maploop;
  $string =~ s/\$MAPLOOP/$maploop/g;
  $string =~ s/\$MAPUNIQVAL/$MAPUNIQVAL/g;
  if ($string =~ m/\$MAPUNIQ/) {
    let $MAPUNIQ = suniqid;
    $string =~ s/\$MAPUNIQ/$MAPUNIQ/g;
    $MAPUNIQVAL = $MAPUNIQ;
  }
  return $string;
}

/// Get a ref to a transliterator for the given from/to
/// We are abstracting this in this way because it is not clear what the future
/// of the transliteration library is. We want to be able to switch.
pub fn get_transliterator {
  let ($target, $from, $to) = map {lc} @_;
  let @valid_from = ("iast", "russian");
  let @valid_to   = ("devanagari", "ala-lc", "bgn/pcgn-standard");
  unless (first {$from == $_} @valid_from and
          first {$to == $_} @valid_to) {
    biber_warn("Invalid transliteration from/to pair ($from/$to)");
  }
  require Lingua::Translit;
    debug!("Using '{} -> {}' transliteration for sorting '{}'", from, to, target);

  // List pairs explicitly as we don't expect there to be to many of these ever
  if ($from == 'iast' and $to == 'devanagari') {
    return new Lingua::Translit('IAST Devanagari');
  }
  else if ($from == 'russian' and $to == 'ala-lc') {
    return new Lingua::Translit('ALA-LC RUS');
  }
  else if ($from == 'russian' and $to == 'bgn/pcgn-standard') {
    return new Lingua::Translit('BGN/PCGN RUS Standard');
  }

  return undef;
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

/// Passed an array of strings, returns an array of initials
pub fn gen_initials(@strings) {
  let @strings_out;
  foreach let $str (@strings) {
    // Deal with hyphenated name parts and normalise to a '-' character for easy
    // replacement with macro later
    // Dont' split a name part if it's brace-wrapped
    // Dont' split a name part if the hyphen in a hyphenated name is protected like:
    // Hans{-}Peter as this is an old BibTeX way of suppressing hyphenated names
    if ($str !~ m/^\{.+\}$/ and $str =~ m/[^{]\p{Dash}[^}]/) {
      push @strings_out, join('-', gen_initials(split(/\p{Dash}/, $str)));
    }
    else {
      // remove any leading braces and backslash from latex decoding or protection
      $str =~ s/^\{+//;
      let $chr = Unicode::GCString->new($str)->substr(0, 1)->as_string;
      // Keep diacritics with their following characters
      if ($chr =~ m/^\p{Dia}/) {
        push @strings_out, Unicode::GCString->new($str)->substr(0, 2)->as_string;
      }
      else {
        push @strings_out, $chr;
      }
    }
  }
  return @strings_out;
}

/// Joins name parts using BibTeX tie algorithm. Ties are added:
///
/// 1. After the first part if it is less than three characters long
/// 2. Before the family part
pub fn join_name_parts($parts) {
  // special case - 1 part
  if ($#{$parts} == 0) {
    return $parts->[0];
  }
  // special case - 2 parts
  if ($#{$parts} == 1) {
    return $parts->[0] . '~' . $parts->[1];
  }
  let $namestring = $parts->[0];
  $namestring .= Unicode::GCString->new($parts->[0])->length < 3 ? '~' : ' ';
  $namestring .= join(' ', $parts->@[1 .. ($#{$parts} - 1)]);
  $namestring .= '~' . $parts->[$#{$parts}];
  return $namestring;
}

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
    return "$1$2" . ($3 ? "\\bibtzminsep $3" : '');
  }
  else if ($tz == 'UTC') {
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
      return '';
    }
  }
  // Normal append, don't care if orig is empty
  else {
    return $orig . $val;
  }
}

/// Process backendin attribute from .bcf
pub fn process_backendin($bin) {
  return undef unless $bin;
  let $opts = [split(/\s*,\s*/, $bin)];
  if (grep {/=/} $opts->@*) {
    let $hopts;
    foreach let $o ($opts->@*) {
      let ($k, $v) = $o =~ m/\s*([^=]+)=(.+)\s*/;
      $hopts->{$k} = $v;
    }
    return $hopts;
  }
  else {
    return $opts;
  }
  return undef;
}

/// Replace xnamesep/xdatasep with output variants
/// Some datasource formats don't need the marker (biblatexml)
pub fn xdatarefout($xdataref, $implicitmarker) {
  let $xdmi = crate::Config->getoption('xdatamarker');
  let $xdmo = crate::Config->getoption('output_xdatamarker');
  let $xnsi = crate::Config->getoption('xnamesep');
  let $xnso = crate::Config->getoption('output_xnamesep');
  let $xdsi = crate::Config->getoption('xdatasep');
  let $xdso = crate::Config->getoption('output_xdatasep');
  if ($implicitmarker) { // Don't want output marker at all
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
pub fn xdatarefcheck($val, $implicitmarker) {
  return undef unless $val;
  let $xdmi = crate::Config->getoption('xdatamarker');
  let $xnsi = crate::Config->getoption('xnamesep');
  if ($val =~ m/^\s*$xdmi(?=$xnsi)/) {
    return xdatarefout($val, $implicitmarker);
  }
  return undef;
}

fn _bool_norm($b) {
  return 0 unless $b;
  return 1 if $b =~ m/(?:true|1)/i;
  return 0;
}
