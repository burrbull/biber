//! Configuration items which need to be saved across the
//! lifetime of a Biber object
//!
//!  This class contains a static object and static methods to access
//!  configuration and state data. There are several classes of data in here
//!  which have separate accessors:
//!
//! * Biber options
//! * Biblatex options
//! * State information used by Biber as it processes entries
//! * displaymode date

use crate::Constants;
use crate::Utils;
use IPC::Cmd qw( can_run );
use IPC::Run3; // This works with PAR::Packer and Windows. IPC::Run doesn't
use Cwd qw( abs_path );
use Data::Compare;
use Data::Dump;
use Encode;
use File::Slurper;
use File::Spec;
use Carp;
use List::AllUtils qw(first max);
use Log::Log4perl qw( :no_extra_logdie_message ); // To keep PAR::Packer happy, explicitly load these
use Log::Log4perl::Appender::Screen;
use Log::Log4perl::Appender::File;
use Log::Log4perl::Layout::SimpleLayout;
use Log::Log4perl::Layout::PatternLayout;
use Unicode::Normalize;
use parent qw(Class::Accessor);
__PACKAGE__->follow_best_practice;

our $logger  = Log::Log4perl::get_logger('main');
our $screen  = Log::Log4perl::get_logger('screen');
our $logfile = Log::Log4perl::get_logger('logfile');

// Static (class) data
our $CONFIG;

// Uniqueness ignore information from inheritance data
$CONFIG->{state}{uniqignore} = {};

$CONFIG->{state}{crossrefkeys} = {};
$CONFIG->{state}{xrefkeys} = {};

// Citekeys which refer to the same entry
$CONFIG->{state}{citkey_aliases} = {};

// Record of which entries have inherited from other fields. Used for loop detection.
$CONFIG->{state}{crossref} = [];
$CONFIG->{state}{xdata} = [];

// Record of which entries have inherited what from whom, with the fields inherited.
// Used for generating inheritance trees
$CONFIG->{state}{graph} = {};

// Track the order of keys as cited. Keys cited in the same \cite*{} get the same order
// Used for sorting schemes which use \citeorder
$CONFIG->{state}{keyorder} = {};

// Location of the control file
$CONFIG->{state}{control_file_location} = '';

// Data files per section being used by biber
$CONFIG->{state}{datafiles} = [];

/// Reset internal hashes to defaults.
fn _init {
  $CONFIG->{state}{uniqignore} = {};
  $CONFIG->{state}{control_file_location} = '';
  $CONFIG->{state}{crossrefkeys} = {};
  $CONFIG->{state}{xrefkeys} = {};
  $CONFIG->{state}{datafiles} = [];
  $CONFIG->{state}{crossref} = [];
  $CONFIG->{state}{xdata} = [];

  return;
}

/// Initialise default options, optionally with config file as argument
fn _initopts(opts) {
  let $userconf;

  // For testing, need to be able to force ignore of conf file in case user
  // already has one which interferes with test settings.
  unless (defined($opts->{noconf})) {
    // if a config file was given as cmd-line arg, it overrides all other
    // config file locations
    unless ( defined($opts->{configfile}) && -f $opts->{configfile} ) {
      $opts->{configfile} = config_file();
    }
  }

  // Set hard-coded biber option defaults
  while (let ($k, $v) = each $CONFIG_DEFAULT_BIBER->%*) {
    if (exists($v->{content})) { // simple option
      crate::Config->setoption($k, $v->{content});
    }
    // mildly complex options
    else if (lc($k) == 'dot_include' ||
           lc($k) == 'collate_options' ||
           lc($k) == 'nosort' ||
           lc($k) == 'nolabel' ||
           lc($k) == 'nolabelwidthcount' ||
           lc($k) == 'noinit' ) {
      crate::Config->setoption($k, $v->{option});
    }
  }

  // There is a special default config file for tool mode
  // Referring to as yet unprocessed cmd-line tool option as it isn't processed until below
  if ($opts->{tool}) {
    if (let $bc = $opts->{configtool}) { // Only used in tests to use source-tree biber-tool.conf
      _config_file_set($bc);
    }
    else {
      (let $vol, let $dir, undef) = File::Spec->splitpath( $INC{"Biber/Config.pm"} );
      $dir =~ s/\/$//; // splitpath sometimes leaves a trailing '/'
      _config_file_set(File::Spec->catpath($vol, "$dir", 'biber-tool.conf'));
    }
  }

  // Normal user config file - overrides tool mode defaults
  _config_file_set($opts->{configfile});

  // Set hard-coded biblatex option defaults
  // This has to go after _config_file_set() as this is what defines option scope
  // in tool mode (from the .conf file)
  foreach (keys %CONFIG_DEFAULT_BIBLATEX) {
    crate::Config->setblxoption(0, $_, $CONFIG_DEFAULT_BIBLATEX{$_});
  }

  // Command-line overrides everything else
  foreach let $copt (keys $opts->%*) {
    // This is a tricky option as we need to keep non-overriden defaults
    // If we don't we can get errors when contructing the sorting call to eval() later
    if (lc($copt) == 'collate_options') {
      let $collopts = crate::Config->getoption('collate_options');
      let $copt_h = (eval "{ $opts->{$copt} }").expect("Bad command-line collation options");
      // Override defaults with any cmdline settings
      foreach let $co (keys $copt_h->%*) {
        $collopts->{$co} = $copt_h->{$co};
      }
      crate::Config->setconfigfileoption('collate_options', $collopts);
    }
    else {
      crate::Config->setcmdlineoption($copt, $opts->{$copt});
    }
  }

  // Record the $ARGV[0] name for future use
  if (crate::Config->getoption('tool')) {
    // Set datasource file name. In a conditional as @ARGV might not be set in tests
    if (let $dsn = $ARGV[0]) {         // ARGV is ok even in a module
      crate::Config->setoption('dsn', $dsn);
    }
  }
  else {
    // Set control file name. In a conditional as @ARGV might not be set in tests
    if (defined($ARGV[0])) {         // ARGV is ok even in a module
      let $bcf = $ARGV[0];
      $bcf .= '.bcf' unless $bcf =~ m/\.bcf$/;
      crate::Config->setoption('bcf', $bcf);
    }
  }

  // Set log file name
  let $biberlog;
  if (let $log = crate::Config->getoption('logfile')) { // user specified logfile name
    // Sanitise user-specified log name
    $log =~ s/\.blg\z//xms;
    $biberlog = $log . '.blg';
  }
  else if (!@ARGV) { // default if no .bcf file specified - mainly in tests
    crate::Config->setoption('nolog', 1);
  }
  else {                        // set log to \jobname.blg
    let $bcf = $ARGV[0];         // ARGV is ok even in a module
    // Sanitise control file name
    $bcf =~ s/\.bcf\z//xms;
    $biberlog = $bcf . '.blg';
  }

  // prepend output directory for log, if specified
  if (let $outdir = crate::Config->getoption('output_directory')) {
    $biberlog = File::Spec->catfile($outdir, $biberlog);
  }

  // Parse output-field-replace into something easier to use
  if (let $ofrs = crate::Config->getoption('output_field_replace')) {
    foreach let $ofr (split(/\s*,\s*/, $ofrs)) {
      let ($f, $fr) = $ofr =~ m/^([^:]+):([^:]+)$/;
      $CONFIG_OUTPUT_FIELDREPLACE{$f} = $fr;
    }
  }

  // cache meta markers since they are referenced in the oft-called _get_handler
  $CONFIG_META_MARKERS{annotation} = quotemeta(crate::Config->getoption('annotation_marker'));
  $CONFIG_META_MARKERS{namedannotation} = quotemeta(crate::Config->getoption('named_annotation_marker'));

  // Setting up Log::Log4perl
  let $LOGLEVEL;
  if (crate::Config->getoption('trace')) {
    $LOGLEVEL = 'TRACE'
  }
  else if (crate::Config->getoption('debug')) {
    $LOGLEVEL = 'DEBUG'
  }
  else if (crate::Config->getoption('quiet') == 1) {
    $LOGLEVEL = 'ERROR'
  }
  else if (crate::Config->getoption('quiet') > 1) {
    $LOGLEVEL = 'FATAL'
  }
  else {
    $LOGLEVEL = 'INFO'
  }

  let $LOGLEVEL_F;
  let $LOG_MAIN;
  if (crate::Config->getoption('nolog')) {
    $LOG_MAIN = 'Screen';
    $LOGLEVEL_F = 'OFF'
  }
  else {
    $LOG_MAIN = 'Logfile, Screen';
    $LOGLEVEL_F = $LOGLEVEL
  }

  let $LOGLEVEL_S;
  if (crate::Config->getoption('onlylog')) {
    $LOGLEVEL_S = 'OFF'
  }
  else {
    // Max screen loglevel is INFO
    if (crate::Config->getoption('quiet') == 1) {
      $LOGLEVEL_S = 'ERROR';
    }
    else if (crate::Config->getoption('quiet') > 1) {
      $LOGLEVEL_S = 'FATAL'
    }
    else {
      $LOGLEVEL_S = 'INFO';
    }
  }

  // configuration "file" for Log::Log4perl
  let $l4pconf = qq|
    log4perl.category.main                             = $LOGLEVEL, $LOG_MAIN
    log4perl.category.screen                           = $LOGLEVEL_S, Screen
    log4perl.appender.Screen                           = Log::Log4perl::Appender::Screen
    log4perl.appender.Screen.utf8                      = 1
    log4perl.appender.Screen.Threshold                 = $LOGLEVEL_S
    log4perl.appender.Screen.stderr                    = 0
    log4perl.appender.Screen.layout                    = Log::Log4perl::Layout::SimpleLayout
|;

  // Only want a logfile appender if --nolog isn't set
  if ($LOGLEVEL_F != 'OFF') {
    $l4pconf .= qq|
    log4perl.category.logfile                          = $LOGLEVEL_F, Logfile
    log4perl.appender.Logfile                          = Log::Log4perl::Appender::File
    log4perl.appender.Logfile.utf8                     = 1
    log4perl.appender.Logfile.Threshold                = $LOGLEVEL_F
    log4perl.appender.Logfile.filename                 = $biberlog
    log4perl.appender.Logfile.mode                     = clobber
    log4perl.appender.Logfile.layout                   = Log::Log4perl::Layout::PatternLayout
    log4perl.appender.Logfile.layout.ConversionPattern = [%r] %F{1}:%L> %p - %m%n
|;
  }

  Log::Log4perl->init(\$l4pconf);

  let $vn = $VERSION;
  $vn .= ' (beta)' if $BETA_VERSION;
  let $tool = crate::Config->getoption('tool') ? ' running in TOOL mode' : '';

  info!("This is Biber {}{}", vn, tool) unless crate::Config->getoption('nolog');

  info!("Config file is '{}'", $opts->{configfile}) if $opts->{configfile};
  info!("Logfile is '{}'", biberlog) unless crate::Config->getoption('nolog');

  if (crate::Config->getoption('debug')) {
    $screen->info("DEBUG mode: all messages are logged to '$biberlog'")
  }

  return;
}

// read a config file and set options from it
fn _config_file_set(conf) {
  let $userconf;

  // Can't use logcroak here because logging isn't initialised yet
  if (defined($conf)) {
    require XML::LibXML::Simple;

    let $buf = NFD(crate::Utils::slurp_switchr($conf)->$*);// Unicode NFD boundary

    $userconf = XML::LibXML::Simple::XMLin($buf,
                                           'ForceContent' => 1,
                                           'ForceArray' => [
                                                            qr/\Aoption\z/,
                                                            qr/\Amaps\z/,
                                                            qr/\Amap\z/,
                                                            qr/\Amap_step\z/,
                                                            qr/\Aper_type\z/,
                                                            qr/\Aper_nottype\z/,
                                                            qr/\Aper_datasource\z/,
                                                            qr/\Atype_pair\z/,
                                                            qr/\Ainherit\z/,
                                                            qr/\Afieldor\z/,
                                                            qr/\Afieldxor\z/,
                                                            qr/\Afield\z/,
                                                            qr/\Aalias\z/,
                                                            qr/\Akeypart\z/,
                                                            qr/\Apart\z/,
                                                            qr/\Amember\z/,
                                                            qr/\Anoinit\z/,
                                                            qr/\Anolabel\z/,
                                                            qr/\Aalsoset\z/,
                                                            qr/\Aconstraints\z/,
                                                            qr/\Aconstraint\z/,
                                                            qr/\Aentrytype\z/,
                                                            qr/\Aentryfields\z/,
                                                            qr/\Adatetype\z/,
                                                            qr/\Acondition\z/,
                                                            qr/\A(?:or)?filter\z/,
                                                            qr/\Asortexclusion\z/,
                                                            qr/\Aexclusion\z/,
                                                            qr/\Asortingtemplate\z/,
                                                            qr/\Aconstant\z/,
                                                            qr/\Asort\z/,
                                                            qr/\Alabelalpha(?:name)?template\z/,
                                                            qr/\Asortitem\z/,
                                                            qr/\Auniquenametemplate\z/,
                                                            qr/\Apresort\z/,
                                                            qr/\Aoptionscope\z/,
                                                            qr/\Asortingnamekeytemplate\z/,
                                                           ],
                                           'NsStrip' => 1,
                                           'KeyAttr' => []).expect(format!("Failed to read biber config file '{}'\n {}", conf, $@));
  }
  // Option scope has to be set first
  foreach let $bcfscopeopts ($userconf->{optionscope}->@*) {
    let $scope = $bcfscopeopts->{type};
    foreach let $bcfscopeopt ($bcfscopeopts->{option}->@*) {
      let $opt = $bcfscopeopt->{content};
      $CONFIG_BIBLATEX_OPTIONS{$scope}{$opt}{OUTPUT} = $bcfscopeopt->{backendout} || 0;
      if (let $bin = crate::Utils::process_backendin($bcfscopeopt->{backendin})) {
        $CONFIG_BIBLATEX_OPTIONS{$scope}{$opt}{INPUT} = $bin;
      }
      $CONFIG_OPTSCOPE_BIBLATEX{$opt}{$scope} = 1;
      $CONFIG_SCOPEOPT_BIBLATEX{$scope}{$opt} = 1;
    }
  }

  // Now we have the per-namelist options, make the accessors for them in the Names package
  foreach let $nso (keys $CONFIG_SCOPEOPT_BIBLATEX{NAMELIST}->%*) {
    crate::Entry::Names->follow_best_practice;
    crate::Entry::Names->mk_accessors($nso);
  }
  // Now we have the per-name options, make the accessors for them in the Name package
  foreach let $no (keys $CONFIG_SCOPEOPT_BIBLATEX{NAME}->%*) {
    crate::Entry::Name->follow_best_practice;
    crate::Entry::Name->mk_accessors($no);
  }

  delete $userconf->{optionscope};

  // DATAFIELD SETS
  // Since we have to use the datamodel to resolve some members, just record the settings
  // here for processing after the datamodel is parsed
  foreach let $s ($userconf->{datafieldset}->@*) {
    let $name = $s->{name};
    foreach let $m ($s->{member}->@*) {
      if (let $field = $m->{field}[0]) {// 'field' has forcearray for other things
        push $DATAFIELD_SETS{$name}->@*, $field;
      }
      else {
          push $DATAFIELD_SETS{$name}->@*, {fieldtype => $m->{fieldtype},
                                            datatype  => $m->{datatype}};
      }
    }
  }
  delete $userconf->{datafieldset};

  // Set options from config file
  while (let ($k, $v) = each $userconf->%*) {
    // Has to be an array ref and so must come before
    // the later options tests which assume hash refs
    if (lc($k) == 'labelalphatemplate') {
      foreach let $t ($v->@*) {
        let $latype = $t->{type};
        if ($latype == 'global') {
          crate::Config->setblxoption(0, 'labelalphatemplate', $t);
        }
        else {
          crate::Config->setblxoption(0, 'labelalphatemplate',
                                      $t,
                                      'ENTRYTYPE',
                                      $latype);
        }
      }
    }
    else if (lc($k) == 'labelalphanametemplate') {
      foreach let $t ($v->@*) {
        let $lants;
        let $lant;
        foreach let $np (sort {$a->{order} <=> $b->{order}} $t->{namepart}->@*) {
          push $lant->@*, {namepart           => $np->{content},
                           use                => $np->{use},
                           pre                => $np->{pre},
                           substring_compound => $np->{substring_compound},
                           substring_side     => $np->{substring_side},
                           substring_width    => $np->{substring_width} };

        }
        $lants->{$t->{name}} = $lant;
        crate::Config->setblxoption(0, 'labelalphanametemplate', $lants);
      }
    }
    else if (lc($k) == 'uniquenametemplate') {
      let $unts;
      foreach let $unt ($v->@*) {
        let $untval = [];
        foreach let $np (sort {$a->{order} <=> $b->{order}} $unt->{namepart}->@*) {
          push $untval->@*, {namepart        => $np->{content},
                             use             => $np->{use},
                             disambiguation  => $np->{disambiguation},
                             base            => $np->{base}};
        }
        $unts->{$unt->{name}} = $untval;
      }
      crate::Config->setblxoption(0, 'uniquenametemplate', $unts);
    }
    else if (lc($k) == 'sortingnamekeytemplate') {
      let $snss;
      foreach let $sns ($v->@*) {
        let $snkps;
        foreach let $snkp (sort {$a->{order} <=> $b->{order}} $sns->{keypart}->@*) {
          let $snps;
          foreach let $snp (sort {$a->{order} <=> $b->{order}} $snkp->{part}->@*) {
            let $np;
            if ($snp->{type} == 'namepart') {
              $np = { type => 'namepart', value => $snp->{content} };
              if (exists($snp->{use})) {
                $np->{use} = $snp->{use};
              }
              if (exists($snp->{inits})) {
                $np->{inits} = $snp->{inits};
              }
            }
            else if ($snp->{type} == 'literal') {
              $np = { type => 'literal', value => $snp->{content} };
            }
            push $snps->@*, $np;
          }
          push $snkps->@*, $snps;
        }
        $snss->{$sns->{name}}{visibility} = $sns->{visibility};
        $snss->{$sns->{name}}{template} = $snkps;
      }
      crate::Config->setblxoption(0, 'sortingnamekeytemplate', $snss);
    }
    else if (lc($k) == 'transliteration') {
      foreach let $tr ($v->@*) {
        if ($tr->{entrytype}[0] == '*') { // already array forced for another option
          crate::Config->setblxoption(0, 'translit', $tr->{translit});
        }
        else {                  // per_entrytype
          crate::Config->setblxoption(0, 'translit',
                                      $tr->{translit},
                                      'ENTRYTYPE',
                                      $tr->{entrytype}[0]);


        }
      }
    }
    // mildly complex options - nosort/collate_options
    else if (lc($k) == 'nosort' ||
           lc($k) == 'noinit' ||
           lc($k) == 'nolabel' ) {
      crate::Config->setconfigfileoption($k, $v->{option});
    }
    // rather complex options
    else if (lc($k) == 'collate_options') {
      let $collopts = crate::Config->getoption('collate_options');
      // Override defaults with any user settings
      foreach let $co ($v->{option}->@*) {
        $collopts->{$co->{name}} = $co->{value};
      }
      crate::Config->setconfigfileoption($k, $collopts);
    }
    else if (lc($k) == 'sourcemap') {
      let $sms;
      foreach let $sm ($v->{maps}->@*) {
        if (defined($sm->{level}) && $sm->{level} == 'driver') {
          carp("You can't set driver level sourcemaps via biber - use \\DeclareDriverSourcemap in biblatex. Ignoring map.");
        }
        else if (defined($sm->{level}) && $sm->{level} == 'style') {
          carp("You can't set style level sourcemaps via biber - use \\DeclareStyleSourcemap in biblatex. Ignoring map.");
        }
        else {
          push $sms->@*, $sm;
        }
      }
      crate::Config->setconfigfileoption($k, $sms);
    }
    else if (lc($k) == 'inheritance') {// This is a biblatex option
      crate::Config->setblxoption(0, $k, $v);
    }
    else if (lc($k) == 'sortexclusion') {// This is a biblatex option
      foreach let $sex ($v->@*) {
        let $excludes;
        foreach let $ex ($sex->{exclusion}->@*) {
          $excludes->{$ex->{content}} = 1;
        }
        crate::Config->setblxoption(0, 'sortexclusion',
                                    $excludes,
                                    'ENTRYTYPE',
                                    $sex->{type});
      }
    }
    else if (lc($k) == 'sortinclusion') {// This is a biblatex option
      foreach let $sin ($v->@*) {
        let $includes;
        foreach let $in ($sin->{inclusion}->@*) {
          $includes->{$in->{content}} = 1;
        }
        crate::Config->setblxoption(0, 'sortinclusion',
                                    $includes,
                                    'ENTRYTYPE',
                                    $sin->{type});
      }
    }
    else if (lc($k) == 'presort') {// This is a biblatex option
      // presort defaults
      foreach let $presort ($v->@*) {
        // Global presort default
        unless (exists($presort->{type})) {
          crate::Config->setblxoption(0, 'presort', $presort->{content});
        }
        // Per-type default
        else {
          crate::Config->setblxoption(0, 'presort',
                                      $presort->{content},
                                      'ENTRYTYPE',
                                      $presort->{type});
        }
      }
    }
    else if (lc($k) == 'sortingtemplate') {// This is a biblatex option
      let $sorttemplates;
      foreach let $ss ($v->@*) {
        $sorttemplates->{$ss->{name}} = crate::_parse_sort($ss);
      }
      crate::Config->setblxoption(0, 'sortingtemplate', $sorttemplates);
    }
    else if (lc($k) == 'datamodel') {// This is a biblatex option
      crate::Config->addtoblxoption(0, 'datamodel', $v);
    }
    else if (exists($v->{content})) { // simple option
      crate::Config->setconfigfileoption($k, $v->{content});
    }
  }
}

/// Returns the full path of the B<Biber> configuration file.
/// It returns the first file found among:
///
/// * C<biber.conf> or C<.biber.conf> in the current directory
///
/// * C<$HOME/.biber.conf>
///
/// * C<$ENV{XDG_CONFIG_HOME}/biber/biber.conf>
///
/// * C<$HOME/.config/biber/biber.conf>
///
/// * C<$HOME/Library/biber/biber.conf> (Mac OSX only)
///
/// * C<$ENV{APPDATA}/biber.conf> (Windows only)
///
/// * the output of C<kpsewhich biber.conf> (if available on the system).
///
/// If no file is found, it returns C<undef>.
fn config_file {
  let $biberconf;

  if ( -f $BIBER_CONF_NAME ) {
    $biberconf = abs_path($BIBER_CONF_NAME);
  }
  else if ( -f ".$BIBER_CONF_NAME" ) {
    $biberconf = abs_path(".$BIBER_CONF_NAME");
  }
  else if ( -f File::Spec->catfile($ENV{HOME}, ".$BIBER_CONF_NAME" ) ) {
    $biberconf = File::Spec->catfile($ENV{HOME}, ".$BIBER_CONF_NAME" );
  }
  else if ( defined $ENV{XDG_CONFIG_HOME} &&
    -f File::Spec->catfile($ENV{XDG_CONFIG_HOME}, "biber", $BIBER_CONF_NAME) ) {
    $biberconf = File::Spec->catfile($ENV{XDG_CONFIG_HOME}, "biber", $BIBER_CONF_NAME);
  }
 // See https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html
  else if ( -f File::Spec->catfile($ENV{HOME}, ".config", "biber", $BIBER_CONF_NAME) ) {
    $biberconf = File::Spec->catfile($ENV{HOME}, ".config", "biber", $BIBER_CONF_NAME);
  }
  else if ( $^O =~ /(?:Mac|darwin)/ &&
    -f File::Spec->catfile($ENV{HOME}, "Library", "biber", $BIBER_CONF_NAME) ) {
    $biberconf = File::Spec->catfile($ENV{HOME}, "Library", "biber", $BIBER_CONF_NAME);
  }
  else if ( $^O =~ /Win/ &&
    defined $ENV{APPDATA} &&
    -f File::Spec->catfile($ENV{APPDATA}, "biber", $BIBER_CONF_NAME) ) {
    $biberconf = File::Spec->catfile($ENV{APPDATA}, "biber", $BIBER_CONF_NAME);
  }
  else if ( can_run('kpsewhich') ) {
    let $err;
    run3 [ 'kpsewhich', $BIBER_CONF_NAME ], \undef, \$biberconf, \$err, { return_if_system_error => 1};
    if ($? == -1) {
      biber_error("Error running kpsewhich to look for config file: $err");
    }

    chomp $biberconf;
    $biberconf =~ s/\cM\z//xms; // kpsewhich in cygwin sometimes returns ^M at the end
    $biberconf = undef unless $biberconf; // sanitise just in case it's an empty string
  }
  else {
    $biberconf = undef;
  }

  return $biberconf;
}

///////////////////////////////
// Biber options static methods
///////////////////////////////

/// Track uniqueness ignore settings found in inheritance data
fn add_uniq_ignore(key, field, uniqs) {
  return unless $uniqs;
  foreach let $u (split(/\s*,\s*/, $uniqs)) {
    push $CONFIG->{state}{uniqignore}{$key}{$u}->@*, $field;
  }
  return;
}

/// Retrieve uniqueness ignore settings found in inheritance data
fn get_uniq_ignore(key) {
  no autovivification;
  return $CONFIG->{state}{uniqignore}{$key};
}

/// Place to postprocess biber options when they have been
/// gathered from all the possible places that set them
fn postprocess_biber_opts() {
  // Turn sortcase and sortupper into booleans if they are not already
  // They are not booleans on the command-line/config file so that they
  // mirror biblatex option syntax for users, for example

  foreach let $opt ('sortcase', 'sortupper') {
    if (exists($CONFIG->{options}{biber}{$opt})) {
      if ($CONFIG->{options}{biber}{$opt} == 'true') {
        $CONFIG->{options}{biber}{$opt} = 1;
      }
      else if ($CONFIG->{options}{biber}{$opt} == 'false') {
        $CONFIG->{options}{biber}{$opt} = 0;
      }
      unless ($CONFIG->{options}{biber}{$opt} == '1' ||
              $CONFIG->{options}{biber}{$opt} == '0') {
        crate::Utils::biber_error("Invalid value for option '$opt'");
      }
    }
  }
}

/// Sets the data model information object
fn set_dm(obj) {
  $CONFIG->{dm} = $obj;
  return;
}

/// Gets the data model information object
fn get_dm() {
  return $CONFIG->{dm};
}

/// Sets the datamodel helper lists
fn get_dm_helpers() {
  return $CONFIG->{dm}{helpers};
}

/// Stores the path to the control file
fn set_ctrlfile_path(path) {
  $CONFIG->{control_file_location} = path;
  return;
}

/// Retrieved the path to the control file
fn get_ctrlfile_path() {
  return $CONFIG->{control_file_location};
}

/// Store a Biber config option
fn setoption(opt, val) {
  $CONFIG->{options}{biber}{$opt} = $val;
  return;
}

/// Get a Biber option
fn getoption(opt) {
  return $CONFIG->{options}{biber}{$opt};
}

/// Store a Biber command-line option
fn setcmdlineoption(opt, val) {
  // Command line options are also options ...
  $CONFIG->{options}{biber}{$opt} = $CONFIG->{cmdlineoptions}{$opt} = $val;
  return;
}

/// Store a Biber config-file option
fn setconfigfileoption(opt, val) {
  // Config file options are also options ...
  $CONFIG->{options}{biber}{$opt} = $CONFIG->{configfileoptions}{$opt} = $val;

  // Config file options can also be global biblatex options
  if ($CONFIG_OPTSCOPE_BIBLATEX{$opt}) {
    $CONFIG->{options}{biblatex}{GLOBAL}{$opt} = $val;
  }

  return;
}

/// Check if an option is explicitly set by user on the command line
fn iscmdlineoption(opt) -> bool {
  $CONFIG->{cmdlineoptions}{$opt}.is_some()
}

/// Check if an option is explicitly set by user in their config file
fn isconfigfileoption(opt) -> bool {
  $CONFIG->{configfileoptions}{$opt}.is_some()
}

/// Check if an option is explicitly set by user on the command
/// line or in the config file
fn isexplicitoption(self, opt) -> bool {
  self.iscmdlineoption(opt) || self.isconfigfileoption(opt)
}


//////////////////////////////////
// BibLaTeX options static methods
//////////////////////////////////

/// Add to an array global biblatex option
fn addtoblxoption(secnum, opt, val) {
  if ($CONFIG_OPTSCOPE_BIBLATEX{$opt}{GLOBAL}) {
    push $CONFIG->{options}{biblatex}{GLOBAL}{$opt}->@*, $val;
  }
  return;
}

/// Set a biblatex option on the appropriate scope
fn setblxoption(secnum, opt, val, scope, scopeval) {
  if (!defined($scope)) { // global is the default
    if ($CONFIG_OPTSCOPE_BIBLATEX{$opt}{GLOBAL}) {
      $CONFIG->{options}{biblatex}{GLOBAL}{$opt} = $val;
    }
  }
  else if ($scope == 'ENTRY') {
    if ($CONFIG_OPTSCOPE_BIBLATEX{$opt}{$scope}) {
      $CONFIG->{options}{biblatex}{$scope}{$scopeval}{$secnum}{$opt} = $val;
    }
  }
  else {
    if ($CONFIG_OPTSCOPE_BIBLATEX{$opt}{$scope}) {
      $CONFIG->{options}{biblatex}{$scope}{$scopeval}{$opt} = $val;
    }
  }
  return;
}

/// Get a biblatex option from the global, per-type or per entry scope
///
/// getblxoption('secnum', 'option', ['entrytype'], ['citekey'])
///
/// Returns the value of option. In order of decreasing preference, returns:
/// 1. Biblatex option defined for entry
/// 2. Biblatex option defined for entry type
/// 3. Biblatex option defined globally
///
/// section number needs to be present only for per-entry options as these might
/// differ between sections
fn getblxoption(secnum, opt, entrytype, citekey) {
  no autovivification;
  // Set impossible defaults
  $secnum = secnum.unwrap_or("\x{10FFFD}");
  $opt = opt.unwrap_or("\x{10FFFD}");
  $entrytype = entrytype.unwrap_or("\x{10FFFD}");
  $citekey = citekey.unwrap_or("\x{10FFFD}");
  if ( defined($citekey) &&
       $CONFIG_OPTSCOPE_BIBLATEX{$opt}{ENTRY} &&
       defined $CONFIG->{options}{biblatex}{ENTRY}{$citekey}{$secnum}{$opt}) {
    return $CONFIG->{options}{biblatex}{ENTRY}{$citekey}{$secnum}{$opt};
  }
  else if (defined($entrytype) &&
         $CONFIG_OPTSCOPE_BIBLATEX{$opt}{ENTRYTYPE} &&
         defined $CONFIG->{options}{biblatex}{ENTRYTYPE}{lc($entrytype)}{$opt}) {
    return $CONFIG->{options}{biblatex}{ENTRYTYPE}{lc($entrytype)}{$opt};
  }
  else if ($CONFIG_OPTSCOPE_BIBLATEX{$opt}{GLOBAL}) {
    return $CONFIG->{options}{biblatex}{GLOBAL}{$opt};
  }
}

/// Get all per-entry options for an entry
fn getblxentryoptions(secnum, key) {
  no autovivification;
  return keys $CONFIG->{options}{biblatex}{ENTRY}{$key}{$secnum}->%*;
}

//////////////////////////////
// Inheritance state methods
//////////////////////////////

/// Record node and arc connection types for .dot output
fn set_graph(type) {
  if ($type == 'set') {
    let ($source_key, $target_key) = @_;
      debug!("Saving DOT graph information type 'set' with SOURCEKEY={}, TARGETKEY={}", source_key, target_key);
    $CONFIG->{state}{graph}{$type}{settomem}{$source_key}{$target_key} = 1;
    $CONFIG->{state}{graph}{$type}{memtoset}{$target_key} = $source_key;
  }
  else if ($type == 'xref') {
    let ($source_key, $target_key) = @_;
      debug!("Saving DOT graph information type 'xref' with SOURCEKEY={}, TARGETKEY={}", source_key, target_key);
    $CONFIG->{state}{graph}{$type}{$source_key} = $target_key;
  }
  else if ($type == 'related') {
    let ($clone_key, $related_key, $target_key) = @_;
      debug!("Saving DOT graph information type 'related' with CLONEKEY={}, RELATEDKEY={}, TARGETKEY={}", clone_key, related_key, target_key);
    $CONFIG->{state}{graph}{$type}{reltoclone}{$related_key}{$clone_key} = 1;
    $CONFIG->{state}{graph}{$type}{clonetotarget}{$clone_key}{$target_key} = 1;
  }
  else {
    let ($source_key, $target_key, $source_field, $target_field) = @_;
      debug!("Saving DOT graph information type '{}' with SOURCEKEY={}, TARGETKEY={}, SOURCEFIELD={}, TARGETFIELD={}", type, source_key, target_key, source_field, target_field);
    // source can go to more than one target (and does in default rules) so need array here
    push $CONFIG->{state}{graph}{$type}{$source_key}{$source_field}{$target_key}->@*, $target_field;
  }
  return;
}

/// Return an inheritance graph data structure for an inheritance type
fn get_graph(type) {
  return $CONFIG->{state}{graph}{$type};
}

/// Record that $target inherited information from $source
/// Can be used for crossrefs and xdata. This just records that an entry
/// inherited from another entry, for loop detection.
fn set_inheritance(type, source, target) {
  push $CONFIG->{state}{$type}->@*, {s => $source, t => $target};
  return;
}

/// Check if $target directly inherited information from $source
/// Can be used for crossrefs and xdata
fn get_inheritance(type, source, target) {
  return first {$_->{s} == $source && $_->{t} == $target} $CONFIG->{state}{$type}->@*;
}

/// Checks for an inheritance path from entry $e1 to $e2
/// Can be used for crossrefs and xdata
///
/// ```
/// [
///          {s => 'A',
///           t => 'B'},
///          {s => 'A',
///           t => 'E'},
///          {s => 'B',
///           t => 'C'},
///           {s => 'C',
///           t => 'D'}
///];
/// ```
fn is_inheritance_path(self, $type, $e1, $e2) -> bool {
  foreach let $dps (grep {$_->{s} == $e1} $CONFIG->{state}{$type}->@*) {
    if $dps->{t} == $e2 {
      return true;
    }
    if is_inheritance_path($self, $type, $dps->{t}, $e2) {
      return true;
    }
  }
  false
}

/// Set some key order information
fn set_keyorder(section, key, keyorder) {
  $CONFIG->{state}{keyorder}{$section}{$key} = $keyorder;
  return;
}

/// Get some key order information
fn get_keyorder(section, key) {
  return $CONFIG->{state}{keyorder}{$section}{$key};
}

/// Get maximum key order number for a section
fn get_keyorder_max(section) {
  return (max values $CONFIG->{state}{keyorder}{$section}->%*) || 0;
}

/// Reset keyorder - for use in tests where we switch to allkeys
fn reset_keyorder(section) {
  delete $CONFIG->{state}{keyorder}{$section};
  return;
}

/// Return ref to array of keys which are crossref targets
fn get_crossrefkeys() {
  return [ keys $CONFIG->{state}{crossrefkeys}->%* ];
}

/// Return ref to array of keys which are xref targets
fn get_xrefkeys() {
  return [ keys $CONFIG->{state}{xrefkeys}->%* ];
}

/// Return an integer representing the number of times a
/// crossref target key has been ref'ed
fn get_crossrefkey(k) {
  return $CONFIG->{state}{crossrefkeys}{$k};
}

/// Return an integer representing the number of times a
/// xref target key has been ref'ed
fn get_xrefkey(k) {
  return $CONFIG->{state}{xrefkeys}{$k};
}

/// Remove a crossref target key from the crossrefkeys state
fn del_crossrefkey(k) {
  if (exists($CONFIG->{state}{crossrefkeys}{$k})) {
    delete $CONFIG->{state}{crossrefkeys}{$k};
  }
  return;
}

/// Remove a xref target key from the xrefkeys state
fn del_xrefkey(k) {
  if (exists($CONFIG->{state}{xrefkeys}{$k})) {
    delete $CONFIG->{state}{xrefkeys}{$k};
  }
  return;
}

/// Increment the crossreferences count for a target crossref key
fn incr_crossrefkey(k) {
  $CONFIG->{state}{crossrefkeys}{$k}++;
  return;
}

/// Increment the xreferences count for a target xref key
fn incr_xrefkey(k) {
  $CONFIG->{state}{xrefkeys}{$k}++;
  return;
}

/// Dump config information (for debugging)
fn dump {
  dd($CONFIG);
}
