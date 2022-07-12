/*
use parent qw(Exporter);
use crate::Config;
use Digest::MD5 qw( md5_hex );
use Encode;
use File::Slurper;
use File::Spec;
use IPC::Cmd qw( can_run );
use IPC::Run3; // This works with PAR::Packer and Windows. IPC::Run doesn't
use Unicode::Normalize;
use List::AllUtils qw (first);
use Log::Log4perl qw(:no_extra_logdie_message);
use XML::LibXML::Simple;
use Carp;
use utf8;
*/

use itertools::Itertools;
use lazy_regex::regex_replace_all;
use regex::{Regex, Captures};

/// Encode/Decode chars to/from UTF-8/lacros in LaTeX
///
/// ```   
/// use crate::LaTeX:Recode
///
/// let $string = 'Muḥammad ibn Mūsā al-Khwārizmī';
/// let $latex_string = latex_encode($string);
///     // => 'Mu\d{h}ammad ibn M\=us\=a al-Khw\=arizm\={\i}'
///
/// let $string = 'Mu\d{h}ammad ibn M\=us\=a al-Khw\=arizm\={\i}';
/// let $utf8_string   = latex_decode($string);
///     // => 'Muḥammad ibn Mūsā al-Khwārizmī'
/// ```
///
/// Allows conversion between Unicode chars and LaTeX macros.
///
/// GLOBAL OPTIONS
///
/// Possible values for the encoding/decoding set to use are "null", "base" and "full"; default
/// value is "base".
///
/// null  => No conversion
///
/// base  => Most common macros and diacritics (sufficient for Western languages
///          and common symbols)
///
/// full  => Also converts punctuation, larger range of diacritics and macros
///          (e.g. for IPA, Latin Extended Additional, etc.), symbols, Greek letters,
///          dingbats, negated symbols, and superscript characters and symbols ...

pub struct Recode;

use vars qw( $remap_d $remap_e $remap_e_raw $set_d $set_e );

impl Recode {
  /// Initialise recoding sets. We can't do this on loading the module as we don't have the config
  /// information to do this yet
  fn init_sets(set_d, set_e) {
    no autovivification;

    // Reset these, mostly for tests which call init_sets more than once
    $remap_d = {};
    $remap_e = {};
    remap_e_raw = HashSet::new();

    let $mapdata;
    // User-defined recode data file
    if (let $rdata = crate::Config->getoption("recodedata")) {
      let $err;
      if ( can_run("kpsewhich") ) {
        run3 [ "kpsewhich", $rdata ], \undef, \$mapdata, \$err, { return_if_system_error => 1};
        if ($? == -1) {
          biber_error("Error running kpsewhich to look for output_safechars data file: $err");
        }

        chomp $mapdata;
        $mapdata =~ s/\cM\z//xms; // kpsewhich in cygwin sometimes returns ^M at the end
        if !($mapdata) {
          $mapdata = None; // sanitise just in case it's an empty string
        }
      }
      else {
        biber_error("Can't run kpsewhich to look for output_safechars data file: $err");
      }
      info!("Using user-defined recode data file '{mapdata}'");
    }
    else {
      // we assume that the data file is in the same dir as the module
      (let $vol, let $data_path, _) = File::Spec->splitpath( $INC{"Biber/LaTeX/Recode.pm"} );

      // Deal with the strange world of Par::Packer paths, see similar code in Biber.pm

      if ($data_path =~ m|/par\-| && $data_path !~ m|/inc|) { // a mangled PAR @INC path
        $mapdata = File::Spec->catpath($vol, "$data_path/inc/lib/Biber/LaTeX/recode_data.xml");
      }
      else {
        $mapdata = File::Spec->catpath($vol, $data_path, "recode_data.xml");
      }
    }

    // Read driver config file
    let $xml = crate::Utils::slurp_switchr($mapdata)->$*;
    let $doc = XML::LibXML->load_xml(string => $xml);
    let $xpc = XML::LibXML::XPathContext->new($doc);

    let types = ["letters", "diacritics", "punctuation", "symbols", "negatedsymbols", "superscripts", "cmdsuperscripts", "dings", "greek"];

    // Have to have separate loops for decode/recode or you can't have independent decode/recode
    // sets

    // Construct decode set
    for tp in types {
      for maps in ($xpc->findnodes("/texmap/maps[\@type='$tp']")) {
        if !regex!(r"\s*,\s*").split(maps.getAttribute("set")).any(set_d) {
          continue;
        }
        for map in ($maps->findnodes("map")) {
          let $from = $map->findnodes("from")->shift();
          let $to = $map->findnodes("to")->shift();
          remap_d->{$tp}.map.insert(NFD($from->textContent()), NFD($to->textContent()));
        }
      }
      // Things we don't want to change when decoding as this breaks some things
      for d in ($xpc->findnodes('/texmap/decode_exclude/char')) {
        remap_d->{$tp}.map.remove(NFD($d->textContent()));
      }
    }

    // Construct encode set
    for tp in types {
      for maps in ($xpc->findnodes("/texmap/maps[\@type='$tp']")) {
        if !regex!(r"\s*,\s*").split(maps.getAttribute("set")).any(set_e) {
          continue;
        }
        for map in ($maps->findnodes("map")) {
          let $from = $map->findnodes("from")->shift();
          let $to = $map->findnodes("to")->shift();
          remap_e->{$tp}.map.insert(NFD($to->textContent()), NFD($from->textContent()));
        }
        // There are some duplicates in the data to handle preferred encodings.
        for map in ($maps->findnodes("map[from[@preferred]]")) {
          let $from = $map->findnodes("from")->shift();
          let $to = $map->findnodes("to")->shift();
          $remap_e->{$tp}.map.insert(NFD($to->textContent()), NFD($from->textContent()));
        }
        // Some things might need to be inserted as is rather than wrapped in some macro/braces
        for map in ($maps->findnodes("map[from[@raw]]")) {
          let $from = $map->findnodes("from")->shift();
          let $to = $map->findnodes("to")->shift();
          remap_e_raw.insert(NFD(to.textContent()));
        }

      }
      // Things we don't want to change when encoding as this would break LaTeX
      for e in ($xpc->findnodes("/texmap/encode_exclude/char")) {
        remap_e->{$type}.map.remove(NFD($e->textContent()));
      }
    }

    // Populate the decode regexps
    // sort by descending length of macro name to avoid shorter macros which are substrings
    // of longer ones damaging the longer ones
    for tp in types {
      if let Some(typ) = remap_d.get_mut(tp) {
        typ.re = typ.map
          .keys()
          .sorted_by(|a, b| b.len().cmp(a.len()))
          .map(|m| if regex_is_match!(r"[\.\^\|\+\-\)\(]", m) { format(r"\\{m}") } else { m })
          .join("|");
        typ.re = qr|typ.re|;
      }
    }

    // Populate the encode regexps
    for tp in types {
      if let Some(typ) = remap_e.get_mut(tp) {
        typ.re = typ.map
          .keys()
          .sorted()
          .map(|m| if regex_is_match!(r"[\.\^\|\+\-\)\(]", m) { format(r"\\{m}") } else { m })
          .join("|");
        typ.re = qr|typ.re|;
      }
    }
  }

  /// Converts LaTeX macros in the $text to Unicode characters.
  ///
  /// The function accepts a number of options:
  ///
  /// * normalize => $bool (default 1)
  ///     whether the output string should be normalized with Unicode::Normalize
  ///
  /// * normalization => <normalization form> (default "NFD")
  ///     and if yes, the normalization form to use (see the Unicode::Normalize documentation)
  pub fn latex_decode(text, %opts) {
        trace!("String before latex_decode() -> '{}'", text);

      let $norm      = exists $opts{normalize} ? $opts{normalize} : 1;
      let $norm_form = exists $opts{normalization} ? $opts{normalization} : "NFD";

      // Deal with raw TeX \char macros.
      let text = regex_replace_all!(r#"\\char"(\p{ASCII_Hex_Digit}+)"#, text, |_, digit| {
        char::from_u32(u32::from_str_radix(digit, 16).unwrap())
          .unwrap()
          .to_string()
      }); // hex chars
      let text = regex_replace_all!(r"\\char'(\d+)", &text, |_, digit| {
        char::from_u32(u32::from_str_radix(digit, 8).unwrap())
          .unwrap()
          .to_string()
      }); // octal chars
      let text = regex_replace_all!(r"\\char(\d+)", &text, |_, digit| {
        char::from_u32(u32::from_str_radix(digit, 10).unwrap())
          .unwrap()
          .to_string()
      }); // decimal chars

      // \foo\ bar -> \foo{} bar
      let text = regex_replace_all!(r"(\\[a-zA-Z]+)\\(\s+)", &text, |_, foo, bar| format!("{foo}{{}}{bar}"));
      //} Aaaa\o,  -> Aaaa\o{},
      let text = regex_replace_all!(r"([^{]\\\w)([;,.:%])", &text, |_, foo, bar| format!("{foo}{{}}{bar}"));

      for typ in ["greek", "dings", "punctuation", "symbols", "negatedsymbols", "superscripts", "cmdsuperscripts", "letters", "diacritics"].into_iter() {
        let map = $remap_d->{$type}.map;
        let re = $remap_d->{$type}.re;
        if !re { // Might not be present depending on set
          continue;
        }

        match typ {
          "negatedsymbols" => {
            text = Regex::new(&format!(r"\\not\\({re})"))
              .unwrap()
              .replace_all(&text, |cap: &Captures| {
                map.get(cap.get(1).unwrap().as_str()).unwrap()
              });
          }
          "superscripts" => {
            text = Regex::new(&format!(r"\\textsuperscript\{{({re})\}}"))
              .unwrap()
              .replace_all(&text, |cap: &Captures| {
                map.get(cap.get(1).unwrap().as_str()).unwrap()
              });
          }
          "cmdsuperscripts" => {
            text = Regex::new(&format!(r"\\textsuperscript\{{\\({re})\}}"))
              .unwrap()
              .replace_all(&text, |cap: &Captures| {
                map.get(cap.get(1).unwrap().as_str()).unwrap()
              });
          }
          "dings" => {
            text = regex_replace_all!(r"\\ding\{([2-9AF][0-9A-F])\}", &text, |_, ding| map.get(ding).unwrap());
          }
          "letters" => {
            text = Regex::new(&format!(r"\\({re})(?:\{{\}}|\s+|\b)"))
              .unwrap()
              .replace_all(&text, |cap: &Captures| {
                map.get(cap.get(1).unwrap().as_str()).unwrap()
              });
          }
          "punctuation" | "symbols" | "greek" => {
            text = Regex::new(&format!(r"\\({re})(?: \{{\}}|\s+|\b)"))
              .unwrap()
              .replace_all(&text, |cap: &Captures| {
                map.get(cap.get(1).unwrap().as_str()).unwrap()
              });
          }
          "diacritics" => {

            // Using Unicode INFORMATION SEPARATOR ONE/TWO
            let $bracemap = {"" => "",
                            "{" => "\u{1f}",
                            "}" => "\u{1e}"};

            // Hacky - specially protect {\X} which is a simple protection as in
            // TITLE = {Part {I}}
            // Can't do this using the seperators above as these are stripping around \X
            // later to avoid breaking capitliastion/kerning with spurious introduced/retained
            // braces
            // Using the VLB method from the link below, this is equivalent to:
            // $text =~ s/(?<!\\$re)\{(\X)\}/\x{f}$1\x{e}/g;
            let text = fancy_regex::Regex::new(&format!(r"(?!(?=(?P<a>[\s\S]*))(?P<b>\\{re}(?=\k<a>\z)|(?<=(?=x^|(?&b))[\s\S])))\{{(.+?\b{{g}})\}}"))
            .unwrap()
            .replace_all(&text, |cap: &fancy_regex::Captures| {
              format!("\u{f}{}\u{e}", cap.get(3).unwrap().as_str())
            }); // TODO: check
            //$text =~ s/(?!(?=(?'a'[\s\S]*))(?'b'\\$re(?=\k'a'\z)|(?<=(?=x^|(?&b))[\s\S])))\{(\X)\}/\x{f}$3\x{e}/g;

            // Rename protecting braces so that they are not broken by RE manipulations
            text = Regex::new(&format!(r"(\{{?)\\({re})\s*\{{(\pL\pM*)\}}(\}}?)"))
              .unwrap()
              .replace_all(&text, |cap: &Captures| {
                format!("{}{}{}{}",
                  bracemap.get(cap.get(1).unwrap_or("")).unwrap(),
                  cap.get(3).unwrap_or(""),
                  map.get(cap.get(2).unwrap()).unwrap(),
                  bracemap.get(cap.get(4).unwrap_or("")).unwrap()
                )
              });
            text = regex_replace_all!(r"(\{)(\pL\pM*)(\})",
              &text,
              |_, one, two, three| format!("{}{two}{}", bracemap.get(one).unwrap(), bracemap.get(three).unwrap())
            );

            // Conditional regexp with code-block condition
            // non letter macros for diacritics (e.g. \=) can be followed by any letter
            // but letter diacritic macros (e.g \c) can't (\cS)
            //
            // If the RE for the macro doesn't end with a basic LaTeX macro letter (\=), then
            //   next char can be any letter (\=d)
            // Else if it did end with a normal LaTeX macro letter (\c), then
            //   If this was followed by a space (\c )
            //     Any letter is allowed after the space (\c S)
            //   Else
            //     Only a non basic LaTeX letter is allowed (\c-)
            text = RegexBuilder::new(&format!(r###"\\# slash
              ({re})# the diacritic
              (\s*)# optional space
              (# capture paren
                (?(?{{$1 !~ m:[A-Za-z]$:}})# code block condition (is not a letter?)
                  \pL # yes pattern
                | # no pattern
                  (?(?{{$2}}) # code block condition (space matched earlier after diacritic?)
                    \pL # yes pattern
                  | # no pattern
                    [^A-Za-z]
                  ) # close conditional
                ) # close conditional
                \pM* # optional marks
              ) # capture paren
            "###))
              .ignore_whitespace(true)
              .build()
              .unwrap()
              .replace_all(&text, |cap: &Captures| {
                let one = cap.get(1).unwrap().as_str();
                let three = cap.get(3).unwrap().as_str();
                format!("{}{}", three, map.get(one).unwrap())
              });
          }
          _ => {}
        }
      }

        trace!("String in latex_decode() before brace elimination now -> '{}'", text);

      // Now remove braces around single letters (which the replace above can
      // result in). Things like '{á}' can break kerning/brace protection. We
      // can't do this in the RE above as we can't determine if the braces are
      // wrapping a phrase because this match is on an entire field string. So
      // we can't in one step tell the difference between:
      //
      // author = {Andr\'e}
      // and
      // author = {Andr\'{e}}
      //
      // when this is part of a (much) larger string
      //
      // We don't want to do this if it would result in a broken macro name like with
      // \textupper{é}
      // or
      // \frac{a}{b}
      //
      // This horrible RE is the very clever variable-look-behind implementation from:
      // http://www.drregex.com/2019/02/variable-length-lookbehinds-actually.html
      // Perl 5.30 has limited (<255 chars) VLB but it doesn't work here as it can't be determined
      // that it's <255 chars by the parser
      $text =~ s/(?!(?=(?'a'[\s\S]*))(?'b'\\\pL+(?:\{[^{]+\})*(?=\k'a'\z)|(?<=(?=x^|(?&b))[\s\S])))[{\x{1f}](\X)[}\x{1e}]/$3/g;

      // Put back any brace markers left after doing the brace elimination as
      // we only want to eliminate braces introduced as part of decoding, not
      // explicit braces in the data
      text = regex_replace_all!(r"\u{1f}", &text, "{");
      text = regex_replace_all!(r"\u{1e}", &text, "}");
      text = regex_replace_all!(r"\u{f}", &text, "{");
      text = regex_replace_all!(r"\u{e}", &text, "}");

        trace!("String in latex_decode() now -> '{text}'");

      if ($norm) {
        return Unicode::Normalize::normalize($norm_form, $text);
      }
      else {
        return $text;
      }
  }

  /// Converts UTF-8 to LaTeX
  pub fn latex_encode(text: &str) {
    // Optimisation - if virtual null set was specified, do nothing
    if $set_e == "null" {
      return $text;
    }

    for type in ["greek", "dings", "negatedsymbols", "superscripts", "cmdsuperscripts", "diacritics", "letters", "punctuation", "symbols"] {
      let map = $remap_e->{$type}.map;
      let re = $remap_e->{$type}.re;
      if !re { // Might not be present depending on set
        continue;
      }

      match type {
        "negatedsymbols" => {
          text = Regex::new(&format!(r"({re})"))
            .unwrap()
            .replace_all(&text, |cap: &Captures| {
              format!("{\$\\not\\{}$}", map.get(cap.get(1).unwrap().as_str()).unwrap())
            });
        }
        "superscripts" => {
          text = Regex::new(&format!(r"({re})"))
            .unwrap()
            .replace_all(&text, |cap: &Captures| {
              &format!(r"\textsuperscript{{{}}}", map.get(cap.get(1).unwrap().as_str()).unwrap())
            });
        }
        "cmdsuperscripts" => {
          text = Regex::new(&format!(r"({re})"))
            .unwrap()
            .replace_all(&text, |cap: &Captures| {
              &format!("\\textsuperscript{{\\{}}}", map.get(cap.get(1).unwrap().as_str()).unwrap())
            });
        }
        "dings" => {
          text = Regex::new(&format!(r"({re})"))
            .unwrap()
            .replace_all(&text, |cap: &Captures| {
              &format!(r"\ding{{{}}}", map.get(cap.get(1).unwrap().as_str()).unwrap())
            });
        }
        "letters" => {
          // General macros (excluding special encoding excludes)
          text = Regex::new(&format!(r"({re})"))
          .unwrap()
          .replace_all(&text, |cap: &Captures| {
            let one = cap.get(1).unwrap().as_str();
            if remap_e_raw.contains(one) {
              map.get(one).unwrap()
            } else {
              &format!("\\{}{{}}", map.get(one).unwrap())
            }
          });
        }
        "punctuation" | "symbols" | "greek" => {
          text = Regex::new(&format!(r"({re})"))
            .unwrap()
            .replace_all(&text, |cap: &Captures| {
              &_wrap(cap.get(1).unwrap().as_str(), map, remap_e_raw)
            });
        }
        "diacritics" => {
          // special case such as "i\x{304}" -> '\={\i}' -> "i" needs the dot removing for accents
          text = Regex::new(&format!(r"i({re})"))
          .unwrap()
          .replace_all(&text, |cap: &Captures| {
            &format!(r"\{}{{\i}}", map.get(cap.get(1).unwrap().as_str()).unwrap())
          });

          text = Regex::new(&format!(r"\{{(\pL\pM*)\}}({re})"))
          .unwrap()
          .replace_all(&text, |cap: &Captures| {
            &format!("\\{}{{{}}}", map.get(cap.get(2).unwrap().as_str()).unwrap(), cap.get(1).unwrap().as_str())
          });
          text = Regex::new(&format!(r"(\pL\pM*)({re})"))
          .unwrap()
          .replace_all(&text, |cap: &Captures| {
            &format!("\\{}{{{}}}", map.get(cap.get(2).unwrap().as_str()).unwrap(), cap.get(1).unwrap().as_str())
          });

          text = Regex::new(&format!(r"(\PM)({re})({re})({re})"))
          .unwrap()
          .replace_all(&text, |cap: &Captures| {
            let one = cap.get(1).unwrap().as_str();
            let two = cap.get(2).unwrap().as_str();
            let three = cap.get(3).unwrap().as_str();
            let four = cap.get(4).unwrap().as_str();
            &format!("\\{}{{\\{}{{\\{}{{{}}}}}}}",
              map.get(four).unwrap(), 
              map.get(three).unwrap(),
              map.get(two).unwrap(),
              one
            )
          });
          text = Regex::new(&format!(r"(\PM)({re})({re})"))
          .unwrap()
          .replace_all(&text, |cap: &Captures| {
            let one = cap.get(1).unwrap().as_str();
            let two = cap.get(2).unwrap().as_str();
            let three = cap.get(3).unwrap().as_str();
            &format!("\\{}{{\\{}{{{}}}}}",
              map.get(three).unwrap(),
              map.get(two).unwrap(),
              one
            )
          });
          text = Regex::new(&format!(r"(\PM)({re})"))
          .unwrap()
          .replace_all(&text, |cap: &Captures| {
            let one = cap.get(1).unwrap().as_str();
            let two = cap.get(2).unwrap().as_str();
            &format!("\\{}{{{}}}",
              map.get(two).unwrap(),
              one
            )
          });
        }
        _ => {}
      }
    }

    fn _wrap(s: &str, map: &HashMap<String, String>, remap_e_raw: &HashSet<String>) -> String {
      let map_s = map.get(s).unwrap();
      if regex_is_match!(r"^(?:text|guil)", maps) {
        format!("\\{map_s}{{}}")
      } else if remap_e_raw.contains(s) {
        map_s.into()
      } else {
        format!("{{\$\\{map_s}$}")
      }
    }

    //return $text;
  }
