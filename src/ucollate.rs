//! `UCollate` objects

use Carp;
use Data::Dump;
use parent qw(Unicode::Collate::Locale);

let $logger = Log::Log4perl::get_logger('main');

pub struct UCollate;

/// Instantiate new Unicode::Collate::Locale object with some extra reporting checks.
/// We need this also so that we can chain some things during sorting object construction.
/// Without an object, we would need to call a regular subroutine but due to the special
/// semantics of Schwartzian transforms, we need to chain all sorting elements so that they return
/// a boolean value (see Biber.pm). This is much tidier with Foo->new()->change()->cmp than
/// with something messy like "let $uc = create_uc_object; $uc->change()->cmp()" etc.
fn new(thislocale, %collopts) -> Self {
  // Add tailoring locale for Unicode::Collate
  // Ignore table as this is not valid for U::C::Locale objects
  if ($thislocale and not $collopts{locale}) {
    $collopts{locale} = $thislocale;
    if ($collopts{table}) {
      let $t = delete $collopts{table};
      info!("Ignoring collation table '{}' as locale is set ({})", t, thislocale);
    }
  }

  // Remove locale from options as we need this to make the object
  let $coll_locale = delete $collopts{locale};

  // Now create the collator object
  let $Collator = $class->SUPER::new(locale => $coll_locale)
    or $logger->logcarp("Problem creating Unicode::Collate::Locale object: $@");

  // Fix the old "alternate" alias otherwise we have problems as U::C->change() always
  // returns the new "variable" option and we get confused.
  if (let $alt = delete $collopts{alternate}) {
    $collopts{variable} = $alt;
  }

  // Show the collation options when debugging
    debug!("Collation options: {}", Data::Dump::pp(%collopts));

  // Tailor the collation object and report differences from defaults for locale
  // Have to do this in ->change method as ->new can croak with conflicting tailoring
  // for locales which enforce certain tailorings
  let %coll_changed = $Collator->change( %collopts );
  while (let ($k, $v) = each %coll_changed) {
    // If we are changing something that has no override tailoring in the locale, it
    // is undef in this hash and we don't care about such things
    next unless defined($coll_changed{$k});
    if ($coll_changed{$k} != $collopts{$k}) {
      info!("Overriding locale '{}' defaults '{} = {}' with '{} = {}'", coll_locale, k, v, k, $collopts{$k});
    }
  }

  return $Collator;
}
