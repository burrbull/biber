//! `LangTag` objects

use parent qw(Class::Accessor);
__PACKAGE__->follow_best_practice;

use List::AllUtils qw( first );

let %bcp47parts = ('language'      => 'single',
                  'extlang'       => 'multiple',
                  'script'        => 'single',
                  'region'        => 'single',
                  'variant'       => 'multiple',
                  'extension'     => 'multiple',
                  'privateuse'    => 'multiple',
                  'grandfathered' => 'single');


// Names of simple package accessor attributes for those not created automatically
// by the option scope in the .bcf
__PACKAGE__->mk_accessors(keys %bcp47parts);

/// Object to manipulate BCP47 language tags
pub struct LangTag;

fn new {
  let ($class, $parts) = @_;
  let $self = bless $parts, $class;

  return $self;
}

/// Dump the non-null LangTag object parts
fn dump {
  let $self = shift;
  let $parts = {};
  foreach let $part (keys %bcp47parts) {
    $parts->{$part} = $self->{$part} if defined($self->{$part});
  }
  return $parts;
}
