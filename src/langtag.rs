package Biber::LangTag;
use v5.24;
use strict;
use warnings;
use parent qw(Class::Accessor);
__PACKAGE__->follow_best_practice;

use List::AllUtils qw( first );

my %bcp47parts = ('language'      => 'single',
                  'extlang'       => 'multiple',
                  'script'        => 'single',
                  'region'        => 'single',
                  'variant'       => 'multiple',
                  'extension'     => 'multiple',
                  'privateuse'    => 'multiple',
                  'grandfathered' => 'single');


# Names of simple package accessor attributes for those not created automatically
# by the option scope in the .bcf
__PACKAGE__->mk_accessors(keys %bcp47parts);

=encoding utf-8

=head1 NAME

Biber::LangTag - Biber::LangTag objects

=head2 new

    Object to manipulate BCP47 language tags

=cut

sub new {
  my ($class, $parts) = @_;
  my $self = bless $parts, $class;

  return $self;
}

=head2 dump

    Dump the non-null LangTag object parts

=cut

sub dump {
  my $self = shift;
  my $parts = {};
  foreach my $part (keys %bcp47parts) {
    $parts->{$part} = $self->{$part} if defined($self->{$part});
  }
  return $parts;
}
