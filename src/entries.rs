package Biber::Entries;
use v5.24;
use strict;
use warnings;

=encoding utf-8

=head1 NAME

Biber::Entries - Biber::Entries objects

=head2 new

    Initialize a Biber::Entries object

=cut

sub new {
  my ($class) = @_;
  my $self = bless {}, $class;
  return $self;
}

=head2 notnull

    Test for an empty object

=cut

sub notnull {
  my $self = shift;
  my @arr = keys $self->%*;
  return $#arr > -1 ? 1 : 0;
}

=head2 entry_exists

    Boolean values sub to tell if there is an entry
    for the passed citation key.

=cut

sub entry_exists {
  my ($self, $citekey) = @_;
  return defined($self->{$citekey}) ? 1 : 0;
}

=head2 entry

    Returns a Biber::Entry object for a given
    citekey

=cut

sub entry {
  my ($self, $citekey) = @_;
  return $self->{$citekey};
}

=head2 entries

    Returns an array of all Biber::Entry objects

=cut

sub entries {
  my $self = shift;
  return values $self->%*;
}


=head2 del_entries

  Deletes all Biber::Entry objects

=cut

sub del_entries {
  my $self = shift;
  foreach my $e (keys $self->%*) {
    delete($self->{$e});
  }
  return;
}


=head2 add_entry

    Adds a Biber::Entry to the Biber::Entries object

=cut

sub add_entry {
  my $self = shift;
  my ($key, $entry) = @_;
  $self->{$key} = $entry;
  return;
}


=head2 del_entry

  Deletes a Biber::Entry object for a given
  citekey. Only used in tests in order to reset
  data before regeneration with different options.

=cut

sub del_entry {
  my ($self, $citekey) = @_;
  delete($self->{$citekey});
  return;
}
