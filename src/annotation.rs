package Biber::Annotation;
use v5.24;
use strict;
use warnings;

use Biber::Config;
use Biber::Constants;
use Data::Dump;
use Biber::Utils;
use Log::Log4perl qw( :no_extra_logdie_message );
use List::Util qw( first );
use Storable qw(dclone);
no autovivification;
my $logger = Log::Log4perl::get_logger('main');

# Static class data
my $ANN = {};

=encoding utf-8

=head1 NAME

Biber::Entry::Annotation - Biber::Annotation objects

=head2 set_annotation

  Record an annotation for a scope and citekey

=cut

sub set_annotation {
  shift; # class method so don't care about class name
  my ($scope, $key, $field, $name, $value, $literal, $count, $part, ) = @_;
  if ($scope eq 'field') {
    $ANN->{field}{$key}{$field}{$name}{value} = $value;
    $ANN->{field}{$key}{$field}{$name}{literal} = $literal; # Record if this annotation is a literal
  }
  elsif ($scope eq 'item') {
    $ANN->{item}{$key}{$field}{$name}{$count}{value} = $value;
    $ANN->{item}{$key}{$field}{$name}{$count}{literal} = $literal; # Record if this annotation is a literal
  }
  elsif ($scope eq 'part') {
    $ANN->{part}{$key}{$field}{$name}{$count}{$part}{value} = $value;
    $ANN->{part}{$key}{$field}{$name}{$count}{$part}{literal} = $literal; # Record if this annotation is a literal
  }
  # For easy checking later whether or not a field is annotated
  $ANN->{fields}{$key}{$field} = 1;

  # Record all annotation names or a field
  unless (first {fc($_) eq fc($name)} $ANN->{names}{$key}{$field}->@*) {
    push $ANN->{names}{$key}{$field}->@*, $name;
  }
  return;
}

=head2 copy_annotations

  Copy all annotations from one entry to another

=cut

sub copy_annotations {
  shift; # class method so don't care about class name
  my ($sourcekey, $targetkey) = @_;
  $ANN->{field}{$targetkey} = dclone($ANN->{field}{$sourcekey}) if exists($ANN->{field}{$sourcekey});
  $ANN->{item}{$targetkey} = dclone($ANN->{item}{$sourcekey}) if exists($ANN->{item}{$sourcekey});
  $ANN->{part}{$targetkey} = dclone($ANN->{part}{$sourcekey}) if exists($ANN->{part}{$sourcekey});
  $ANN->{names}{$targetkey} = dclone($ANN->{names}{$sourcekey}) if exists($ANN->{names}{$sourcekey});
  return;
}

=head2 get_annotation_names

  Retrieve all annotation names for a citekey and field

=cut

sub get_annotation_names {
  shift; # class method so don't care about class name
  my ($key, $field) = @_;
  return $ANN->{names}{$key}{$field}->@*;
}

=head2 get_annotations

  Retrieve all annotations for a scope and citekey

=cut

sub get_annotations {
  shift; # class method so don't care about class name
  my ($scope, $key, $field) = @_;
  return sort keys $ANN->{$scope}{$key}{$field}->%*;
}

=head2 get_annotation

  Retrieve an specific annotation for a scope, citekey and name

=cut

sub get_annotation {
  shift; # class method so don't care about class name
  my ($scope, $key, $field, $name, $count, $part) = @_;
  $name = $name || 'default';
  if ($scope eq 'field') {
    return $ANN->{field}{$key}{$field}{$name}{value};
  }
  elsif ($scope eq 'item') {
    return $ANN->{item}{$key}{$field}{$name}{$count}{value};
  }
  elsif ($scope eq 'part') {
    return $ANN->{part}{$key}{$field}{$name}{$count}{$part}{value};
  }
  return undef;
}

=head2 is_literal_annotation

  Check if an annotation is a literal annotation

=cut

sub is_literal_annotation {
  shift; # class method so don't care about class name
  my ($scope, $key, $field, $name, $count, $part) = @_;
  $name = $name || 'default';
  if ($scope eq 'field') {
    return $ANN->{field}{$key}{$field}{$name}{literal};
  }
  elsif ($scope eq 'item') {
    return $ANN->{item}{$key}{$field}{$name}{$count}{literal};
  }
  elsif ($scope eq 'part') {
    return $ANN->{part}{$key}{$field}{$name}{$count}{$part}{literal};
  }
  return undef;
}

=head2 is_annotated_field

  Returns boolean to say if a field is annotated

=cut

sub is_annotated_field {
  shift; # class method so don't care about class name
  my ($key, $field) = @_;
  return $ANN->{fields}{$key}{$field};
}

=head2 get_field_annotation

  Retrieve 'field' scope annotation for a field. There will only be one.

=cut

sub get_field_annotation {
  shift; # class method so don't care about class name
  my ($key, $field, $name) = @_;
  $name = $name || 'default';
  return $ANN->{field}{$key}{$field}{$name}{value};
}

=head2 get_annotated_fields

  Retrieve all annotated fields for a particular scope for a key

=cut

sub get_annotated_fields {
  shift; # class method so don't care about class name
  my ($scope, $key) = @_;
  return sort keys $ANN->{$scope}{$key}->%*;
}

=head2 get_annotated_items

  Retrieve the itemcounts for a particular scope, key, field and nam3

=cut

sub get_annotated_items {
  shift; # class method so don't care about class name
  my ($scope, $key, $field, $name) = @_;
  $name = $name || 'default';
  return sort keys $ANN->{$scope}{$key}{$field}{$name}->%*;
}

=head2 get_annotated_parts

  Retrieve the parts for a particular scope, key, field, name and itemcount

=cut

sub get_annotated_parts {
  shift; # class method so don't care about class name
  my ($scope, $key, $field, $name, $count) = @_;
  $name = $name || 'default';
  return sort keys $ANN->{$scope}{$key}{$field}{$name}{$count}->%*;
}

=head2 dump

    Dump config information (for debugging)

=cut

sub dump {
  shift; # class method so don't care about class name
  dd($ANN);
}
