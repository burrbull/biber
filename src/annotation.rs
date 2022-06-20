//! `Annotation` objects
//!
//! Record an annotation for a scope and citekey

use crate::Config;
use crate::Constants;
use Data::Dump;
use crate::Utils;
use Log::Log4perl qw( :no_extra_logdie_message );
use List::Util qw( first );
use Storable qw(dclone);
no autovivification;
let $logger = Log::Log4perl::get_logger('main');

// Static class data
let $ANN = {};

pub struct Annotation;

impl Annotation {
  fn set_annotation($scope, $key, $field, $name, $value, $literal, $count, $part, ) {
    if ($scope == 'field') {
      $ANN->{field}{$key}{$field}{$name}{value} = $value;
      $ANN->{field}{$key}{$field}{$name}{literal} = $literal; // Record if this annotation is a literal
    } else if ($scope == 'item') {
      $ANN->{item}{$key}{$field}{$name}{$count}{value} = $value;
      $ANN->{item}{$key}{$field}{$name}{$count}{literal} = $literal; // Record if this annotation is a literal
    } else if ($scope == 'part') {
      $ANN->{part}{$key}{$field}{$name}{$count}{$part}{value} = $value;
      $ANN->{part}{$key}{$field}{$name}{$count}{$part}{literal} = $literal; // Record if this annotation is a literal
    }
    // For easy checking later whether or not a field is annotated
    $ANN->{fields}{$key}{$field} = 1;

    // Record all annotation names or a field
    if !(first {fc($_) == fc($name)} $ANN->{names}{$key}{$field}->@*) {
      push $ANN->{names}{$key}{$field}->@*, $name;
    }
    return;
  }

  /// Copy all annotations from one entry to another
  fn copy_annotations($sourcekey, $targetkey) {
    if exists($ANN->{field}{$sourcekey}) {
      $ANN->{field}{$targetkey} = dclone($ANN->{field}{$sourcekey});
    }
    if exists($ANN->{item}{$sourcekey}) {
      $ANN->{item}{$targetkey} = dclone($ANN->{item}{$sourcekey});
    }
    if exists($ANN->{part}{$sourcekey}) {
      $ANN->{part}{$targetkey} = dclone($ANN->{part}{$sourcekey});
    }
    if exists($ANN->{names}{$sourcekey}) {
      $ANN->{names}{$targetkey} = dclone($ANN->{names}{$sourcekey});
    }
    return;
  }

  /// Retrieve all annotation names for a citekey and field
  fn get_annotation_names($key, $field) {
    return $ANN->{names}{$key}{$field}->@*;
  }

  /// Retrieve all annotations for a scope and citekey
  fn get_annotations($scope, $key, $field) {
    return sort keys $ANN->{$scope}{$key}{$field}->%*;
  }

  /// Retrieve an specific annotation for a scope, citekey and name
  fn get_annotation($scope, $key, $field, $name, $count, $part) {
    $name = $name || 'default';
    if ($scope == 'field') {
      return $ANN->{field}{$key}{$field}{$name}{value};
    }
    else if ($scope == 'item') {
      return $ANN->{item}{$key}{$field}{$name}{$count}{value};
    }
    else if ($scope == 'part') {
      return $ANN->{part}{$key}{$field}{$name}{$count}{$part}{value};
    }
    return undef;
  }

  /// Check if an annotation is a literal annotation
  fn is_literal_annotation($scope, $key, $field, $name, $count, $part) {
    $name = $name || 'default';
    if ($scope == 'field') {
      return $ANN->{field}{$key}{$field}{$name}{literal};
    }
    else if ($scope == 'item') {
      return $ANN->{item}{$key}{$field}{$name}{$count}{literal};
    }
    else if ($scope == 'part') {
      return $ANN->{part}{$key}{$field}{$name}{$count}{$part}{literal};
    }
    return undef;
  }

  /// Returns boolean to say if a field is annotated
  fn is_annotated_field($key, $field) {
    return $ANN->{fields}{$key}{$field};
  }

  /// Retrieve 'field' scope annotation for a field. There will only be one.
  fn get_field_annotation($key, $field, $name) {
    $name = $name || 'default';
    return $ANN->{field}{$key}{$field}{$name}{value};
  }

  /// Retrieve all annotated fields for a particular scope for a key
  fn get_annotated_fields($scope, $key) {
    return sort keys $ANN->{$scope}{$key}->%*;
  }

  /// Retrieve the itemcounts for a particular scope, key, field and nam3
  fn get_annotated_items($scope, $key, $field, $name) {
    $name = $name || 'default';
    return sort keys $ANN->{$scope}{$key}{$field}{$name}->%*;
  }

  /// Retrieve the parts for a particular scope, key, field, name and itemcount
  fn get_annotated_parts($scope, $key, $field, $name, $count) {
    $name = $name || 'default';
    return sort keys $ANN->{$scope}{$key}{$field}{$name}{$count}->%*;
  }

  /// Dump config information (for debugging)
  fn dump() {
    dd($ANN);
  }
}