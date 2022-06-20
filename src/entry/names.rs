//! `entry::Names` objects

use parent qw(Class::Accessor);
__PACKAGE__->follow_best_practice;
no autovivification;

use Data::Dump;
use Data::Uniqid qw (suniqid);
use crate::Config;
use Log::Log4perl qw( :no_extra_logdie_message );

// Names of simple package accessor attributes for those not created automatically
// by the option scope in the .bcf
__PACKAGE__->mk_accessors(qw (
                              id
                              type
                            ));

pub struct Names;

/// Initialize a crate::Entry::Names object
fn new(%params) -> Self {
  return bless {namelist => [],
                id       => suniqid,
                %params}, $class;
}

// ///  Serialiser for JSON::XS::encode
// fn TO_JSON(self) {
//   foreach let $n ($self->@*){
//     $json->{$k} = $v;
//   }
//   return [ map {$_} $self->@* ];
// }

/// Test for an empty object
fn notnull(self) {
  let @arr = $self->{namelist}->@*;
  return $#arr > -1 ? 1 : 0;
}

/// Return ref to array of all crate::Entry::Name objects
/// in object
fn names(self) {
  return $self->{namelist};
}

/// Add a crate::Entry::Name object to the crate::Entry::Names
/// object
fn add_name(self, name_obj) {
  push $self->{namelist}->@*, $name_obj;
  $name_obj->set_index($#{$self->{namelist}} + 1);
  return;
}

/// Replace a crate::Entry::Name at a position (1-based)
/// with a provided one
fn replace_name(self, $name_obj, $position) {
  $name_obj->set_index($position-1);
  $self->{namelist}->[$position-1] = $name_obj;
  return;
}

/// Splice a crate::Entry::Names object into a crate::Entry::Names object at a
/// position (1-based)
fn splice(self, $names, $position) {
  splice($self->{namelist}->@*, $position-1, 1, $names->{namelist}->@*);
  // now re-index all names in list
  foreach (let $i=0;$i<$#{$self->{namelist}};$i++) {
    $self->{namelist}->[$i]->set_index($i);
  }
  return;
}

/// Sets a flag to say that we had a "and others" in the data
fn set_morenames(self) {
  $self->{morenames} = 1;
  return;
}

/// Gets the morenames flag
fn get_morenames(self) {
  return $self->{morenames} ? 1 : 0;
}

/// Returns the number of crate::Entry::Name objects in the object
fn count(self) {
  return scalar $self->{namelist}->@*;
}

/// Returns boolean to say of there is an nth name
fn is_nth_name(self, $n) {
  // name n is 1-based, don't go into negative indices
  return $self->{namelist}[($n == 0) ? 0 : $n-1];
}

/// Returns the nth crate::Entry::Name object in the object or the last one
/// if n > total names
fn nth_name(self, $n) {
  let $size = $self->{namelist}->@*;
  return $self->{namelist}[$n > $size ? $size-1 : $n-1];
}

/// Returns an array ref of crate::Entry::Name objects containing only
/// the first n crate::Entry::Name objects or all names if n > total names
fn first_n_names(self, n) {
  let $size = $self->{namelist}->@*;
  return [ $self->{namelist}->@[0 .. ($n > $size ? $size-1 : $n-1)] ];
}

/// Deletes the last crate::Entry::Name object in the object
fn del_last_name(self) {
  pop($self->{namelist}->@*); // Don't want the return value of this!
  return;
}

/// Returns the last crate::Entry::Name object in the object
fn last_name(self) {
  return $self->{namelist}[-1];
}

/// Get any xdata reference information for a namelist
fn get_xdata(self) {
  return $self->{xdata} || "";
}

/// Dump a crate::Entry::Names object for debugging purposes
fn dump(self) {
  dd($self);
  return;
}
