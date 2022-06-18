//! `Entries` objects

pub struct Entries;

/// Initialize a crate::Entries object
fn new {
  let ($class) = @_;
  let $self = bless {}, $class;
  return $self;
}

/// Test for an empty object
fn notnull {
  let $self = shift;
  let @arr = keys $self->%*;
  return $#arr > -1 ? 1 : 0;
}

/// Boolean values sub to tell if there is an entry
/// for the passed citation key.
fn entry_exists {
  let ($self, $citekey) = @_;
  return defined($self->{$citekey}) ? 1 : 0;
}

/// Returns a crate::Entry object for a given
/// citekey
fn entry {
  let ($self, $citekey) = @_;
  return $self->{$citekey};
}

/// Returns an array of all crate::Entry objects
fn entries {
  let $self = shift;
  return values $self->%*;
}

/// Deletes all crate::Entry objects
fn del_entries {
  let $self = shift;
  foreach let $e (keys $self->%*) {
    delete($self->{$e});
  }
  return;
}

/// Adds a crate::Entry to the crate::Entries object
fn add_entry {
  let $self = shift;
  let ($key, $entry) = @_;
  $self->{$key} = $entry;
  return;
}

/// Deletes a crate::Entry object for a given
/// citekey. Only used in tests in order to reset
/// data before regeneration with different options.
fn del_entry {
  let ($self, $citekey) = @_;
  delete($self->{$citekey});
  return;
}
