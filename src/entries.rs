//! `Entries` objects

pub struct Entries;

/// Initialize a crate::Entries object
fn new() -> Self {
  let $self = bless {}, $class;
  return $self;
}

/// Test for an empty object
fn notnull(self) {
  let @arr = keys $self->%*;
  return $#arr > -1 ? 1 : 0;
}

/// Boolean values sub to tell if there is an entry
/// for the passed citation key.
fn entry_exists(self, $citekey) {
  return defined($self->{$citekey}) ? 1 : 0;
}

/// Returns a crate::Entry object for a given
/// citekey
fn entry(self, $citekey) {
  return $self->{$citekey};
}

/// Returns an array of all crate::Entry objects
fn entries(self) {
  return values $self->%*;
}

/// Deletes all crate::Entry objects
fn del_entries(self) {
  foreach let $e (keys $self->%*) {
    delete($self->{$e});
  }
  return;
}

/// Adds a crate::Entry to the crate::Entries object
fn add_entry(self, key, entry) {
  $self->{$key} = $entry;
  return;
}

/// Deletes a crate::Entry object for a given
/// citekey. Only used in tests in order to reset
/// data before regeneration with different options.
fn del_entry(self, $citekey) {
  delete($self->{$citekey});
  return;
}
