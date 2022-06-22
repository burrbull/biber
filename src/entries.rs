//! `Entries` objects
use super::entry::Entry;

pub struct Entries(Hash<String, Entry>);

impl Entries {
  /// Initialize a crate::Entries object
  pub fn new() -> Self {
    Self(Hash::new())
  }

  /// Test for an empty object
  pub fn notnull(&self) {
    !self.0.is_empty()
  }

  /// Boolean values sub to tell if there is an entry
  /// for the passed citation key.
  pub fn entry_exists(&self, citekey: &str) -> bool {
    self.0.contains_key(citekey)
  }

  /// Returns a crate::Entry object for a given
  /// citekey
  pub fn entry(&self, citekey: &str) -> Option<&Entry> {
    self.0.get(citekey)
  }

  /// Returns an array of all crate::Entry objects
  pub fn entries(&self) -> impl Iterator<Item=&Entry> {
    self.0.values()
  }

  /// Deletes all crate::Entry objects
  pub fn del_entries(&mut self) {
    self.0.clear()
  }

  /// Adds a crate::Entry to the crate::Entries object
  pub fn add_entry(&mut self, key: &str, entry: Entry) {
    self.0.insert(key, entry)
  }

  /// Deletes a crate::Entry object for a given
  /// citekey. Only used in tests in order to reset
  /// data before regeneration with different options.
  pub fn del_entry(&mut self, citekey: &str) {
    self.0.remove(citekey)
  }
}
