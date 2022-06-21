//! `Sections` objects
 
pub struct Sections(Hash<u32, Section>);

impl Sections {
  /// Initialize a crate::Sections object
  fn new() -> Self {
    Self(Hash::new())
  }

  /// Gets the number of crate::Section objects
  fn get_num_sections(self) -> usize {
    self.0.len()
  }

  /// Gets a crate::Section by number from the crate::Sections object
  fn get_section(&self, number: u32) -> Option<Section> {
    self.0.get(number)
  }

  /// Gets an sorted array ref of all crate::Section objects
  fn get_sections(&self) -> Vec<&Section> {
    let mut v: Vec<_> = self.0.values().collect();
    v.sort_by_key(|k| k.number);
    v
  }

  /// Adds a crate::Section to the crate::Sections object
  fn add_section(&mut self, section: Section) {
    let number = section.number;
    self.0.insert(number, section);
  }

  /// Deletes a section
  /// Mainly used in test scripts
  fn delete_section(&mut self, section: &Section) {
    let number = section.number;
    self.remove(number)
  }
}
