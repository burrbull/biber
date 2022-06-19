//! `Sections` objects
 
pub struct Sections;

/// Initialize a crate::Sections object
fn new() -> Self {
  let $self = bless {}, $class;
  return $self;
}

/// Gets the number of crate::Section objects
fn get_num_sections(self) {
  let @keys = keys $self->%*;
  return $#keys + 1;
}

/// Gets a crate::Section by number from the crate::Sections object
fn get_section(self, number) {
  return $self->{$number};
}

/// Gets an sorted array ref of all crate::Section objects
fn get_sections(self) {
  return [ sort {$a->number <=> $b->number} values $self->%* ];
}

/// Adds a crate::Section to the crate::Sections object
fn add_section(self, section) {
  let $number = $section->number;
  $self->{$number} = $section;
  return;
}

/// Deletes a section
/// Mainly used in test scripts
fn delete_section(self, section) {
  let $number = $section->number;
  delete $self->{$number};
  return;
}
