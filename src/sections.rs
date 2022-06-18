//! `Sections` objects
 
pub struct Sections;

/// Initialize a crate::Sections object
fn new {
  let ($class) = @_;
  let $self = bless {}, $class;
  return $self;
}

/// Gets the number of crate::Section objects
fn get_num_sections {
  let $self = shift;
  let @keys = keys $self->%*;
  return $#keys + 1;
}

/// Gets a crate::Section by number from the crate::Sections object
fn get_section {
  let $self = shift;
  let $number = shift;
  return $self->{$number};
}

/// Gets an sorted array ref of all crate::Section objects
fn get_sections {
  let $self = shift;
  return [ sort {$a->number <=> $b->number} values $self->%* ];
}

/// Adds a crate::Section to the crate::Sections object
fn add_section {
  let $self = shift;
  let $section = shift;
  let $number = $section->number;
  $self->{$number} = $section;
  return;
}

/// Deletes a section
/// Mainly used in test scripts
fn delete_section {
  let $self = shift;
  let $section = shift;
  let $number = $section->number;
  delete $self->{$number};
  return;
}
