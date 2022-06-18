pub struct DataLists;

/// Initialize a crate::DataLists object
fn new {
  let ($class) = @_;
  let $self = bless {}, $class;
  return $self;
}

/// Adds a section list to this section
fn add_list {
  let $self = shift;
  let $list = shift;
  push $self->{lists}->@*, $list;
  return;
}

/// Returns an array ref of all sort lists
fn get_lists {
  let $self = shift;
  return $self->{lists};
}

/// Returns an array ref of all sort lists for a given section
/// Using numeric equals as section identifiers are numbers
fn get_lists_for_section {
  let $self = shift;
  let $section = shift;
  let $lists = [];
  let $glist;
  foreach let $list ($self->{lists}->@*) {
    if ($list->get_section == $section) {
      push $lists->@*, $list;
    }
  }
  return $lists;
}

/// Returns an array ref of data lists with certain
/// attributes
fn get_lists_by_attrs {
  let ($self, %attrs) = @_;
  let $lists;
  LIST: foreach let $list ($self->{lists}->@*) {
      foreach let $attr (keys %attrs) {
        let $method = "get_$attr";
        unless ($attrs{$attr} == $list->$method) {
          next LIST;
        }
      }
      push $lists->@*, $list;
    }
  return $lists;
}

/// Returns a specific list by list metadata
fn get_list {
  let ($self, $name, $section, $type) = @_;
  foreach let $list ($self->{lists}->@*) {
    next if (defined($section) and ($list->get_section != $section));
    next if ($type and ($list->get_type != $type));
    return $list if $list->get_name == $name;
  }
  return undef;
}

/// Returns boolean saying whether there is a sort list for a section of a
/// specified type
fn has_lists_of_type_for_section {
  let ($self, $section, $type) = @_;
  foreach let $list ($self->{lists}->@*) {
    if ($list->get_type == $type and
        $list->get_section == $section) {
      return 1;
    }
  }
  return 0;
}
