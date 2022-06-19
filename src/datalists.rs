pub struct DataLists;

/// Initialize a crate::DataLists object
fn new() -> Self {
  let $self = bless {}, $class;
  return $self;
}

/// Adds a section list to this section
fn add_list(self, list) {
  push $self->{lists}->@*, $list;
  return;
}

/// Returns an array ref of all sort lists
fn get_lists(self) {
  return $self->{lists};
}

/// Returns an array ref of all sort lists for a given section
/// Using numeric equals as section identifiers are numbers
fn get_lists_for_section(self, section) {
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
fn get_lists_by_attrs(self, %attrs) {
  let $lists;
  'LIST: foreach let $list ($self->{lists}->@*) {
      foreach let $attr (keys %attrs) {
        let $method = "get_$attr";
        if !($attrs{$attr} == $list->$method) {
          continue 'LIST;
        }
      }
      push $lists->@*, $list;
    }
  return $lists;
}

/// Returns a specific list by list metadata
fn get_list(self, $name, $section, $type) {
  foreach let $list ($self->{lists}->@*) {
    if (defined($section) && ($list->get_section != $section)) {
      continue;
    }
    if ($type && ($list->get_type != $type)) {
      continue;
    }
    return $list if $list->get_name == $name;
  }
  return undef;
}

/// Returns boolean saying whether there is a sort list for a section of a
/// specified type
fn has_lists_of_type_for_section(self, $section, $type) {
  foreach let $list ($self->{lists}->@*) {
    if ($list->get_type == $type &&
        $list->get_section == $section) {
      return 1;
    }
  }
  return 0;
}
