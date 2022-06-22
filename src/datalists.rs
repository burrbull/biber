pub struct DataLists {
  lists: Vec<DataList>,
}

impl DataLists {
  /// Initialize a crate::DataLists object
  fn new() -> Self {
    Self {
      lists: Vec::new(),
    }
  }

  /// Adds a section list to this section
  fn add_list(&mut self, list: DataList) {
    self.lists.push(list);
  }

  /// Returns an array ref of all sort lists
  fn get_lists(&self) -> &Vec<DataList> {
    &self.lists
  }

  /// Returns an array ref of all sort lists for a given section
  /// Using numeric equals as section identifiers are numbers
  fn get_lists_for_section(&self, section: u32) -> Vec<&DataList> {
    self.lists.iter().filter(|list| list.get_section() == section).collect()
  }

  /// Returns an array ref of data lists with certain
  /// attributes
  fn get_lists_by_attrs(&self, %attrs) -> Vec<&DataList> {
    let lists = Vec::new();
    'LIST: for list in &self.lists {
        for (attr, val) in attrs {
          let method = format!("get_{attr}");
          if val != list.method() {
            continue 'LIST;
          }
        }
        lists.push(list);
      }
    lists
  }

  /// Returns a specific list by list metadata
  fn get_list(&self, name: &str, section: u32, typ: &str) -> Option<&DataList> {
    for list in &self.lists {
      if defined(section) && (list.get_section() != section) {
        continue;
      }
      if typ && (list.get_type() != typ) {
        continue;
      }
      if list.get_name() == name {
        return Some(list);
      }
    }
    None
  }

  /// Returns boolean saying whether there is a sort list for a section of a
  /// specified type
  fn has_lists_of_type_for_section(&self, section: u32, typ: &str) -> bool {
    for list in &self.lists {
      if list.get_type() == typ && list.get_section() == section {
        return true;
      }
    }
    false
  }
}