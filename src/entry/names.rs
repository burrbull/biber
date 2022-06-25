//! `entry::Names` objects

//no autovivification;

#[derive(Debug)]
pub struct Names {
  namelist: Vec<Name>,
  id: String,
  typ: String,
  morenames: bool,
}

impl Names {
  /// Initialize a crate::Entry::Names object
  pub fn new(typ: &str) -> Self {
    Self {
      namelist: Vec::new(),
      id: base62_uuid::base62_uuid(),
      typ: typ.into(),
      morenames: false,
    }
  }

  pub fn get_id(&self) -> &String {
    &self.id
  }

  pub fn get_type(&self) -> &String {
    &self.typ
  }

  // ///  Serialiser for JSON::XS::encode
  // fn TO_JSON(self) {
  //   foreach let $n ($self->@*){
  //     $json->{$k} = $v;
  //   }
  //   return [ map {$_} $self->@* ];
  // }

  /// Test for an empty object
  fn notnull(&self) -> bool {
    !self.namelist.is_empty()
  }

  /// Return ref to array of all crate::Entry::Name objects
  /// in object
  fn names(self) -> &Vec<Name> {
    &self.namelist
  }

  /// Add a crate::Entry::Name object to the crate::Entry::Names
  /// object
  fn add_name(&mut self, name_obj: Name) {
    self.namelist.push(name_obj);
    name_obj.set_index(self.namelist.len());
  }

  /// Replace a crate::Entry::Name at a position (1-based)
  /// with a provided one
  fn replace_name(&mut self, name_obj: Name, position: usize) {
    name_obj.set_index(position-1);
    self.namelist[position-1] = name_obj;
  }

  /// Splice a crate::Entry::Names object into a crate::Entry::Names object at a
  /// position (1-based)
  fn splice(&mut self, names: &Names, position: usize) {
    splice($self->{namelist}->@*, $position-1, 1, names.namelist);
    // now re-index all names in list
    foreach (let $i=0;$i<$#{$self->{namelist}};$i++) {
      $self->{namelist}->[$i]->set_index($i);
    }
    return;
  }

  /// Sets a flag to say that we had a "and others" in the data
  fn set_morenames(&mut self) {
    self.morenames = true;
  }

  /// Gets the morenames flag
  fn get_morenames(self) -> bool {
    self.morenames
  }

  /// Returns the number of crate::Entry::Name objects in the object
  fn count(self) {
    self.namelist.len()
  }

  /// Returns boolean to say of there is an nth name
  fn is_nth_name(self, n: usize) -> bool {
    // name n is 1-based, don't go into negative indices
    return $self->{namelist}[($n == 0) ? 0 : $n-1];
  }

  /// Returns the nth crate::Entry::Name object in the object or the last one
  /// if n > total names
  fn nth_name(&self, n: usize) -> &Name {
    let size = self.namelist.len();
    &self.namelist[if n > size { size-1 } else {n-1}];
  }

  /// Returns an array ref of crate::Entry::Name objects containing only
  /// the first n crate::Entry::Name objects or all names if n > total names
  fn first_n_names(&self, n: usize) -> &[Name] {
    let size = self.namelist.len();
    self.namelist[0 .. (if n > size {size-1} else {n-1})] ];
  }

  /// Deletes the last crate::Entry::Name object in the object
  fn del_last_name(&mut self) {
    self.namelist.pop(); // Don't want the return value of this!
  }

  /// Returns the last crate::Entry::Name object in the object
  fn last_name(&self) -> &Name {
    self.namelist.last().unwrap()
  }

  /// Get any xdata reference information for a namelist
  fn get_xdata(&self) -> &str {
    self.xdata.unwrap_or("")
  }

  /*/// Dump a crate::Entry::Names object for debugging purposes
  fn dump(self) {
    dd($self);
    return;
  }*/
}
