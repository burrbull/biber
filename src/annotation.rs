//! `Annotation` objects
//!
//! Record an annotation for a scope and citekey

use crate::{OptIter, NestedMap};
use std::collections::{btree_map, hash_map, HashMap, HashSet, BTreeMap};
use once_cell::sync::Lazy;
use std::sync::Mutex;
use unicase::UniCase;

// Static class data
pub(crate) static ANN: Lazy<Mutex<Ann>> = Lazy::new(|| Mutex::new(Ann::new()));

#[derive(Debug)]
pub struct Ann {
  //             key             field            name
  field: HashMap<String, HashMap<String, BTreeMap<String, Annotation>>>,
  //             key             field            name             item
  item: HashMap<String, HashMap<String, BTreeMap<String, BTreeMap<String, Annotation>>>>,
  //             key             field            name             item             part
  part: HashMap<String, HashMap<String, BTreeMap<String, BTreeMap<String, BTreeMap<String, Annotation>>>>>,
  //              key      field                    name
  names: HashMap<String, HashMap<String, HashSet<UniCase<String>>>>,
}

impl Ann {
  fn new() -> Self {
    Self {
      field: HashMap::new(),
      item: HashMap::new(),
      part: HashMap::new(),
      names: HashMap::new(),
    }
  }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Scope {
  Field,
  Item,
  Part,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Annotation {
  value: String,
  /// Record if this annotation is a literal
  literal: bool,
}

impl Annotation {
  pub fn value(&self) -> &String {
    &self.value
  }
  pub fn is_literal(&self) -> char {
    if self.literal {
      '1'
    } else {
      '0'
    }
  }
}

impl Ann {
  fn set_field_annotation(&mut self, key: String, field: String, name: String, value: &str, literal: bool) {
    let elem = Annotation { value: value.into(), literal };

    match self.field.entry(key.clone()) {
        hash_map::Entry::Occupied(mut e) => {
          match e.get_mut().entry(field.clone()) {
            hash_map::Entry::Occupied(mut e) => {
              e.get_mut().insert(name.clone(), elem);
            }
            hash_map::Entry::Vacant(e) => {
              e.insert(BTreeMap::from([(name.clone(), elem)]));
            }
          }
        },
        hash_map::Entry::Vacant(e) => {
            e.insert(HashMap::from([(field.clone(), BTreeMap::from([(name.clone(), elem)]))]));
        }
    }

    // Record all annotation names or a field
    let uniname = UniCase::new(name);
    match self.names.entry(key) {
      hash_map::Entry::Occupied(mut e) => {
        match e.get_mut().entry(field) {
          hash_map::Entry::Occupied(mut e) => {
            e.get_mut().insert(uniname);
          }
          hash_map::Entry::Vacant(e) => {
            e.insert(HashSet::from([uniname]));
          }
        }
      },
      hash_map::Entry::Vacant(e) => {
          e.insert(HashMap::from([(field, HashSet::from([uniname]))]));
      }
    }
  }

  fn set_item_annotation(&mut self, key: String, field: String, name: String, value: &str, literal: bool, count: String) {
    let elem = Annotation { value: value.into(), literal };

    match self.item.entry(key.clone()) {
        hash_map::Entry::Occupied(mut e) => {
          match e.get_mut().entry(field.clone()) {
            hash_map::Entry::Occupied(mut e) => {
              match e.get_mut().entry(name.clone()) {
                btree_map::Entry::Occupied(mut e) => {
                  e.get_mut().insert(count, elem);
                }
                btree_map::Entry::Vacant(e) => {
                  e.insert(BTreeMap::from([(count, elem)]));
                }
              }
            },
            hash_map::Entry::Vacant(e) => {
              e.insert(BTreeMap::from([(name.clone(), BTreeMap::from([(count, elem)]))]));
            }
          }
        },
        hash_map::Entry::Vacant(e) => {
            e.insert(HashMap::from([(field.clone(), BTreeMap::from([(name.clone(), BTreeMap::from([(count, elem)]))]))]));
        }
    }

    // Record all annotation names or a field
    let uniname = UniCase::new(name);
    match self.names.entry(key) {
      hash_map::Entry::Occupied(mut e) => {
        match e.get_mut().entry(field) {
          hash_map::Entry::Occupied(mut e) => {
            e.get_mut().insert(uniname);
          }
          hash_map::Entry::Vacant(e) => {
            e.insert(HashSet::from([uniname]));
          }
        }
      },
      hash_map::Entry::Vacant(e) => {
          e.insert(HashMap::from([(field, HashSet::from([uniname]))]));
      }
    }
  }

  fn set_part_annotation(&mut self, key: String, field: String, name: String, value: &str, literal: bool, count: String, part: String ) {
    let elem = Annotation { value: value.into(), literal };

    match self.part.entry(key.clone()) {
        hash_map::Entry::Occupied(mut e) => {
          match e.get_mut().entry(field.clone()) {
            hash_map::Entry::Occupied(mut e) => {
              match e.get_mut().entry(name.clone()) {
                btree_map::Entry::Occupied(mut e) => {
                  match e.get_mut().entry(count) {
                    btree_map::Entry::Occupied(mut e) => {
                      e.get_mut().insert(part, elem);
                    }
                    btree_map::Entry::Vacant(e) => {
                      e.insert(BTreeMap::from([(part, elem)]));
                    }
                  }
                },
                btree_map::Entry::Vacant(e) => {
                  e.insert(BTreeMap::from([(count, BTreeMap::from([(part, elem)]))]));
                }
              }
            },
            hash_map::Entry::Vacant(e) => {
              e.insert(BTreeMap::from([(name.clone(), BTreeMap::from([(count, BTreeMap::from([(part, elem)]))]))]));
            }
          }
        },
        hash_map::Entry::Vacant(e) => {
            e.insert(HashMap::from([(field.clone(), BTreeMap::from([(name.clone(), BTreeMap::from([(count, BTreeMap::from([(part, elem)]))]))]))]));
        }
    }

    // Record all annotation names or a field
    let uniname = UniCase::new(name);
    match self.names.entry(key) {
      hash_map::Entry::Occupied(mut e) => {
        match e.get_mut().entry(field) {
          hash_map::Entry::Occupied(mut e) => {
            e.get_mut().insert(uniname);
          }
          hash_map::Entry::Vacant(e) => {
            e.insert(HashSet::from([uniname]));
          }
        }
      },
      hash_map::Entry::Vacant(e) => {
          e.insert(HashMap::from([(field, HashSet::from([uniname]))]));
      }
    }
  }

  /// Copy all annotations from one entry to another
  fn copy_annotations(&mut self, sourcekey: &str, targetkey: &str) {
    if let Some(map) = self.field.get(sourcekey).cloned() {
      self.field.insert(targetkey.into(), map);
    }
    if let Some(map) = self.item.get(sourcekey).cloned() {
      self.item.insert(targetkey.into(), map);
    }
    if let Some(map) = self.part.get(sourcekey).cloned() {
      self.part.insert(targetkey.into(), map);
    }
    if let Some(map) = self.names.get(sourcekey).cloned() {
      self.names.insert(targetkey.into(), map);
    }
  }

  /// Retrieve all annotation names for a citekey and field
  fn get_annotation_names(self, key: &str, field: &str) {
    self.names.get2(key, field).iter();
  }

  /// Retrieve all annotations for a scope and citekey
  fn get_annotations(&self, scope: Scope, key: &str, field: &str) -> Vec<&String> {
    match scope {
      Scope::Field => {
        OptIter::new(self.field.get2(key, field).map(|m| m.keys())).collect()
      }
      Scope::Item => {
        OptIter::new(self.item.get2(key, field).map(|m| m.keys())).collect()
      }
      Scope::Part => {
        OptIter::new(self.part.get2(key, field).map(|m| m.keys())).collect()
      }
    }
  }

  /// Retrieve "field" scope annotation for a field. There will only be one.
  fn get_field_annotation(&self, key: &str, field: &str, mut name: &str) -> Option<&Annotation> {
    if name.is_empty() {
      name = "default";
    }
    self.field.get2(key, field).and_then(|m| m.get(name))
  }

  /// Retrieve an specific annotation for a scope, citekey and name
  fn get_item_annotation(&self, key: &str, field: &str, mut name: &str, count: &str) -> Option<&Annotation> {
    if name.is_empty() {
      name = "default";
    }
    self.item.get2(key, field).and_then(|m| m.get(name)).and_then(|m| m.get(count))
  }

  /// Retrieve an specific annotation for a scope, citekey and name
  fn get_part_annotation(&self, key: &str, field: &str, mut name: &str, count: &str, part: &str) -> Option<&Annotation> {
    if name.is_empty() {
      name = "default";
    }
    self.part.get2(key, field).and_then(|m| m.get(name)).and_then(|m| m.get(count)).and_then(|m| m.get(part))
  }

  /// Returns boolean to say if a field is annotated
  fn is_annotated_field(key: &str, field: &str) -> bool {
    let ann = &ANN.lock().unwrap();
    ann.field.contains_key2(key, field)
    || ann.item.contains_key2(key, field)
    || ann.part.contains_key2(key, field)
  }

  /// Retrieve all annotated fields for a particular scope for a key
  fn get_annotated_fields(&self, scope: Scope, key: &str) -> Vec<&String> {
    match scope {
      Scope::Field => OptIter::new(self.field.get(key).map(|a| a.keys())).collect(),
      Scope::Item => OptIter::new(self.item.get(key).map(|a| a.keys())).collect(),
      Scope::Part => OptIter::new(self.part.get(key).map(|a| a.keys())).collect(),
    }
  }

  /// Retrieve the itemcounts for a particular scope, key, field and nam3
  fn get_annotated_items(&self, key: &str, field: &str, mut name: &str) -> impl Iterator<Item=(&String, &Annotation)> {
    if name.is_empty() {
      name = "default";
    }
    OptIter::new(self.item.get2(key, field).and_then(|m| m.get(name)).map(|m| m.iter()))
  }

  /// Retrieve the parts for a particular scope, key, field, name and itemcount
  fn get_annotated_parts(&self, key: &str, field: &str, mut name: &str) -> impl Iterator<Item=(&String, &String, &Annotation)> {
    if name.is_empty() {
      name = "default";
    }
    OptIter::new(self.part.get2(key, field).and_then(|m| m.get(name)).map(|m| m.iter().flat_map(|(k1, f)| f.iter().map(move |(k2, val)| (k1, k2, val)))))
  }
/*
  /// Dump config information (for debugging)
  fn dump() {
    dd($ANN);
  }*/
}