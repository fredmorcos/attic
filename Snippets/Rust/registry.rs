use serde;
use serde_json;
use std::collections::{HashMap, HashSet as Set};
use std::fmt::{self, Display, Formatter};
use std::hash::Hash;

pub trait Register {
  type ID: Eq + Hash;

  fn id(&self) -> Self::ID;
  fn update(&mut self) {}
  fn validate(&self) -> Result<(), String> {
    Ok(())
  }
}

pub struct Registry<V: Register> {
  vec: Vec<V>,
  map: HashMap<V::ID, usize>,
}

fn empty_seal<T: Register>(_: &mut Registry<T>) {}

impl<V: Register> Registry<V> {
  pub fn new<T: Register>(seal_func: Option<fn(&mut Registry<T>)>) -> Self {
    Self {
      vec: Vec::new(),
      map: HashMap::new(),
    }
  }

  pub fn len(&self) -> usize {
    self.vec.len()
  }

  pub fn seal(&mut self) {
    for elem in &mut self.vec {
      elem.update()
    }
  }

  pub fn push(&mut self, elem: V) -> Result<(), String> {
    elem.validate()?;
    let idx = self.vec.len();
    match self.map.insert(elem.id(), idx) {
      None => Ok(self.vec.push(elem)),
      Some(old) => Err(format!("ID {} is already in registry map", old)),
    }
  }

  pub fn append(&mut self, elems: Vec<V>) -> Result<(), String> {
    for elem in elems {
      self.push(elem)?;
    }
    Ok(())
  }

  pub fn push_from_str<'a>(&mut self, s: &'a str) -> Result<(), String>
    where V: serde::Deserialize<'a>
  {
    self.push(match serde_json::from_str::<V>(s) {
                Ok(res) => res,
                Err(err) => return Err(format!("Push: Cannot parse JSON string: {}", err)),
              })
  }

  pub fn append_from_str<'a>(&mut self, s: &'a str) -> Result<(), String>
    where V: serde::Deserialize<'a>
  {
    self.append(match serde_json::from_str::<Vec<V>>(s) {
                  Ok(res) => res,
                  Err(err) => return Err(format!("Append: Cannot parse JSON string: {}", err)),
                })
  }

  pub fn from_id(&self, id: &V::ID) -> Result<&V, String>
    where V::ID: Display
  {
    match self.map.get(id) {
      None => Err(format!("ID {} not found", id)),
      Some(idx) => {
        match self.vec.get(*idx) {
          None => panic!("Registry::from_id(): ID {} points outside of registry vector", id),
          Some(elem) => Ok(elem),
        }
      },
    }
  }

  pub fn from_index(&self, idx: usize) -> Option<&V> {
    self.vec.get(idx)
  }

  pub fn from_ids(&self, ids: &Vec<&V::ID>) -> Result<Vec<&V>, String>
    where V::ID: Display
  {
    let mut res: Vec<&V> = Vec::with_capacity(ids.len());
    for id in ids {
      res.push(self.from_id(id)?);
    }
    Ok(res)
  }

  pub fn indexes_from_ids(&self, ids: &Vec<&V::ID>) -> Result<Set<usize>, String>
    where V::ID: Display
  {
    let mut res: Set<usize> = Set::with_capacity(ids.len());
    for id in ids {
      match self.map.get(id) {
        None => return Err(format!("ID {} not found", id)),
        Some(idx) => {
          res.insert(*idx);
        },
      }
    }
    Ok(res)
  }
}

impl<V: Register + Display> Display for Registry<V> {
  fn fmt(&self, f: &mut Formatter) -> fmt::Result {
    for e in &self.vec {
      writeln!(f, "  {}", e)?;
    }
    Ok(())
  }
}
