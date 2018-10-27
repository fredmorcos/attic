//! Handling non-empty vectors.

use serde::{de, Deserializer, Deserialize};
use std::fmt::{self, Debug, Formatter};
use std::ops::Deref;
use std::vec::IntoIter;

/// A non-empty vector.
#[derive(Clone, PartialEq, Eq)]
pub struct NeVec<T>(Vec<T>);

impl<'de, T: Deserialize<'de>> Deserialize<'de> for NeVec<T> {
  fn deserialize<D>(d: D) -> Result<Self, D::Error>
  where D: Deserializer<'de>
  {
    let v: Vec<T> = Deserialize::deserialize(d)?;
    NeVec::from(v).map_err(|e| de::Error::invalid_value(
      de::Unexpected::Other(e), &"a non-empty vector"))
  }
}

impl<T> Deref for NeVec<T> {
  type Target = [T];

  fn deref(&self) -> &Self::Target {
    &self.0
  }
}

impl<T: Debug> Debug for NeVec<T> {
  fn fmt(&self, f: &mut Formatter) -> fmt::Result {
    self.0.fmt(f)
  }
}

impl<T> IntoIterator for NeVec<T> {
  type Item = T;
  type IntoIter = IntoIter<T>;

  fn into_iter(self) -> Self::IntoIter {
    self.0.into_iter()
  }
}

impl<T> NeVec<T> {
  /// Create an `NeVec<T>` from a `Vec<T>`.
  ///
  /// # Errors
  ///
  /// - When `v` is empty.
  pub fn from(v: Vec<T>) -> Result<Self, &'static str> {
    if v.is_empty() {
      Err("vector cannot be empty")
    } else {
      Ok(NeVec(v))
    }
  }
}

#[cfg(test)]
mod test {
  use super::*;
  use serde_json;

  #[test]
  #[should_panic]
  fn empty_vec() {
    let _: NeVec<usize> = NeVec::from(vec![]).unwrap();
  }

  #[test]
  fn non_empty_vec() {
    let nevec = NeVec::from(vec![1,2,3]).unwrap();
    let _: &[usize] = &nevec;
  }

  #[derive(Deserialize)]
  struct Something {
    _name: NeVec<usize>,
  }

  #[test]
  #[should_panic]
  fn parse_something_empty() {
    let data = r#"{"_name": []}"#;
    let _: Something = serde_json::from_str(data).unwrap();
  }

  #[test]
  fn parse_something() {
    let data = r#"{"_name": [1,2,3]}"#;
    let _: Something = serde_json::from_str(data).unwrap();
  }

  #[test]
  fn debug() {
    let s = NeVec::from(vec![1,2,3]).unwrap();
    println!("{:?}", s);
  }
}
