//! Handling non-empty strings.

use serde::{de, Deserializer, Deserialize};
use std::fmt::{self, Debug, Display, Formatter};
use std::str::FromStr;
use std::ops::Deref;

/// A non-empty string.
///
/// This type is useful for representing names, IDs, FQNs, etc... In
/// general anything that guarantees to not be an empty String.
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct NeString(String);

impl<'de> Deserialize<'de> for NeString {
  fn deserialize<D>(d: D) -> Result<Self, D::Error>
  where D: Deserializer<'de>
  {
    let s: String = Deserialize::deserialize(d)?;
    NeString::from(s).map_err(|e| de::Error::invalid_value(
      de::Unexpected::Other(e), &"a non-empty string"))
  }
}

impl Deref for NeString {
  type Target = str;

  fn deref(&self) -> &Self::Target {
    &self.0
  }
}

impl Debug for NeString {
  fn fmt(&self, f: &mut Formatter) -> fmt::Result {
    (&self.0 as &Debug).fmt(f)
  }
}

impl Display for NeString {
  fn fmt(&self, f: &mut Formatter) -> fmt::Result {
    (&self.0 as &Display).fmt(f)
  }
}

impl FromStr for NeString {
  type Err = &'static str;

  /// Create an `NeString` from a `&str`.
  ///
  /// # Errors
  ///
  /// See [`from`].
  ///
  /// [`from`]: #method.from
  fn from_str(s: &str) -> Result<Self, Self::Err> {
    NeString::from(s.to_owned())
  }
}

impl NeString {
  /// Create an `NeString` from a `String`.
  ///
  /// # Errors
  ///
  /// - When `s` is empty.
  pub fn from(s: String) -> Result<Self, &'static str> {
    if s.is_empty() {
      Err("string cannot be empty")
    } else {
      Ok(NeString(s))
    }
  }
}

#[cfg(test)]
mod test {
  use super::*;
  use serde_json;

  #[test]
  #[should_panic]
  fn empty_str() {
    let _ = NeString::from_str("").unwrap();
  }

  #[test]
  #[should_panic]
  fn empty_string() {
    let _ = NeString::from(String::from("")).unwrap();
  }

  #[test]
  fn non_empty_string() {
    let nestr = NeString::from_str("hello").unwrap();
    let _: &str = &nestr;
  }

  #[derive(Deserialize)]
  struct Something {
    _name: NeString,
  }

  #[test]
  #[should_panic]
  fn parse_something_empty() {
    let data = r#"{"_name": ""}"#;
    let _: Something = serde_json::from_str(data).unwrap();
  }

  #[test]
  fn parse_something() {
    let data = r#"{"_name": "somename"}"#;
    let _: Something = serde_json::from_str(data).unwrap();
  }

  #[test]
  fn debug_display() {
    let s = NeString::from_str("hello").unwrap();
    println!("{:?}", s);
    println!("{}", s);
  }
}
