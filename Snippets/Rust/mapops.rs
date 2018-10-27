// -*- flycheck-rust-crate-type: "lib"; -*-

use std::fmt::Display;
use std::hash::Hash;
use std::collections::HashMap;

pub trait ID {
  fn get_id(&self) -> &String;
}

pub fn make_map<V: ID>(vec: &Vec<V>) -> Result<HashMap<&String, &V>, String> {
  let mut map = HashMap::with_capacity(vec.len());

  for v in vec {
    match map.insert(v.get_id(), v) {
      None => {}
      Some(_) => return Err(format!("Duplicate ID {} in map", v.get_id())),
    }
  }

  Ok(map)
}

pub fn intersection<'a, K: Display + Hash + Eq, V>(
  m1: &HashMap<&'a K, &V>,
  m2: &HashMap<&K, &'a V>,
) -> Result<HashMap<&'a K, &'a V>, String> {
  let mut res = HashMap::new();

  for k in m1.keys() {
    match m2.get(k) {
      None => {}
      Some(v) => {
        match res.insert(*k, *v) {
          None => {}
          Some(_) => {
            return Err(
              format!("Fatal: Duplicate key {} in intersection list", k),
            )
          }
        }
      }
    }
  }

  Ok(res)
}

pub fn difference<'a, K: Display + Hash + Eq, V>(
  m1: &HashMap<&'a K, &'a V>,
  m2: &HashMap<&K, &V>,
) -> Result<HashMap<&'a K, &'a V>, String> {
  let mut res = HashMap::new();

  for k in m1.keys() {
    match m2.get(k) {
      Some(_) => {}
      None => {
        match res.insert(*k, *m1.get(k).unwrap()) {
          None => {}
          Some(_) => {
            return Err(
              format!("Fatal: Duplicate key {} in intersection list", k),
            )
          }
        }
      }
    }
  }

  Ok(res)
}

#[cfg(test)]
mod test {
  use super::*;

  fn make_map(v: &Vec<(String, u8)>) -> HashMap<&String, &u8> {
    let mut m = HashMap::new();

    for e in v {
      m.insert(&e.0, &e.1);
    }

    m
  }

  fn test_data_1() -> Vec<(String, u8)> {
    vec![
      (String::from("one"), 1),
      (String::from("two"), 2),
      (String::from("three"), 3),
    ]
  }

  fn test_data_2() -> Vec<(String, u8)> {
    vec![
      (String::from("four"), 4),
      (String::from("five"), 5),
      (String::from("six"), 6),
    ]
  }

  fn test_data_3() -> Vec<(String, u8)> {
    vec![
      (String::from("one"), 1),
      (String::from("three"), 2),
      (String::from("four"), 4),
    ]
  }

  #[test]
  fn test_difference_empty() {
    let v1 = test_data_1();
    let v2 = test_data_1();
    let m1 = make_map(&v1);
    let m2 = make_map(&v2);
    let m3 = difference(&m1, &m2).unwrap();
    assert_eq!(m3.len(), 0);
  }

  #[test]
  fn test_difference_full() {
    let v1 = test_data_1();
    let v2 = test_data_2();
    let m1 = make_map(&v1);
    let m2 = make_map(&v2);
    let m3 = difference(&m1, &m2).unwrap();
    assert_eq!(m3, m1);
  }

  #[test]
  fn test_difference_some() {
    let v1 = test_data_1();
    let v2 = test_data_3();
    let m1 = make_map(&v1);
    let m2 = make_map(&v2);
    let m3 = difference(&m1, &m2).unwrap();
    assert_eq!(m3.len(), 1);
  }

  #[test]
  fn test_intersection_empty() {
    let v1 = test_data_1();
    let v2 = test_data_2();
    let m1 = make_map(&v1);
    let m2 = make_map(&v2);
    let m3 = intersection(&m1, &m2).unwrap();
    assert_eq!(m3.len(), 0);
  }

  #[test]
  fn test_intersection_full() {
    let v1 = test_data_1();
    let v2 = test_data_1();
    let m1 = make_map(&v1);
    let m2 = make_map(&v2);
    let m3 = intersection(&m1, &m2).unwrap();
    assert_eq!(m1, m3);
    assert_eq!(m2, m3);
  }

  #[test]
  fn test_intersection_some() {
    let v1 = test_data_1();
    let v2 = test_data_3();
    let m1 = make_map(&v1);
    let m2 = make_map(&v2);
    let m3 = intersection(&m1, &m2).unwrap();
    assert_eq!(m3.len(), 2);
  }
}
