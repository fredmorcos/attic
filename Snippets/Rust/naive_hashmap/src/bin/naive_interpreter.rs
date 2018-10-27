extern crate naive_hashmap;

use std::io;
use std::io::prelude::*;

fn main() {
  let mut hash_map = naive_hashmap::HashMap::default();
  let n = io::stdin();

  for line in n.lock().lines() {
    if let Ok(line) = line {
      let mut cmd = line.split_whitespace();

      match cmd.next() {
        Some("LOOKUP") => if let Some(key) = cmd.next() {
          let _ = hash_map.get(key);
        } else {
          continue;
        }
        Some("INSERT") => if let Some(key) = cmd.next() {
          if let Some(val) = cmd.next() {
            let _ = hash_map.insert(key.to_string(), val.to_string());
          } else {
            continue;
          }
        }
        _ => continue,
      }
    } else {
      break;
    }
  }
}
