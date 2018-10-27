// -*- flycheck-rust-crate-type: "lib"; -*-

use std::io::{Read, BufReader};
use std::fs::File;
use std::marker::Sized;

use serde;
use serde_json;

pub trait JSON: Sized {
  fn post_load(_: &mut Vec<Self>) -> Result<(), String> {
    Ok(())
  }

  fn from_reader<R: Read>(r: BufReader<R>) -> Result<Vec<Self>, String>
  where
    for<'de> Self: serde::Deserialize<'de>,
  {
    let mut res: Vec<Self> = match serde_json::from_reader(r) {
      Ok(r) => r,
      Err(e) => return Err(format!("Cannot parse JSON: {}", e)),
    };

    match Self::post_load(&mut res) {
      Ok(_) => Ok(res),
      Err(e) => Err(e),
    }
  }

  fn from_file(path: &str) -> Result<Vec<Self>, String>
  where
    for<'de> Self: serde::Deserialize<'de>,
  {
    let buf_cap = 4 * 1024 * 1024;

    match File::open(path) {
      Ok(f) => return Self::from_reader(BufReader::with_capacity(buf_cap, f)),
      Err(e) => return Err(format!("Cannot open file {}: {}", path, e)),
    }
  }
}
