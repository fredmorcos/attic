#[macro_use]
extern crate serde_derive;

extern crate serde_json;

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
struct Obj {
  id: String,
  name: String,
}

#[derive(Debug, Deserialize)]
#[serde(tag = "primitive")]
#[serde(deny_unknown_fields)]
enum Prim {
  #[serde(rename = "obj")]
  SomeObj(Obj)
}

use std::fs::File;

fn main() {
  let file = File::open("test.json").unwrap();
  let data: Vec<Prim> = serde_json::from_reader(file).unwrap();
  println!("{:?}", data);
}
