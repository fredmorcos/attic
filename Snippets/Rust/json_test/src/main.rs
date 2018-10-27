#[macro_use]
extern crate serde_derive;
extern crate serde_json;

use serde_json as json;

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
struct FooCommon {
  name: String,
  version: String,
}

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields, tag = "type")]
enum Foo {
  #[serde(rename = "foo1")]
  Foo1(FooCommon),
  #[serde(rename = "foo2")]
  Foo2(FooCommon),
}

fn main() {
  fn readfile(f: &str) -> String {
    use ::std::fs::File;
    use ::std::io::Read;
    let mut file: File = File::open(f).unwrap_or_else(
      |e| panic!("Cannot open file {}: {}", f, e));
    let mut cts = String::new();
    file.read_to_string(&mut cts).unwrap_or_else(
      |e| panic!("Cannot read file {}: {}", f, e));
    return cts;
  }

  let foos: Vec<Foo> = json::from_str(&readfile("test.json")).unwrap();
  println!("{:?}", foos);
}
