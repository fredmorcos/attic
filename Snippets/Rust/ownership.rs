use std::process;
use std::collections::HashMap;

#[derive(Debug)]
struct Band {
  a: isize,
  id: String,
}

type BandMap<'a> = HashMap<&'a String, &'a Band>;

impl Band {
  fn new(id: String) -> Self {
    Self { a: 0, id: id }
  }

  fn from_json() -> Vec<Band> {
    vec![
      Band::new(String::from("Band A")),
      Band::new(String::from("Band B")),
    ]
  }

  fn make_map(v: &Vec<Band>) -> Result<BandMap, String> {
    let mut m: BandMap = HashMap::with_capacity(v.len());

    for b in v {
      match m.insert(&b.id, b) {
        None => {}
        Some(_) => return Err(format!("Duplicate key: {}", b.id)),
      }
    }

    Ok(m)
  }

  // fn bad_from_json<'a>() -> Result<(BandMap<'a>, Vec<Band>), String> {
  //   let v = vec![
  //     Band::new(String::from("Band A")),
  //     Band::new(String::from("Band B")),
  //   ];

  //   let mut m: BandMap = HashMap::with_capacity(v.len());

  //   for b in &v {
  //     match m.insert(&b.id, b) {
  //       None => {}
  //       Some(_) => return Err(format!("Duplicate key: {}", b.id)),
  //     }
  //   }

  //   Ok((m, v))
  // }

  // fn bad_single_entry<'a>() -> Result<(BandMap<'a>, Vec<Band>), String> {
  //   let v = Band::from_json();
  //   let m = match Band::make_map(&v) {
  //     Ok(x) => x,
  //     Err(e) => return Err(format!("Cannot make map: {}", e)),
  //   };

  //   Ok((m, v))
  // }
}

fn main() {
  let v = Band::from_json();
  let m = match Band::make_map(&v) {
    Ok(x) => x,
    Err(e) => {
      println!("Error: {}", e);
      process::exit(1)
    }
  };

  println!("{:?}", v);
  println!("{:?}", m);
}
