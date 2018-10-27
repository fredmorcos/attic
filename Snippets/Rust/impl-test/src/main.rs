use std::rc::Rc;

struct Port<T> {
  name: String,
  parent: Rc<T>,
}

struct Comp {
  comp_name: String,
}

struct Prim {
  prim_name: String,
}

// impl Port<T> {
//   fn parent_name(&self) -> &str {
//     self.parent.name()
//   }
// }

impl Port<Comp> {
  fn parent_name(&self) -> &str {
    &self.parent.comp_name
  }
}

impl Port<Prim> {
  fn parent_name(&self) -> &str {
    &self.parent.prim_name
  }
}

fn main() {
  use ::std::iter::FromIterator;

  let len1 = 5;
  let len2 = 6;
  let range = (len1, len1 + len2);
  for i in range.0 .. range.1 {
    println!("i = {}", i);
  }

  println!("-----------");
  let v = Vec::from_iter(range.0 .. range.1);
  for i in v {
    println!("i = {}", i);
  }

  println!("-----------");
  let v = vec![0,1,2,3,4,5];

  for i in 0 .. v.len() {
    println!("i = {} -> {}", i, v[i]);
  }

  let comp = Rc::new(Comp { comp_name: "Comp1".to_owned() });
  let prim = Rc::new(Prim { prim_name: "Prim1".to_owned() });
  let comp_ports = vec![
    Port { name: "CompPort1".to_owned(), parent: comp.clone() },
    Port { name: "CompPort2".to_owned(), parent: comp.clone() },
  ];
  let prim_ports = vec![
    Port { name: "PrimPort1".to_owned(), parent: prim.clone() },
    Port { name: "PrimPort2".to_owned(), parent: prim.clone() }
  ];

  for port in &comp_ports {
    println!("{} {}", port.name, port.parent_name());
  }

  for port in &prim_ports {
    println!("{} {}", port.name, port.parent_name());
  }
}
