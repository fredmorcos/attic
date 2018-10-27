use std::rc::Rc;

trait Ported {
  fn get_name(&self) -> &str;
}

struct Port {
  name: String,
  parent: Rc<Ported>,
}

impl Port {
  fn parent_name(&self) -> &str {
    self.parent.get_name()
  }
}

struct Comp {
  comp_name: String,
}

impl Ported for Comp {
  fn get_name(&self) -> &str {
    &self.comp_name
  }
}

struct Prim {
  prim_name: String,
}

impl Ported for Prim {
  fn get_name(&self) -> &str {
    &self.prim_name
  }
}

fn main() {
  let comp = Rc::new(Comp { comp_name: "Comp1".to_owned() });
  let prim = Rc::new(Prim { prim_name: "Prim1".to_owned() });

  let ports = vec![
    Port { name: "CompPort1".to_owned(), parent: comp.clone() },
    Port { name: "CompPort2".to_owned(), parent: comp.clone() },
    Port { name: "PrimPort1".to_owned(), parent: prim.clone() },
    Port { name: "PrimPort2".to_owned(), parent: prim.clone() }
  ];

  for port in &ports {
    println!("{} {}", port.name, port.parent_name());
  }
}
