use std::fmt::{self, Display, Formatter};

struct Port<'a> {
  name: &'a str,
}

impl<'a> Display for Port<'a> {
  fn fmt(&self, f: &mut Formatter) -> fmt::Result {
    write!(f, "Port {}", self.name)
  }
}

enum Prim<'a> {
  Foo {
    in_port: Port<'a>,
    out_port: Port<'a>,
  },
  Bar {
    in_port: Port<'a>,
    out_ports: Vec<Port<'a>>,
  },
  Baz {
    in_ports: Vec<Port<'a>>,
    out_ports: Vec<Port<'a>>,
  }
}

impl<'a> Display for Prim<'a> {
  fn fmt(&self, f: &mut Formatter) -> fmt::Result {
    use self::Prim::*;

    match *self {
      Foo { .. } => write!(f, "Foo"),
      Bar { .. } => write!(f, "Bar"),
      Baz { .. } => write!(f, "Baz"),
    }
  }
}

struct PortIter<'a> {
  prim: &'a Prim<'a>,
  count: usize,
}

impl<'a> Prim<'a> {
  pub fn iterate_mut<'b, F>(&'b mut self, mut f: F) -> ()
    where F: FnMut(&'b mut Port<'a>) -> ()
  {
    use self::Prim::*;

    match *self {
      Foo { ref mut in_port, ref mut out_port } => {
        f(in_port);
        f(out_port);
      },
      Bar { ref mut in_port, ref mut out_ports } => {
        f(in_port);
        for port in out_ports.iter_mut() {
          f(port);
        }
      },
      Baz { ref mut in_ports, ref mut out_ports } => {
        for port in in_ports.iter_mut() {
          f(port);
        }

        for port in out_ports.iter_mut() {
          f(port);
        }
      }
    }
  }
}

// struct PortIterMut<'a> {
//   prim: &'a mut Prim<'a>,
//   count: usize,
// }

// struct PortIterMut<'a: 'b, 'b> {
//   single_0: Option<&'b mut Port<'a>>,
//   single_1: Option<&'b mut Port<'a>>,
//   multi_0: Option<std::slice::IterMut<'b, Port<'a>>>,
//   multi_1: Option<std::slice::IterMut<'b, Port<'a>>>,
// }

impl<'a> IntoIterator for &'a Prim<'a> {
  type Item = &'a Port<'a>;
  type IntoIter = PortIter<'a>;

  fn into_iter(self) -> PortIter<'a> {
    PortIter { prim: self, count: 0 }
  }
}

// impl<'a> IntoIterator for &'a mut Prim<'a> {
//   type Item = &'a mut Port<'a>;
//   type IntoIter = PortIterMut<'a>;

//   fn into_iter(mut self) -> PortIterMut<'a> {
//     PortIterMut { prim: self, count: 0 }
//   }
// }

// impl<'a, 'b> IntoIterator for &'b mut Prim<'a> {
//   type Item = &'b mut Port<'a>;
//   type IntoIter = PortIterMut<'a, 'b>;

//   fn into_iter(self) -> PortIterMut<'a, 'b> {
//     use self::Prim::*;
//     match self {
//       &mut Foo { ref mut in_port, ref mut out_port } => {
//         PortIterMut {
//             single_0: Some(in_port),
//             single_1: Some(out_port),
//             multi_0: None,
//             multi_1: None,
//         }
//       },
//       &mut Bar { ref mut in_port, ref mut out_ports } => {
//         PortIterMut {
//             single_0: Some(in_port),
//             single_1: None,
//             multi_0: Some(out_ports.into_iter()),
//             multi_1: None,
//         }
//       },
//       &mut Baz { ref mut in_ports, ref mut out_ports } => {
//         PortIterMut {
//             single_0: None,
//             single_1: None,
//             multi_0: Some(in_ports.into_iter()),
//             multi_1: Some(out_ports.into_iter()),
//         }
//       }
//     }
//   }
// }

impl<'a> Iterator for PortIter<'a> {
  type Item = &'a Port<'a>;

  fn next(&mut self) -> Option<&'a Port<'a>> {
    use self::Prim::*;

    let idx = self.count;
    self.count += 1;

    match self.prim {
      &Foo { ref in_port, ref out_port } => {
        if idx == 0 {
          Some(in_port)
        } else if idx == 1 {
          Some(out_port)
        } else {
          None
        }
      },
      &Bar { ref in_port, ref out_ports } => {
        if idx == 0 {
          Some(in_port)
        } else if idx - 1 < out_ports.len() {
          Some(&out_ports[idx - 1])
        } else {
          None
        }
      },
      &Baz { ref in_ports, ref out_ports } => {
        if idx < in_ports.len() {
          Some(&in_ports[idx])
        } else if idx - in_ports.len() < out_ports.len() {
          Some(&out_ports[idx - in_ports.len()])
        } else {
          None
        }
      }
    }
  }
}

// impl<'a> Iterator for PortIterMut<'a> {
//   type Item = &'a mut Port<'a>;

//   fn next(&mut self) -> Option<&'a mut Port<'a>> {
//     use self::Prim::*;

//     let idx = self.count;
//     self.count += 1;

//     match self.prim {
//       &mut Foo { ref mut in_port, ref mut out_port } => {
//         if idx == 0 {
//           Some(in_port)
//         } else if idx == 1 {
//           Some(out_port)
//         } else {
//           None
//         }
//       },
//       &mut Bar { ref mut in_port, ref mut out_ports } => {
//         if idx == 0 {
//           Some(in_port)
//         } else if idx - 1 < out_ports.len() {
//           Some(&mut out_ports[idx - 1])
//         } else {
//           None
//         }
//       },
//       &mut Baz { ref mut in_ports, ref mut out_ports } => {
//         if idx < in_ports.len() {
//           Some(&mut in_ports[idx])
//         } else if idx - in_ports.len() < out_ports.len() {
//           Some(&mut out_ports[idx - in_ports.len()])
//         } else {
//           None
//         }
//       }
//     }
//   }
// }

// impl<'a, 'b> Iterator for PortIterMut<'a, 'b> {
//   type Item = &'b mut Port<'a>;

//   fn next(&mut self) -> Option<&'b mut Port<'a>> {
//     let &mut PortIterMut {
//       ref mut single_0,
//       ref mut single_1,
//       ref mut multi_0,
//       ref mut multi_1,
//     } = self;

//     if single_0.is_some() {
//       return single_0.take();
//     }

//     if single_1.is_some() {
//       return single_1.take();
//     }

//     let multi_0 = multi_0.as_mut().and_then(|iter| iter.next());

//     if multi_0.is_some() {
//       return multi_0;
//     }

//     multi_1.as_mut().and_then(|iter| iter.next())
//   }
// }

fn main() {
  let mut l = vec![
    Prim::Foo {
      in_port: Port { name: "foo_in" },
      out_port: Port { name: "foo_out" },
    },
    Prim::Bar {
      in_port: Port { name: "bar_in" },
      out_ports: vec![
        Port { name: "bar_out_0" },
        Port { name: "bar_out_1" },
      ],
    },
    Prim::Baz {
      in_ports: vec![
        Port { name: "baz_in_0" },
        Port { name: "baz_in_1" },
      ],
      out_ports: vec![
        Port { name: "baz_out_0" },
        Port { name: "baz_out_1" },
      ]
    },
  ];

  for p in &l {
    println!("{}", p);
    for port in p {
      println!("  {}", port);
    }
  }

  // for p in &mut l {
  //   for port in p {
  //     port.name = "foo";
  //   }
  // }

  for p in &mut l {
    p.iterate_mut(|port| { port.name = "foo" });
  }

  for p in &l {
    println!("{}", p);
    for port in p {
      println!("  {}", port);
    }
  }
}
