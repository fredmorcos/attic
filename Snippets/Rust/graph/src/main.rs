extern crate petgraph;

use petgraph::Graph;
use petgraph::visit::Topo;

fn main() {
  let mut g: Graph<String, u8> = Graph::new();
  let p1 = g.add_node("port1".to_owned());
  let p2 = g.add_node("port2".to_owned());
  let p3 = g.add_node("port3".to_owned());
  let p4 = g.add_node("port4".to_owned());
  let p5 = g.add_node("port5".to_owned());
  let p6 = g.add_node("port6".to_owned());
  g.extend_with_edges(&[(p1, p2), (p2, p3), (p3, p4),
                        (p4, p5), (p5, p6)]);

  println!("Starting ports");

  for n in g.externals(petgraph::Incoming) {
    if let Some(n) = g.node_weight(n) {
      println!("{}", n);
    }
  }

  println!("Ending ports");

  for n in g.externals(petgraph::Outgoing) {
    if let Some(n) = g.node_weight(n) {
      println!("{}", n);
    }
  }

  println!("---");

  let mut v = Topo::new(&g);

  while let Some(n) = v.next(&g) {
    if let Some(n) = g.node_weight(n) {
      println!("{}", n);
    } else {
      println!("Nothing");
    }
  }
}
