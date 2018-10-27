use std::hash::{Hash, Hasher};
use std::collections::HashSet;

#[derive(Debug, Clone, Copy)]
struct Foo {
  elem: usize,
}

impl PartialEq for Foo {
  #[inline]
  fn eq(&self, other: &Self) -> bool {
    self as *const Self == other as *const Self
  }
}

impl Eq for Foo {}

impl Hash for Foo {
  #[inline]
  fn hash<H: Hasher>(&self, state: &mut H) {
    (self as *const Self).hash(state);
  }
}


fn main() {
  let a = Foo { elem: 5 };
  let b = a.clone();
  let c = a;

  let mut set = HashSet::new();

  assert!(a != b);
  assert!(a != c);
  assert!(b != c);

  if !set.insert(a) {
    unreachable!();
  }

  if !set.insert(b) {
    unreachable!();
  }

  if !set.insert(c) {
    unreachable!();
  }

  println!("{:#?}", set);
}
