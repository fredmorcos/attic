extern crate fnv;
use fnv::FnvHashSet;

struct Set<T>(FnvHashSet<T>);

impl<T: Eq + Hash> Set<T> {
  fn new(cap: usize) -> Self {
    Set(FnvHashSet::with_capacity_and_hasher(cap, Default::default()))
  }
}

impl<'a, T> Into<&'a FnvHashSet<T>> for &'a Set<T> {
  fn into(self) -> &'a FnvHashSet<T> {
    let &Set(ref hash_set) = self;
    hash_set
  }
}

impl<'a, T> Into<&'a mut FnvHashSet<T>> for &'a mut Set<T> {
  fn into(self) -> &'a mut FnvHashSet<T> {
    let &mut Set(ref mut hash_set) = self;
    hash_set
  }
}

#[cfg(test)]
mod set_tests {
  extern crate fnv;
  use fnv::FnvHashSet;
  use super::Set;

  #[test]
  fn test_into() {
    let mut set: Set<usize> = Set::new(3);
    let hash_set: &mut FnvHashSet<usize> = (&mut set).into();
    if !hash_set.insert(1) {
      unreachable!();
    }
    if !hash_set.insert(2) {
      unreachable!();
    }
    if !hash_set.insert(3) {
      unreachable!();
    }
  }
}
