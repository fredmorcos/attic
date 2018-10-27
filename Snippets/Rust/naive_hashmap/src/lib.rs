use std::hash::{BuildHasher, Hash, Hasher};
use std::borrow::Borrow;
use std::collections::hash_map::RandomState;
use std::{cmp, mem, ptr};
use std::fmt::Debug;

#[derive(Default)]
pub struct HashMap<K: Eq, V: Debug, S = RandomState> {
  hash_builder: S,
  data: Vec<(u64, K, V)>,
}

fn make_hash<T: ?Sized, S>(hash_builder: &S, t: &T) -> u64
  where T: Hash,
        S: BuildHasher
{
  let mut state = hash_builder.build_hasher();
  t.hash(&mut state);
  state.finish()
}

impl<K: Eq + Hash, V: Debug> HashMap<K, V, RandomState> {
  // pub fn new() -> Self {
  //   Self {
  //     hash_builder: RandomState::new(),
  //     data: Vec::new(),
  //   }
  // }

  pub fn insert(&mut self, k: K, v: V) -> Option<V> {
    let hash = make_hash(&self.hash_builder, &k);
    let end = self.data.len();

    for i in 0 .. end {
      match self.data[i].0.cmp(&hash) {
        cmp::Ordering::Greater => {
          self.data.insert(i, (hash, k, v));
          return None;
        }
        cmp::Ordering::Less => continue,
        cmp::Ordering::Equal => {
          let old = mem::replace(&mut self.data[i].2, v);
          return Some(old);
        }
      }
    }

    self.data.push((hash, k, v));
    None
  }

  pub fn get<Q: ?Sized>(&mut self, k: &Q) -> Option<&V>
    where K: Borrow<Q> + Debug,
          Q: Hash + Eq + Debug,
  {
    let hash = make_hash(&self.hash_builder, &k);

    for &(bucket_hash, _, ref v) in &self.data {
      if hash == bucket_hash {
        return Some(v);
      }
    }

    None
  }
}

impl<K: Eq + Hash, V: Debug, S: BuildHasher> HashMap<K, V, S> {
  pub fn with_hasher(hash_builder: S) -> Self {
    Self {
      hash_builder,
      data: Vec::new(),
    }
  }
}

pub struct HashMapU8<V: Debug> {
  data: [Option<V>; 256],
}

impl<V: Debug> Default for HashMapU8<V> {
  fn default() -> Self {
    Self::new()
  }
}

impl<V: Debug> HashMapU8<V> {
  pub fn new() -> Self {
    let data = unsafe {
      let mut data: [Option<V>; 256] = mem::uninitialized();

      for element in data.iter_mut() {
        ptr::write(element, None);
      }

      data
    };

    HashMapU8 { data }
  }

  pub fn insert(&mut self, k: u8, v: V) -> Option<V> {
    mem::replace(&mut self.data[(k as usize)], Some(v))
  }

  pub fn get(&mut self, k: &u8) -> Option<&V> {
    let val = unsafe {
      self.data.get_unchecked(*k as usize)
    };
    val.as_ref()
  }
}

#[cfg(test)]
mod tests {
  extern crate quickcheck;

  use super::*;
  use quickcheck::{Arbitrary, Gen, QuickCheck, TestResult};

  #[test]
  fn get_what_you_give() {
    fn property(k: u16, v: u16) -> TestResult {
      let mut system_under_test = HashMap::default();

      assert_eq!(None, system_under_test.insert(k, v));
      assert_eq!(Some(&v), system_under_test.get(&k));

      TestResult::passed()
    }

    QuickCheck::new().quickcheck(property as fn(u16, u16) -> TestResult);
  }

  #[derive(Clone, Debug)]
  enum Action<T: Arbitrary> {
    Insert(T, u16),
    Lookup(T),
  }

  impl<T: Arbitrary> Arbitrary for Action<T> {
    fn arbitrary<G: Gen>(g: &mut G) -> Action<T> {
      let i: usize = g.gen_range(0, 100);

      match i {
        0 ... 50 => Action::Insert(Arbitrary::arbitrary(g), u16::arbitrary(g)),
        _ => Action::Lookup(Arbitrary::arbitrary(g)),
      }
    }
  }

  #[test]
  fn sut_vs_genuine_article() {
    fn property<T: Arbitrary + Eq + Hash + Debug>(actions: Vec<Action<T>>) -> TestResult {
      let mut model = ::std::collections::hash_map::HashMap::new();
      let mut system_under_test = HashMap::default();

      for action in actions.into_iter() {
        match action {
          Action::Insert(k, v) =>
            assert_eq!(model.insert(k.clone(), v), system_under_test.insert(k, v)),
          Action::Lookup(k) =>
            assert_eq!(model.get(&k), system_under_test.get(&k)),
        }
      }

      TestResult::passed()
    }

    QuickCheck::new().quickcheck(property as fn(Vec<Action<u8>>) -> TestResult);
  }
}
