pub enum List<T> {
  Nil,
  Node {
    element: T,
    next: Box<List<T>>,
  },
}

impl<T> Default for List<T> {
  fn default() -> Self {
    List::Nil
  }
}

impl<T> List<T> {
  pub fn new() -> List<T> {
    Self::default()
  }

  pub fn prepend(self, element: T) -> List<T> {
    List::Node { element, next: Box::new(self) }
  }

  pub fn next(&self) -> Option<&List<T>> {
    match self {
      List::Nil => None,
      List::Node { next, .. } => Some(next),
    }
  }

  pub fn pop_front(self) -> Option<(T, List<T>)> {
    match self {
      List::Nil => None,
      List::Node { element, next } => Some((element, *next)),
    }
  }

  pub fn is_empty(&self) -> bool {
    self.len() == 0
  }

  pub fn len(&self) -> usize {
    let mut len = 0;
    let mut cur = self;

    while let Some(node) = cur.next() {
      len += 1;
      cur = node;
    }

    len
  }
}

pub mod extras {
  use super::List;

  pub fn create_linkedlist(n: u32) -> List<u32> {
    let mut list = List::new();

    for i in 0 .. n {
      list = list.prepend(i);
    }

    list
  }

  pub fn traverse_linkedlist(list: &List<u32>) -> u64 {
    use std::ops::Deref;

    let mut cur = list;
    let mut sum = match list {
      List::Nil => 0,
      List::Node { element, .. } => u64::from(*element),
    };

    while let Some(next) = cur.next() {
      cur = next;

      sum += match next.deref() {
        List::Nil => 0,
        List::Node { element, .. } => u64::from(*element),
      };
    }

    sum
  }

  pub fn drop_linkedlist(mut list: List<u32>) {
    while let Some((_, next)) = list.pop_front() {
      list = next;
    }
  }

  pub fn create_vector(n: u32) -> Vec<u32> {
    let mut vec = Vec::new();

    for i in 0 .. n {
      vec.push(i);
    }

    vec
  }

  pub fn traverse_vector_iter(vec: &[u32]) -> u64 {
    let mut sum = 0;

    for &i in vec {
      sum += u64::from(i);
    }

    sum
  }

  pub fn traverse_vector_index(vec: &[u32]) -> u64 {
    let mut sum = 0;

    for i in 0 .. vec.len() {
      sum += u64::from(vec[i]);
    }

    sum
  }

  pub fn traverse_vector_unchecked(vec: &[u32]) -> u64 {
    let mut sum = 0;

    for i in 0 .. vec.len() {
      sum += u64::from(unsafe { *vec.get_unchecked(i) });
    }

    sum
  }
}

// #[cfg(test)]
// mod tests {
//   use super::List;

//   #[test]
//   fn test_list() {
//     let mut list = List::new();

//     for i in 0 .. 100_000_000 {
//       list = list.prepend(i);
//     }

//     assert_eq!(list.len(), 100_000_000);

//     while let Some((_, next)) = list.pop_front() {
//       list = next;
//     }
//   }
// }
