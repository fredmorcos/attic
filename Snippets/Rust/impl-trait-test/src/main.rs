use std::collections::{linked_list, LinkedList};

trait WhatAmI {
  fn what_am_i(&self) -> &'static str;
}

impl<T> WhatAmI for Vec<T> {
  fn what_am_i(&self) -> &'static str {
    "A Vector!"
  }
}

impl<T> WhatAmI for LinkedList<T> {
  fn what_am_i(&self) -> &'static str {
    "A List!"
  }
}

fn give_me_something(a_list: bool) -> Box<dyn WhatAmI> {
  if a_list {
    let l: LinkedList<u32> = LinkedList::new();
    Box::new(l)
  } else {
    let v: Vec<u32> = Vec::new();
    Box::new(v)
  }

  // let v: Vec<u32> = Vec::new();
  // Box::new(v)
}

// trait MyIterator<T: 'iter> {
//   fn next(&self) -> Option<T> {
//     None
//   }
// }

// impl<T> MyIterator<T> for std::vec::IntoIter<T> {}
// impl<T> MyIterator<T> for linked_list::IntoIter<T> {}

// fn give_me_iterable<'iter, T: 'iter>(a_list: bool) -> Box<MyIterator<T<'iter>>> {
//   if a_list {
//     Box::new(LinkedList::new().into_iter())
//   } else {
//     Box::new(Vec::new().into_iter())
//   }
// }

fn add1(n: u32) -> u32 {
  n + 1
}

fn give_me_some_closure() -> impl Fn(u32) -> u32 {
  // or: |x| x + 1
  add1
}

fn give_me_some_other_closure(n: u32) -> impl Fn() -> u32 {
  move || n + 1
}

fn give_me_a_closure(s: bool) -> Box<Fn(u32) -> u32> {
  if s {
    // or Box::new(add1)
    Box::new(|n| n + 1)
  } else {
    Box::new(|n| n + 2)
  }
}

fn main() {
  let something = give_me_something(true);
  println!("I Am {}", something.what_am_i());
  let something = give_me_something(false);
  println!("I Am {}", something.what_am_i());

  println!("Got {}", give_me_some_closure()(42));
  println!("Got {}", give_me_some_other_closure(12)());
  println!("Got {}", give_me_a_closure(false)(13));
}
