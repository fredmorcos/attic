use std::rc::Rc;
use std::rc::Weak;
use std::cell::RefCell;

#[derive(Debug)]
enum List {
  Cons(i32, Box<List>),
  Nil,
}

use std::ops::Deref;

struct Mp3 {
  audio: Vec<u8>,
  _artist: Option<String>,
  _title: Option<String>,
}

impl Deref for Mp3 {
  type Target = Vec<u8>;

  fn deref(&self) -> &Vec<u8> {
    &self.audio
  }
}

fn compress_mp3(audio: &[u8]) -> Vec<u8> {
  Vec::from(audio)
}

#[derive(Debug)]
enum RcList {
  RcCons(i32, Rc<RcList>),
  RcNil,
}

#[derive(Debug)]
enum CycleList {
  Cons(i32, RefCell<Rc<CycleList>>),
  Nil,
}

impl CycleList {
  fn tail(&self) -> Option<&RefCell<Rc<CycleList>>> {
    match *self {
      CycleList::Cons(_, ref tail) => Some(tail),
      CycleList::Nil => None,
    }
  }
}

#[derive(Debug)]
struct Node {
  pub value: i32,
  pub children: RefCell<Vec<Rc<Node>>>,
  pub parent: RefCell<Weak<Node>>,
}

fn main() {
  let b = Box::new(5);
  println!("b = {}", b);

  use List::{Cons, Nil};

  let list = Cons(1, Box::new(Cons(2, Box::new(Cons(3, Box::new(Nil))))));

  println!("{:?}", list);

  let song = Mp3 {
    audio: vec![1, 2, 3],
    _artist: Some(String::from("Nirvana")),
    _title: Some(String::from("Smells Like Teen Spirit")),
  };

  assert_eq!(vec![1, 2, 3], *song);

  compress_mp3(&song);

  let a = Rc::new(RcList::RcCons(5, Rc::new(RcList::RcCons(10, Rc::new(RcList::RcNil)))));

  println!("rc after a = {}", Rc::strong_count(&a));

  let _ = RcList::RcCons(3, a.clone());

  println!("rc after b = {}", Rc::strong_count(&a));

  {
    let _ = RcList::RcCons(4, a.clone());

    // println!("{:?}", b);
    // println!("{:?}", c);

    println!("rc after c = {}", Rc::strong_count(&a));
  }

  println!("rc after c is out of scope = {}", Rc::strong_count(&a));

  let a = Rc::new(CycleList::Cons(5, RefCell::new(Rc::new(CycleList::Nil))));

  println!("initial refcount = {}", Rc::strong_count(&a));
  println!("next item = {:?}", a.tail());

  let b = Rc::new(CycleList::Cons(10, RefCell::new(a.clone())));

  println!("refcount now with b->a = {}", Rc::strong_count(&a));
  println!("refcount of b = {}", Rc::strong_count(&b));
  println!("b's next item = {:?}", b.tail());

  if let Some(ref link) = a.tail() {
    *link.borrow_mut() = b.clone();
  }

  println!("b refcount after changing a to point to b = {}", Rc::strong_count(&b));
  println!("a refcount after changing a to point to b = {}", Rc::strong_count(&a));

  // This will overflow the stack because of an infinite loop (due to ref cycle)
  // println!("a's next element = {:?}", a.tail());

  let leaf = Rc::new(Node {
    value: 3,
    children: RefCell::new(vec![]),
    parent: RefCell::new(Weak::new()),
  });

  println!("Leaf parent = {:?}", leaf.parent.borrow().upgrade());

  let branch = Rc::new(Node {
    value: 5,
    children: RefCell::new(vec![leaf.clone()]),
    parent: RefCell::new(Weak::new()),
  });

  *leaf.parent.borrow_mut() = Rc::downgrade(&branch);

  println!("Leaf parent = {:?}", leaf.parent.borrow().upgrade());
}
