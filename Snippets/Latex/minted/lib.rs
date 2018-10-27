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
