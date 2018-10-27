use std::cmp::PartialOrd;

struct Point<T> {
  x: T,
  y: T,
}

impl<T> Point<T> {
  fn x(&self) -> &T {
    &self.x
  }
}

impl<T> Point<T> {
  fn mixup<V>(self, other: Point<V>) -> Point<V> {
    Point {
      x: other.x,
      y: other.y,
    }
  }
}

pub trait Summarizable {
  fn summary(&self) -> String;
}

pub struct NewsArticle {
  pub headline: String,
  pub location: String,
  pub author: String,
  pub content: String,
}

pub struct Tweet {
  pub username: String,
  pub content: String,
  pub reply: bool,
  pub retweet: bool,
}

impl Summarizable for NewsArticle {
  fn summary(&self) -> String {
    format!(
      "{}, by {} from {}",
      self.headline,
      self.author,
      self.location
    )
  }
}

impl Summarizable for Tweet {
  fn summary(&self) -> String {
    format!("{}: {}", self.username, self.content)
  }
}

fn main() {
  let nums1 = vec![34, 50, 25, 100, 65];
  let nums2 = vec![102, 34, 6000, 89, 54, 2, 43, 8];

  let chars = vec!['y', 'm', 'a', 'q'];

  let int_point = Point { x: 5, y: 10 };
  let float_point = Point { x: 1.0, y: 4.0 };

  println!("Largest number: {}", largest(&nums1));
  println!("Largest number: {}", largest(&nums2));
}

fn largest<T: PartialOrd>(list: &[T]) -> &T {
  // fn largest(list: &[i32]) -> i32 {
  let mut largest = &list[0];

  for n in list {
    if n > largest {
      largest = n;
    }
  }

  largest
}
