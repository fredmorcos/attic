#[derive(Debug)]
pub struct Rectangle {
  length: u32,
  width: u32,
}

impl Rectangle {
  pub fn can_hold(&self, other: &Rectangle) -> bool {
    self.length > other.length && self.width > other.width
  }
}

pub fn add_two(a: i32) -> i32 {
  a + 2
}

#[cfg(test)]
mod tests {
  use super::Rectangle;
  use super::add_two;

  #[test]
  fn it_works() {}

  #[test]
  fn exploration() {}

  #[test]
  #[ignore] // this test purposefully fails
  fn another() {
    panic!("Make the test fail");
  }

  #[test]
  fn larger_can_hold_smaller() {
    let larger = Rectangle {
      length: 8,
      width: 7,
    };

    let smaller = Rectangle {
      length: 5,
      width: 1,
    };

    assert!(larger.can_hold(&smaller));
  }

  #[test]
  fn smaller_can_hold_larger() {
    let larger = Rectangle {
      length: 8,
      width: 7,
    };

    let smaller = Rectangle {
      length: 5,
      width: 1,
    };

    assert!(!smaller.can_hold(&larger));
  }

  #[test]
  fn it_adds_two() {
    assert_eq!(4, add_two(2));
  }
}

pub fn greeting(name: &str) -> String {
  format!("Hello {}!", name)
}

pub fn greeting2(_: &str) -> String {
  String::from("Hello!")
}

#[cfg(test)]
mod tests2 {
  use super::{greeting, greeting2};

  #[test]
  fn greeting_contains_name() {
    let result = greeting("Carol");
    assert!(result.contains("Carol"));
  }

  #[test]
  #[ignore] // this test purposefully fails
  fn greeting2_contains_name() {
    let name = "Carol";
    let result = greeting2(name);
    assert!(
      result.contains(name),
      "Greeting did not contain name ({}), value was `{}`",
      name,
      result
    );
  }
}

#[allow(dead_code)]
struct Guess {
  value: u32,
}

#[allow(dead_code)]
impl Guess {
  pub fn new(value: u32) -> Guess {
    if value < 1 || value > 100 {
      panic!("Guess value must be between 1 and 100, got {}", value);
    }

    Guess { value: value }
  }
}

#[cfg(test)]
mod guess_test {
  use super::Guess;

  #[test]
  #[should_panic]
  fn greate_than_100() {
    Guess::new(200);
  }
}
