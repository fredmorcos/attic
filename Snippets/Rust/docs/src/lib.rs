//! This crate is a simple test for documentation stuff.

/// Adds one to the number given.
///
/// # Examples
///
/// ```
/// use docs::add_one;
/// let five = 5;
/// assert_eq!(6, add_one(five));
/// ```
pub fn add_one(x: i32) -> i32 {
  x + 1
}

#[cfg(test)]
mod tests {
  #[test]
  fn it_works() {}
}
