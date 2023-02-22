pub fn run(input: &str) {
  let lines = input.lines().collect::<Vec<_>>();
  println!("  Part 1:");
  println!("    Power Consumption: {}", power_consumption(&lines));
}

// struct Counter {
//   zeros: usize,
//   ones: usize,
// }

// impl Counter {
//   fn new() -> Self {
//     Self { zeros: 0, ones: 0 }
//   }
// }

fn get_digit_at_position(input: &str, column: usize) -> char {
  input.chars().nth(column).unwrap()
}

/// Returns the number of lines with bit in position column set.
fn count_ones(lines: &[&str], column: usize) -> usize {
  lines.iter().filter(|&&line| get_digit_at_position(line, column) == '1').count()
}

use std::cmp::Ordering;

fn most_frequent_digit(lines: &[&str], column: usize) -> char {
  let number_of_ones = count_ones(lines, column);
  match number_of_ones.cmp(&(lines.len() / 2)) {
    Ordering::Greater => '1',
    Ordering::Less => '0',
    Ordering::Equal => unimplemented!("Same occurrence of zeros and ones."),
  }
}

fn number_of_columns(lines: &[&str]) -> usize {
  lines[0].len()
}

fn gamma(lines: &[&str]) -> usize {
  let mut gamma = 0;
  let number_of_columns = number_of_columns(lines);
  for column in 0..number_of_columns {
    let digit = most_frequent_digit(lines, column);
    let digit = if digit == '1' {
      1
    } else if digit == '0' {
      0
    } else {
      unimplemented!("Same occurrence of zeros and ones.");
    };
    gamma = gamma * 2 + digit;
  }

  gamma
}

fn epsilon(lines: &[&str], gamma: usize) -> usize {
  let number_of_columns = number_of_columns(lines);
  let mask = 2_usize.pow(number_of_columns as u32) - 1; // e.g. for 5 columns : 0b00000111111
  gamma ^ mask
}

pub fn power_consumption(lines: &[&str]) -> usize {
  let gamma = gamma(lines);
  let epsilon = epsilon(lines, gamma);
  gamma * epsilon
}

#[cfg(test)]
mod tests {
  use super::*;
  use indoc::indoc;

  #[test]
  fn counts_ones() {
    // "000\n001\n010"
    // let mut counter = Counter::new();
    // counter.accept("0");
    // assert_eq!(0, counter.count_ones(0));
  }

  #[test]
  fn create_counter() {
    // let counter = Counter::new();
    // assert_eq!(0, counter.count_ones(0));
  }

  #[test]
  fn test_get_digit_at_position() {
    assert_eq!('0', get_digit_at_position("010", 2));
    assert_eq!('1', get_digit_at_position("010", 1));
  }

  fn test_input() -> Vec<&'static str> {
    (indoc! {"
      00100
      11110
      10110
      10111
      10101
      01111
      00111
      11100
      10000
      11001
      00010
      01010
    "})
    .lines()
    .collect::<Vec<_>>()
  }

  #[test]
  fn test_count_ones() {
    let lines = test_input();
    assert_eq!(7, count_ones(&lines, 0));
    assert_eq!(5, count_ones(&lines, 1));
    assert_eq!(7, count_ones(&lines, 3));
  }

  #[test]
  fn test_most_frequent_digit() {
    let lines = test_input();
    assert_eq!('1', most_frequent_digit(&lines, 0));
    assert_eq!('0', most_frequent_digit(&lines, 1));
  }

  #[test]
  fn test_count_zeros() {
    let lines = test_input();
    assert_eq!(7, count_ones(&lines, 0));
    assert_eq!(5, count_ones(&lines, 1));
    assert_eq!(7, count_ones(&lines, 3));
  }

  #[test]
  fn test_number_of_columns() {
    let lines = test_input();
    assert_eq!(5, number_of_columns(&lines));
  }

  #[test]
  fn test_gamma() {
    let lines = test_input();
    assert_eq!(22, gamma(&lines));
  }

  #[test]
  fn test_epsilon() {
    let lines = test_input();
    assert_eq!(9, epsilon(&lines, 22));
  }

  #[test]
  fn test_power_consumption() {
    let lines = test_input();
    assert_eq!(198, power_consumption(&lines));
  }

  #[test]
  fn test_power_consumption_input_file() {
    let lines = include_str!("../puzzles/day03_input_Fred.txt").lines().collect::<Vec<_>>();
    assert_eq!(4138664, power_consumption(&lines));
  }
}
