pub fn run(input: &str) {
  let lines = input.lines().collect::<Vec<_>>();
  println!("  Part 1:");
  println!("    Power Consumption: {}", power_consumption(&lines));
  println!("  Part 2:");
  let oxygen_rating = oxygen_rating(&lines);
  let co2_rating = co2_rating(&lines);
  println!("    Oxygen Rating: {}", oxygen_rating);
  println!("    CO2 Rating: {}", co2_rating);
  println!("    Life Support Rating: {}", oxygen_rating * co2_rating);
}

fn get_digit_at_position(input: &str, column: usize) -> char {
  input.chars().nth(column).unwrap()
}

/// Returns the number of lines with bit in position column set.
fn count_ones(lines: &[&str], column: usize) -> usize {
  lines.iter().filter(|&&line| get_digit_at_position(line, column) == '1').count()
}

use std::cmp::Ordering;

enum Rating {
  Oxygen,
  CO2,
  None,
}

impl From<Rating> for char {
  fn from(rating: Rating) -> Self {
    match rating {
      Rating::Oxygen => '1',
      Rating::CO2 => '0',
      Rating::None => unimplemented!("Same occurrence of zeros and ones."),
    }
  }
}

fn most_frequent_digit(lines: &[&str], column: usize, rating: Rating) -> char {
  let number_of_ones = count_ones(lines, column);
  match (number_of_ones * 2).cmp(&lines.len()) {
    Ordering::Greater => '1',
    Ordering::Less => '0',
    Ordering::Equal => char::from(rating),
  }
}

fn least_frequent_digit(lines: &[&str], column: usize, rating: Rating) -> char {
  let number_of_ones = count_ones(lines, column);
  match (number_of_ones * 2).cmp(&lines.len()) {
    Ordering::Greater => '0',
    Ordering::Less => '1',
    Ordering::Equal => char::from(rating),
  }
}

fn number_of_columns(lines: &[&str]) -> usize {
  lines[0].len()
}

fn gamma(lines: &[&str]) -> usize {
  let mut gamma = 0;
  let number_of_columns = number_of_columns(lines);
  for column in 0..number_of_columns {
    let digit = most_frequent_digit(lines, column, Rating::None);
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

fn oxygen_rating(lines: &[&str]) -> usize {
  let number_of_columns = number_of_columns(lines);

  let mut filtered_lines = Vec::from(lines);
  for column in 0..number_of_columns {
    if filtered_lines.len() == 1 {
      break;
    }

    filtered_lines = filter_column_by_most_common_digit_with_rating(&filtered_lines, column, Rating::Oxygen);
  }

  assert_eq!(filtered_lines.len(), 1);

  usize::from_str_radix(filtered_lines[0], 2).unwrap()
}

fn co2_rating(lines: &[&str]) -> usize {
  let number_of_columns = number_of_columns(lines);

  let mut filtered_lines = Vec::from(lines);
  for column in 0..number_of_columns {
    if filtered_lines.len() == 1 {
      break;
    }

    filtered_lines = filter_column_by_least_common_digit_with_rating(&filtered_lines, column, Rating::CO2);
  }

  assert_eq!(filtered_lines.len(), 1);

  usize::from_str_radix(filtered_lines[0], 2).unwrap()
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
fn filter_column_by_most_common_digit<'a>(lines: &[&'a str], column: usize) -> Vec<&'a str> {
  let most_common_digit = most_frequent_digit(lines, column, Rating::None);

  lines
    .iter()
    .filter(|&element| get_digit_at_position(element, column) == most_common_digit)
    .copied()
    .collect()
}

fn filter_column_by_most_common_digit_with_rating<'a>(
  lines: &[&'a str],
  column: usize,
  rating: Rating,
) -> Vec<&'a str> {
  let most_common_digit = most_frequent_digit(lines, column, rating);

  lines
    .iter()
    .filter(|&element| get_digit_at_position(element, column) == most_common_digit)
    .copied()
    .collect()
}

fn filter_column_by_least_common_digit_with_rating<'a>(
  lines: &[&'a str],
  column: usize,
  rating: Rating,
) -> Vec<&'a str> {
  let least_common_digit = least_frequent_digit(lines, column, rating);

  lines
    .iter()
    .filter(|&element| get_digit_at_position(element, column) == least_common_digit)
    .copied()
    .collect()
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
    indoc! {"
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
    "}
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
    assert_eq!('1', most_frequent_digit(&lines, 0, Rating::None));
    assert_eq!('0', most_frequent_digit(&lines, 1, Rating::None));
  }

  #[test]
  fn test_least_frequent_digit() {
    let lines = test_input();
    assert_eq!('0', least_frequent_digit(&lines, 0, Rating::None));
    assert_eq!('1', least_frequent_digit(&lines, 1, Rating::None));
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

  #[test]
  fn test_filter_column_by_most_common_digit() {
    let lines = test_input();
    assert_eq!(7, filter_column_by_most_common_digit(&lines, 0).len());
  }

  #[test]
  fn test_filter_column_by_most_common_digit_for_oxygen() {
    let lines = test_input();
    assert_eq!(7, filter_column_by_most_common_digit_with_rating(&lines, 0, Rating::Oxygen).len());
  }

  #[test]
  fn test_find_oxygen_rating() {
    let lines = test_input();
    let oxygen_rating = oxygen_rating(&lines);
    assert_eq!(0b10111, oxygen_rating);
  }

  #[test]
  fn test_find_co2_rating() {
    let lines = test_input();
    let co2_rating = co2_rating(&lines);
    assert_eq!(0b01010, co2_rating);
  }

  #[test]
  fn test_most_frequent_digit_with_equal_ones_and_zeroes_for_oxygen() {
    let lines = vec!["0", "1"];
    assert_eq!('1', most_frequent_digit(&lines, 0, Rating::Oxygen));
  }

  #[test]
  fn test_most_frequent_digit_with_equal_ones_and_zeroes_for_co2() {
    let lines = vec!["0", "1"];
    assert_eq!('0', most_frequent_digit(&lines, 0, Rating::CO2));
  }

  #[test]
  fn test_for_filter() {
    let input = vec!["11110", "10110", "10111", "10101", "11100", "10000", "11001"];
    let expected = vec!["10110", "10111", "10101", "10000"];
    assert_eq!(filter_column_by_most_common_digit_with_rating(&input, 1, Rating::Oxygen), expected);
  }

  #[test]
  fn test_for_filter_least() {
    let input = vec!["11110", "10110", "10111", "10101", "11100", "10000", "11001"];
    let expected = vec!["11110", "11100", "11001"];
    assert_eq!(filter_column_by_least_common_digit_with_rating(&input, 1, Rating::CO2), expected);
  }

  #[test]
  fn test_most_freq_digit() {
    let input = vec!["11110", "10110", "10111", "10101", "11100", "10000", "11001"];
    assert_eq!(most_frequent_digit(&input, 1, Rating::Oxygen), '0');
  }
}
