use std::str::FromStr;

pub fn run(input: &str) {
  let commands = input.lines().collect::<Vec<_>>();
  let loc = navigate(&commands);
  println!("  Part 1:");
  println!("    Depth: {}", loc.depth);
  println!("    Horizontal Position: {}", loc.horizontal_position);
  println!("    Depth * Horizontal Position: {}", loc.depth * loc.horizontal_position);
  let loc = navigate_with_aim(&commands);
  println!("  Part 2:");
  println!("    Depth: {}", loc.depth);
  println!("    Horizontal Position: {}", loc.horizontal_position);
  println!("    Depth * Horizontal Position: {}", loc.depth * loc.horizontal_position);
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
enum Command {
  Forward(usize),
  Up(usize),
  Down(usize),
}

#[derive(Debug, Copy, Clone, Default)]
struct Location {
  horizontal_position: usize,
  depth: usize,
  aim: usize,
}

impl FromStr for Command {
  type Err = ();
  fn from_str(s: &str) -> Result<Self, Self::Err> {
    let mut iter = s.split_whitespace();
    let command = iter.next().unwrap();
    let distance = iter.next().unwrap().parse::<usize>().unwrap();

    match command {
      "forward" => Ok(Self::Forward(distance)),
      "up" => Ok(Self::Up(distance)),
      "down" => Ok(Self::Down(distance)),
      _ => Err(()),
    }
  }
}

fn navigate(commands: &[&str]) -> Location {
  let mut location = Location::default();

  // parse vector element
  // split with whitespace
  for cmd in commands {
    let cmd = Command::from_str(cmd).unwrap();
    match cmd {
      Command::Forward(n) => location.horizontal_position += n,
      Command::Up(n) => location.depth -= n,
      Command::Down(n) => location.depth += n,
    }
  }
  location
}

fn navigate_with_aim(commands: &[&str]) -> Location {
  let mut location = Location::default();

  // parse vector element
  // split with whitespace
  for cmd in commands {
    let cmd = Command::from_str(cmd).unwrap();
    match cmd {
      Command::Forward(n) => {
        location.horizontal_position += n;
        location.depth += location.aim * n;
      }
      Command::Up(n) => location.aim -= n,
      Command::Down(n) => location.aim += n,
    }
  }
  location
}

#[cfg(test)]
mod tests {
  use super::*;
  use indoc::indoc;

  #[test]
  fn can_create_command_from_string() {
    assert_eq!(Command::from_str("forward 1").unwrap(), Command::Forward(1));
  }

  #[test]
  fn going_forward_by_1() {
    let commands = vec!["forward 1"];
    let loc = navigate(&commands);
    assert_eq!(loc.horizontal_position, 1);
    assert_eq!(loc.depth, 0);
  }

  #[test]
  fn example_input() {
    let input = indoc! {"
      forward 5
      down 5
      forward 8
      up 3
      down 8
      forward 2
    "};
    let commands = input.lines().collect::<Vec<_>>();
    let loc = navigate(&commands);
    assert_eq!(loc.horizontal_position, 15);
    assert_eq!(loc.depth, 10);
  }

  #[test]
  fn navigate_with_aim_on_example_input() {
    let input = indoc! {"
      forward 5
      down 5
      forward 8
      up 3
      down 8
      forward 2
    "};
    let commands = input.lines().collect::<Vec<_>>();
    let loc = navigate_with_aim(&commands);
    assert_eq!(loc.horizontal_position * loc.depth, 900);
  }

  #[test]
  fn puzzle_input() {
    let input = include_str!("../puzzles/day02_input_Fred.txt"); // "198\n201\n208\n..."
    let commands = input.lines().collect::<Vec<_>>();
    let loc = navigate(&commands);
    assert_eq!(loc.horizontal_position, 1906);
    assert_eq!(loc.depth, 1017);
    assert_eq!(loc.depth * loc.horizontal_position, 1938402);
  }

  #[test]
  fn puzzle_input_aim() {
    let input = include_str!("../puzzles/day02_input_Fred.txt"); // "198\n201\n208\n..."
    let commands = input.lines().collect::<Vec<_>>();
    let loc = navigate_with_aim(&commands);
    assert_eq!(loc.horizontal_position, 1906);
    assert_eq!(loc.depth, 1021972);
    assert_eq!(loc.depth * loc.horizontal_position, 1947878632);
  }
}
