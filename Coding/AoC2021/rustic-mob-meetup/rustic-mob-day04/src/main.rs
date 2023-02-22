use regex::Regex;
use std::{error::Error, fmt, fs};
use walkdir::WalkDir;

mod day01;
mod day02;
mod day03;
mod day04;

static FUNCS: &[fn(&str)] = &[day01::run, day02::run, day03::run, day04::run];

struct MainErr(String);

impl MainErr {
  fn with_message(msg: String) -> Result<(), Box<dyn Error>> {
    Err(Box::new(Self(msg)))
  }
}

impl fmt::Display for MainErr {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "Error: {}", self.0)
  }
}

impl fmt::Debug for MainErr {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    fmt::Display::fmt(self, f)
  }
}

impl Error for MainErr {}

fn solution(day: u8, name: &str, input: &str) {
  println!("Solution for Day {} ({})", day, name);
  FUNCS[(day - 1) as usize](input);
  println!("---");
}

fn main() -> Result<(), Box<dyn Error>> {
  let regex = match Regex::new(r"^day(\d{2})_input_(.+).txt") {
    Ok(regex) => regex,
    Err(e) => return MainErr::with_message(format!("Failed to compile regex: {}", e)),
  };

  let puzzles_dir = WalkDir::new("puzzles").min_depth(1).max_depth(1);
  for entry in puzzles_dir {
    let entry = entry?;

    if let Some(filename) = entry.file_name().to_str() {
      let captures = match regex.captures(filename) {
        Some(captures) => captures,
        None => continue,
      };

      let day = match captures.get(1) {
        Some(day) => day.as_str().parse::<u8>()?,
        None => continue,
      };

      let name = match captures.get(2) {
        Some(name) => name.as_str(),
        None => continue,
      };

      let input = fs::read_to_string(entry.path())?;

      solution(day, name, &input);
    } else {
      return MainErr::with_message(format!("Invalid filename: {:#?}", entry.file_name()));
    }
  }

  Ok(())
}
