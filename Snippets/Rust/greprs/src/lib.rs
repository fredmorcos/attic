use std::env;
use std::error::Error;
use std::fs::File;
use std::io::prelude::*;
use std::io::BufReader;

pub struct Config {
  pub query: String,
  pub filename: String,
  // pub struct Config<'a> {
  //   pub query: &'a String,
  //   pub filename: &'a String,
  pub case_sensitive: bool,
}

impl Config {
  // impl<'a> Config<'a> {
  // pub fn new(args: &'a [String]) -> Result<Self, &'static str> {
  pub fn new(mut args: std::env::Args) -> Result<Self, &'static str> {
    // let args_len: usize = args.len();

    // if args_len < 3 {
    //   return Err("Not enough arguments");
    // } else if args_len > 3 {
    //   return Err("Too many arguments");
    // }

    args.next();

    let query = match args.next() {
      Some(arg) => arg,
      None => return Err("Not enough arguments"),
    };

    let filename = match args.next() {
      Some(arg) => arg,
      None => return Err("Not enough arguments"),
    };

    let _: Option<String> = match args.next() {
      Some(_) => return Err("Too many arguments"),
      None => None,
    };

    Ok(Self {
      query: query,
      filename: filename,
      case_sensitive: env::var("CASE_INSENSITIVE").is_err(),
    })
  }
}

pub fn run(config: &Config) -> Result<(), Box<Error>> {
  let f: File = File::open(&config.filename)?;
  let mut f: BufReader<File> = BufReader::new(f);
  let mut contents: String = String::new();
  f.read_to_string(&mut contents)?;

  let results = if config.case_sensitive {
    search(&config.query, &contents)
  } else {
    search_case_insensitive(&config.query, &contents)
  };

  for line in results {
    println!("{}", line);
  }

  Ok(())
}

fn search<'a>(query: &str, contents: &'a str) -> Vec<&'a str> {
  // let mut results: Vec<&str> = Vec::new();

  // for line in contents.lines() {
  //   if line.contains(query) {
  //     results.push(line);
  //   }
  // }

  // results

  contents
    .lines()
    .filter(|line| line.contains(query))
    .collect()
}

fn search_case_insensitive<'a>(query: &str, contents: &'a str) -> Vec<&'a str> {
  let query: String = query.to_lowercase();
  // let mut results: Vec<&str> = Vec::new();

  // for line in contents.lines() {
  //   if line.to_lowercase().contains(&query) {
  //     results.push(line);
  //   }
  // }

  // results

  contents
    .lines()
    .filter(|line| line.to_lowercase().contains(&query))
    .collect()
}

#[cfg(test)]
mod test {
  use super::*;

  #[test]
  fn one_result() {
    let query = "duct";
    let contents = "\
Rust:
safe, fast, productive.
Pick three.";

    assert_eq!(vec!["safe, fast, productive."], search(query, contents));
  }

  #[test]
  fn case_insensitive() {
    let query = "rUsT";
    let contents = "\
Rust:
safe, fast, productive.
Pick three.
Trust me.";

    assert_eq!(
      vec!["Rust:", "Trust me."],
      search_case_insensitive(query, contents)
    );
  }
}
