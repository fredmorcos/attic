extern crate greprs;

use std::env;
use std::process;
use std::io::Write;
// use std::io::prelude::*;

use greprs::Config;

fn main() {
  let mut stderr = std::io::stderr();

  // let args: Vec<String> = env::args().collect();
  // let config: Config = Config::new(&args[..]).unwrap_or_else(|err| {
  let config: Config = Config::new(env::args()).unwrap_or_else(|err| {
    writeln!(&mut stderr, "Error: {}", err).expect("Error: cannot write to stderr");
    process::exit(1);
  });

  if let Err(err) = greprs::run(&config) {
    writeln!(&mut stderr, "Error: {}", err).expect("Error: cannot write to stderr");
    process::exit(1);
  }
}
