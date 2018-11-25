use std::io::{self, BufRead};

fn main() {
  let stdin = io::stdin();
  let num: u64 = stdin.lock()
                      .lines()
                      .take(1)
                      .next()
                      .unwrap()
                      .unwrap()
                      .parse()
                      .unwrap();

  eprintln!("Number = {}", num);
  println!("{}", if num % 2 == 0 { "Bob" } else { "Alice" });
}
