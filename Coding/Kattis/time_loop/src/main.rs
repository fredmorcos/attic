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

  for i in 1 ..= num {
    println!("{} Abracadabra", i);
  }
}
