use std::io::{self, BufRead};
use std::collections::HashMap as Map;

fn main() {
  let table: &[u8] = "ABCDEFGHIJKLMNOPQRSTUVWXYZ_.".as_bytes();
  let table_char: Map<u8, usize> = table.iter()
                                        .enumerate()
                                        .map(|(i, &c)| (c, i))
                                        .collect();

  fn lookup(table: &[u8], table_char: &Map<u8, usize>, c: u8, n: usize) -> u8 {
    let l: usize = *table_char.get(&c).unwrap();
    table[(l + n) % table.len()]
  }

  let stdin = io::stdin();

  for line in stdin.lock().lines().map(|l| l.unwrap()) {
    let mut words = line.split_whitespace();
    let n: usize = words.next().unwrap().parse().unwrap();

    if n == 0 {
      break;
    }

    let msg: String = words.next().unwrap().into();
    let mut msg: Vec<u8> = msg.into_bytes();
    let msg: &mut [u8] = msg.as_mut_slice();
    msg.reverse();

    for c in msg.into_iter() {
      print!("{}", char::from(lookup(table, &table_char, *c, n)));
    }

    println!();
  }
}
