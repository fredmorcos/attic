use std::fs::File;
use std::io::{BufRead, BufReader};
use std::str::FromStr;
use std::path::Path;
use std::fmt::Debug;
use std::collections::BTreeSet;

fn read_lines<P, T>(path: P) -> impl Iterator<Item = T>
  where <T as std::str::FromStr>::Err: Debug,
        P: AsRef<Path>,
        T: FromStr,
{
  BufReader::new(File::open(path).unwrap())
    .lines()
    .map(|e| T::from_str(&e.unwrap()).unwrap())
}

fn d1_1() {
  let mut freq: i64 = 0;

  for jump in read_lines::<_, i64>("day1") {
    freq += jump;
  }

  println!("1_1 {}", freq);
}

fn d1_2() {
  let jumps: Vec<i64> = read_lines("day1").collect();
  // let mut freqs: Vec<i64> = vec![0];
  let mut freqs: BTreeSet<i64> = BTreeSet::new();
  let mut freq: i64 = 0;

  freqs.insert(0);

  'outer: loop {
    for jump in jumps.iter() {
      freq += jump;

      if freqs.contains(&freq) {
        break 'outer;
      }

      freqs.insert(freq);
    }
  }

  println!("1_2 {}", freq);
}

fn main() {
  d1_1();
  d1_2();
}
