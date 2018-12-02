use std::fs::File;
use std::io::{BufRead, BufReader};
use std::str::FromStr;
use std::path::Path;
use std::fmt::Debug;
use std::collections::BTreeSet;
use std::collections::HashMap as Map;

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

fn d2_1() {
  let mut have_2: usize = 0;
  let mut have_3: usize = 0;

  for id in read_lines::<_, String>("day2") {
    let mut chars: Map<char, usize> = Map::new();

    for c in id.chars() {
      chars.entry(c).and_modify(|e| *e += 1).or_insert(1);
    }

    let mut has_2: bool = false;
    let mut has_3: bool = false;

    for (_, v) in chars {
      if v == 2 {
        has_2 = true;
      } else if v == 3 {
        has_3 = true;
      }
    }

    if has_2 {
      have_2 += 1;
    }

    if has_3 {
      have_3 += 1;
    }
  }

  println!("2_1 {}", have_2 * have_3);
}

fn d2_2() {
  let ids: Vec<String> = read_lines("day2").collect();
  let mut diff: Option<(&str, &str, usize)> = None;

  'outer: for id1 in &ids {
    'inner: for id2 in &ids {
      let mut one_diff: bool = false;

      for (i, (c1, c2)) in id1.chars().zip(id2.chars()).enumerate() {
        if c1 != c2 {
          if one_diff {
            continue 'inner;
          } else {
            one_diff = true;
            diff = Some((id1, id2, i));
          }
        }
      }

      if one_diff {
        break 'outer;
      }
    }
  }

  print!("2_2 ");

  if let Some((id1, id2, i)) = diff {
    for (j, (c1, c2)) in id1.chars().zip(id2.chars()).enumerate() {
      if i != j {
        assert_eq!(c1, c2);
        print!("{}", c1);
      }
    }

    println!();
  } else {
    println!("ERROR");
  }
}

fn main() {
  d1_1();
  d1_2();
  d2_1();
  d2_2();
}
