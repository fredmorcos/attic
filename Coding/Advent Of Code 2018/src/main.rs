use std::fs::File;
use std::io::{BufRead, BufReader};
use std::str::FromStr;
use std::path::Path;
use std::fmt::Debug;
use std::collections::{BTreeSet, HashMap as Map, HashSet as Set};
use std::num::ParseIntError;
use std::iter::Peekable;
use std::convert::From;

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

#[derive(Debug)]
struct Square {
  id: usize,

  x: usize,
  y: usize,
  w: usize,
  h: usize,
}

#[derive(Debug)]
enum ParseError {
  ParseInt(ParseIntError),
  UnexpectedEOF,
  Other(String),
}

impl From<ParseIntError> for ParseError {
  fn from(e: ParseIntError) -> Self {
    ParseError::ParseInt(e)
  }
}

fn skip_ws<I>(iter: &mut Peekable<I>)
  where I: Iterator<Item = char>,
{
  while let Some(&c) = iter.peek() {
    if c.is_whitespace() {
      iter.next();
    } else {
      break;
    }
  }
}

fn single<I>(iter: &mut Peekable<I>, c: char) -> Result<(), ParseError>
  where I: Iterator<Item = char>,
{
  if let Some(&c_) = iter.peek() {
    if c_ != c {
      return Err(ParseError::Other(
        format!("Expected a '{}', found a '{}'", c, c_)));
    }
    iter.next();
  } else {
    return Err(ParseError::UnexpectedEOF);
  }

  Ok(())
}

fn multi<T, I>(iter: &mut Peekable<I>) -> Result<T, <T as FromStr>::Err>
  where T: FromStr,
        I: Iterator<Item = char>,
{
  let mut id = String::new();

  while let Some(&c) = iter.peek() {
    if c.is_digit(10) {
      id.push(c);
      iter.next();
    } else {
      break;
    }
  }

  Ok(T::from_str(&id)?)
}

impl FromStr for Square {
  type Err = ParseError;

  fn from_str(input: &str) -> Result<Self, Self::Err> {
    let mut iter = input.chars().peekable();

    single(&mut iter, '#')?;

    let id = multi::<usize, _>(&mut iter)?;

    skip_ws(&mut iter);
    single(&mut iter, '@')?;
    skip_ws(&mut iter);

    let x = multi::<usize, _>(&mut iter)?;

    single(&mut iter, ',')?;

    let y = multi::<usize, _>(&mut iter)?;

    single(&mut iter, ':')?;
    skip_ws(&mut iter);

    let w = multi::<usize, _>(&mut iter)?;

    single(&mut iter, 'x')?;

    let h = multi::<usize, _>(&mut iter)?;

    Ok(Square { id, x, y, w, h })
  }
}

fn d3_1_2() {
  let squares: Vec<Square> = read_lines("day3").collect();
  let mut overlaps: Set<(usize, usize)> = Set::new();

  for (idx, i) in squares.iter().enumerate() {
    for j in &squares[idx + 1 ..] {
      let i_left = i.x;
      let i_right = i.x + i.w;
      let i_top = i.y;
      let i_bot = i.y + i.h;

      let j_left = j.x;
      let j_right = j.x + j.w;
      let j_top = j.y;
      let j_bot = j.y + j.h;

      let right = i_right.min(j_right);
      let left = i_left.max(j_left);
      let bot = i_bot.min(j_bot);
      let top = i_top.max(j_top);

      for i in left .. right {
        for j in top .. bot {
          if !overlaps.contains(&(i ,j)) {
            overlaps.insert((i, j));
          }
        }
      }
    }
  }

  println!("d3_1 {}", overlaps.len());

  let mut id = None;

  'outer: for s in squares {
    for i in s.x .. s.x + s.w {
      for j in s.y .. s.y + s.h {
        if overlaps.contains(&(i, j)) {
          continue 'outer;
        }
      }
    }

    id = Some(s.id);
    break;
  }

  if let Some(id) = id {
    println!("d3_2 {}", id);
  }
}

fn d4_1() {
}

fn main() {
  d1_1();
  d1_2();
  d2_1();
  d2_2();
  d3_1_2();
  d4_1();
}
