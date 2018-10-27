use std::str::FromStr;

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub enum Move { Forward, Right, Left }

#[derive(Debug, Clone, Copy)]
pub enum Dir { North, East, South, West }

impl Default for Dir {
  fn default() -> Self {
    Dir::North
  }
}

impl Dir {
  pub fn next(self, m: Move) -> Self {
    match m {
      Move::Forward => self,
      Move::Right => match self {
        Dir::North => Dir::East,
        Dir::East => Dir::South,
        Dir::South => Dir::West,
        Dir::West => Dir::North,
      },
      Move::Left => match self {
        Dir::North => Dir::West,
        Dir::West => Dir::South,
        Dir::South => Dir::East,
        Dir::East => Dir::North,
      },
    }
  }
}

#[derive(Debug, Clone, Copy, Default)]
pub struct DirState {
  dir: Dir,

  norths: usize,
  souths: usize,
  easts: usize,
  wests: usize,

  vert: i64,
  horiz: i64,
}

impl DirState {
  pub fn next(&mut self, m: Move) {
    match m {
      Move::Forward => match self.dir {
        Dir::North => {
          self.norths += 1;
          self.vert += 1;
        },
        Dir::East => {
          self.easts += 1;
          self.horiz += 1;
        },
        Dir::South => {
          self.souths += 1;
          self.vert -= 1;
        },
        Dir::West => {
          self.wests += 1;
          self.horiz -= 1;
        },
      },
      _ => {},
    }

    self.dir = self.dir.next(m);
  }
}

impl From<Move> for char {
  fn from(m: Move) -> Self {
    match m {
      Move::Forward => 'F',
      Move::Right => 'R',
      Move::Left => 'L',
    }
  }
}

impl From<char> for Move {
  fn from(c: char) -> Self {
    match c {
      'F' | 'f' => Move::Forward,
      'R' | 'r' => Move::Right,
      'L' | 'l' => Move::Left,
      _ => panic!("Unrecognized MOVE character {}", c),
    }
  }
}

impl std::fmt::Display for Move {
  fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
    write!(f, "{}", char::from(*self))
  }
}

fn expand_seq<'a>(mut i: impl Iterator<Item = &'a str>) -> Vec<Move> {
  let mut len = usize::from_str(i.next().unwrap()).unwrap();
  let mut res: Vec<Move> = Vec::new();

  loop {
    let moves = if let Some(moves) = i.next() {
      moves
    } else {
      break;
    };

    let nmoves = usize::from_str(i.next().unwrap()).unwrap();

    for _ in 0 .. nmoves {
      for m in moves.chars() {
        res.push(Move::from(m));
      }
    }

    len -= 1;
  }

  assert_eq!(len, 0);
  res
}

fn seq_dist(s: &[Move]) -> usize {
  s.iter().filter(|&&m| m == Move::Forward).count()
}

fn seq_print(s: &[Move]) {
  s.iter().for_each(|&m| print!("{}", char::from(m)))
}

fn seq_rect(s: &[Move]) -> usize {
  let mut ds = DirState::default();
  s.iter().for_each(|&m| ds.next(m));
  assert_eq!(ds.norths, ds.souths);
  assert_eq!(ds.easts, ds.wests);
  assert_eq!(ds.vert, 0);
  assert_eq!(ds.horiz, 0);
  ds.norths * ds.easts
}

fn l1(file: &str) {
  println!("=== {}", file);
  let seq = expand_seq(read_file(file).split_whitespace());
  print!("Seq: ");
  seq_print(&seq);
  println!();
  println!("D = {}", seq_dist(&seq));
  println!();
}

fn l2(file: &str) {
  println!("=== {}", file);
  let seq = expand_seq(read_file(file).split_whitespace());
  print!("Seq: ");
  seq_print(&seq);
  println!();
  println!("D = {} | RA = {}", seq_dist(&seq), seq_rect(&seq));
  println!();
}

fn read_file(path: &str) -> String {
  use ::std::io::Read;
  let mut f = ::std::fs::File::open(path).unwrap();
  let mut buf = String::new();
  f.read_to_string(&mut buf).unwrap();
  buf
}

fn main() {
  l1("data/l1-test.txt");
  l1("data/l1-0.txt");
  l1("data/l1-1.txt");
  l1("data/l1-2.txt");
  l1("data/l1-3.txt");
  l1("data/l1-4.txt");

  l2("data/l1-test.txt");
}
