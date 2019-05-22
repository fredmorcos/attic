#![feature(tool_lints)]
#![warn(clippy::all)]

use std::str::FromStr;
use std::ops::Index;
use std::ops::IndexMut;

static PRINT_BOARDS: bool = false;

#[derive(Clone, Copy, Debug)]
enum Move { North, East, South, West }

impl From<char> for Move {
  fn from(c: char) -> Move {
    match c {
      'N' => Move::North,
      'E' => Move::East,
      'S' => Move::South,
      'W' => Move::West,
      _   => panic!("Invalid direction"),
    }
  }
}

impl From<Move> for char {
  fn from(m: Move) -> char {
    match m {
      Move::North => 'N',
      Move::East  => 'E',
      Move::South => 'S',
      Move::West  => 'W',
    }
  }
}

impl Move {
  fn invert(self) -> Self {
    match self {
      Move::North => Move::South,
      Move::South => Move::North,
      Move::East  => Move::West,
      Move::West  => Move::East,
    }
  }
}

#[derive(PartialOrd, Ord, PartialEq, Eq, Debug, Clone, Copy)]
struct Location {
  location: usize,
  width: usize,
  height: usize,
}

impl Location {
  fn next_valid_locations(self) -> Vec<(Move, Location)> {
    [(Move::North, self.north()),
     (Move::East,  self.east()),
     (Move::South, self.south()),
     (Move::West,  self.west())]
      .into_iter()
      .filter(|(_, l)| l.is_some())
      .map(|&(m, l)| (m, l.unwrap()))
      .collect()
  }

  fn north(self) -> Option<Self> {
    if self.location <= self.width {
      None
    } else {
      Some(Location {
        location: self.location - self.width,
        width: self.width,
        height: self.height,
      })
    }
  }

  fn east(self) -> Option<Self> {
    if self.location % self.width == 0 {
      None
    } else {
      Some(Location {
        location: self.location + 1,
        width: self.width,
        height: self.height,
      })
    }
  }

  fn south(self) -> Option<Self> {
    if self.location + self.width > self.width * self.height {
      None
    } else {
      Some(Location {
        location: self.location + self.width,
        width: self.width,
        height: self.height,
      })
    }
  }

  fn west(self) -> Option<Self> {
    if self.location % self.width == 1 {
      None
    } else {
      Some(Location {
        location: self.location - 1,
        width: self.width,
        height: self.height,
      })
    }
  }
}

impl Index<Location> for Vec<Cell> {
  type Output = Cell;

  fn index(&self, l: Location) -> &Self::Output {
    &self[l.location - 1]
  }
}

impl IndexMut<Location> for Vec<Cell> {
  fn index_mut(&mut self, l: Location) -> &mut Self::Output {
    &mut self[l.location - 1]
  }
}

impl Index<Location> for [Cell] {
  type Output = Cell;

  fn index(&self, l: Location) -> &Self::Output {
    &self[l.location - 1]
  }
}

impl IndexMut<Location> for [Cell] {
  fn index_mut(&mut self, l: Location) -> &mut Self::Output {
    &mut self[l.location - 1]
  }
}

#[derive(PartialOrd, Ord, PartialEq, Eq, Debug, Clone, Copy)]
struct Color(usize);

impl From<usize> for Color {
  fn from(c: usize) -> Self {
    Color(c)
  }
}

impl From<Color> for usize {
  fn from(c: Color) -> Self {
    c.0
  }
}

type Cell = Option<Color>;

struct Test {
  width: usize,
  height: usize,
  board: Vec<Cell>,
  explicit_paths: Vec<Color>,
  explicit_points: Vec<Location>,
}

impl Test {
  fn mov(l: Location, m: Move) -> Option<Location> {
    match m {
      Move::North => l.north(),
      Move::East  => l.east(),
      Move::South => l.south(),
      Move::West  => l.west(),
    }
  }

  fn print_board(really_print: bool,
                 board: &[Cell],
                 width: usize,
                 loc: Option<Location>)
  {
    if really_print {
      let mut newline = true;

      for _ in 0 .. (width * 4) + 3 {
        print!("-");
      }

      println!();

      for (i, cell) in board.iter().enumerate() {
        if newline {
          print!("|");
          newline = false;
        }

        if let Some(color) = cell {
          print!(" {:2}", color.0);
        } else {
          print!("  .");
        }

        if let Some(loc) = loc {
          if i + 1 == loc.location {
            print!("*");
          } else {
            print!(" ");
          }
        } else {
          print!(" ");
        }

        if (i + 1) % width == 0 {
          println!(" |");
          newline = true;
        }
      }

      for _ in 0 .. (width * 4) + 3 {
        print!("-");
      }

      println!();
    }
  }

  fn parse<'a, I: Iterator<Item = &'a str>>(iter: &mut I) -> Test {
    let height: usize = usize::from_str(iter.next().unwrap()).unwrap();
    let width: usize = usize::from_str(iter.next().unwrap()).unwrap();
    let npoints: usize = usize::from_str(iter.next().unwrap()).unwrap();
    let mut explicit_points: Vec<Location> = Vec::with_capacity(npoints);
    let mut board: Vec<Cell> = Vec::new();

    board.resize(width * height, None);

    // println!("Width = {}, Height = {}, {} points", width, height, npoints);

    for _i in 0 .. npoints {
      let location: Location = Location {
        location: usize::from_str(iter.next().unwrap()).unwrap(),
        width: width,
        height: height,
      };

      let color: Color = Color::from(usize::from_str(iter.next().unwrap()).unwrap());
      board[location] = Some(color);

      explicit_points.push(location);
      // println!("Point at location {} with color {}", location.0, color.0);
    }

    // Test::print_board(&board, width, height, None);

    let mut explicit_paths: Vec<Color> = Vec::new();
    let npaths: usize = usize::from_str(iter.next().unwrap()).unwrap();

    for _i in 0 .. npaths {
      let color: Color = Color::from(usize::from_str(iter.next().unwrap()).unwrap());

      let start_location: Location = Location {
        location: usize::from_str(iter.next().unwrap()).unwrap(),
        width: width,
        height: height,
      };

      let nmoves: usize = usize::from_str(iter.next().unwrap()).unwrap();

      assert_eq!(board[start_location], Some(color));

      let mut current_location: Location = start_location;

      for j in 0 .. nmoves {
        let m = Move::from(char::from_str(iter.next().unwrap()).unwrap());
        current_location = Test::mov(current_location, m).unwrap();

        if j == nmoves - 1 {
          assert_eq!(board[current_location], Some(color));
        } else {
          board[current_location] = Some(color);
        }
      }

      explicit_paths.push(color);
    }

    Test::print_board(PRINT_BOARDS, &board, width, None);

    Test { width, height, board, explicit_paths, explicit_points }
  }
}

struct Tests(Vec<Test>);

impl Tests {
  fn parse<'a, I: Iterator<Item = &'a str>>(iter: &mut I) -> Tests {
    let ntests: usize = usize::from_str(iter.next().unwrap()).unwrap();
    let mut tests: Vec<Test> = Vec::with_capacity(ntests);

    for i in 0 .. ntests {
      if PRINT_BOARDS {
        println!("Test {}", i);
      }
      tests.push(Test::parse(iter));
    }

    assert_eq!(iter.next(), None);

    Tests(tests)
  }
}

#[derive(Clone)]
struct Path {
  color: Color,
  start: Location,
  end: Location,
  hist: Vec<Location>,
  moves: Vec<Move>,
}

fn partial_path(t: usize, board: &mut Test, path: &mut Path, finished: &mut Vec<Color>) -> bool {
  if finished.contains(&path.color) {
    return false;
  }

  let next: Vec<(Move, Location)> = path.end.next_valid_locations();
  let next: Vec<(Move, Location)> = next.into_iter()
                                        .filter(|&(_, l)| {
                                          if board.board[l].is_none() {
                                            true
                                          } else if let Some(c) = board.board[l] {
                                            if c == path.color {
                                              !path.hist.contains(&l)
                                            } else {
                                              false
                                            }
                                          } else {
                                            false
                                          }
                                        })
                                        .collect();

  // if t == 20 && path.color.0 == 4 {
  //   println!("==> Color {} at location {} has {} possible moves",
  //            path.color.0, path.end.location, next.len());
  // }

  if next.len() == 1 {
    let (m, l) = (next[0].0, next[0].1);

    path.moves.push(m);
    path.end = l;
    path.hist.push(l);

    if let Some(c) = board.board[l] {
      assert_eq!(c, path.color);
      finished.push(c);
    } else {
      board.board[l] = Some(path.color);
      partial_path(t, board, path, finished);
    }

    return true;
  }

  false
}

fn points_to_paths(points: &[Location], board: &Test) -> Vec<Path> {
  points.iter().map(|&l| Path {
    color: (board.board[l]).unwrap(),
    start: l,
    end: l,
    hist: vec![l],
    moves: Vec::new(),
  }).collect()
}

fn l6(file: &str) {
  let data = read_file(file);
  // println!("{} contents: {}", file, data);

  let mut iter = data.split_whitespace();
  let mut tests = Tests::parse(&mut iter);

  let mut test_results: Vec<Vec<Path>> = Vec::with_capacity(tests.0.len());

  for (t, mut test) in tests.0.iter_mut().enumerate() {
    let mut paths: Vec<Path> = points_to_paths(&test.explicit_points, test);
    let mut finished: Vec<Color> = Vec::new();
    let mut keep_going: bool = true;

    while keep_going {
      keep_going = false;

      for mut path in &mut paths {
        keep_going |= partial_path(t, &mut test, &mut path, &mut finished);
      }
    }

    // Delete empty paths
    let mut final_paths: Vec<Path> = Vec::new();
    let mut merged: Vec<Color> = Vec::new();

    for path in &paths {
      if merged.contains(&path.color) {
        continue;
      }

      if !finished.contains(&path.color) {
        final_paths.push(path.clone());
        continue;
      }

      let mut final_path: Path = path.clone();

      let other: &Path = paths.iter().find(|p| {
        p.start != final_path.start && p.color == final_path.color
      }).unwrap();

      assert_eq!(final_path.end, other.end);

      final_path.end = other.start;

      for m in other.moves.iter().rev() {
        final_path.moves.push(m.invert());
      }

      merged.push(path.color);
      final_paths.push(final_path);
    }

    final_paths.sort_unstable_by(|a, b| a.color.0.cmp(&b.color.0));

    test_results.push(final_paths);
  }

  print!("{}: {}", file, test_results.len());

  for (t, res) in test_results.iter().enumerate() {
    // if t == 20 {
    //   println!();
    //   println!();
    //   Test::print_board(true, &tests.0[t].board, tests.0[t].width, None);
    // }

    Test::print_board(true, &tests.0[t].board, tests.0[t].width, None);

    print!(" {}", res.iter().filter(|p| !p.moves.is_empty()).count());

    for path in res {
      // println!("Path color:{} start:{} end:{} with {} moves",
      //          path.color.0, path.start.location, path.end.location, path.moves.len());

      if !path.moves.is_empty() {
        print!(" {} {} {}",
               path.color.0,
               path.start.location,
               path.moves.len());

        for m in &path.moves {
          print!(" {}", char::from(*m));
        }
      }
    }

    // if t == 20 {
    //   println!();
    //   println!();
    // }
  }

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
  // l5("data/level5-0.in");
  // l5("data/level5-1.in");
  // l5("data/level5-2.in");
  // l5("data/level5-3.in");
  // l5("data/level5-4.in");

  // println!("--------------");

  // l6("data/level6-1.in");
  // l6("data/level6-2.in");
  // l6("data/level6-3.in");
  // l6("data/level6-4.in");
  // l6("data/level6-5.in");

  // println!("--------------");

  l6("data/level7-1.in");
  l6("data/level7-2.in");
  l6("data/level7-3.in");
}
