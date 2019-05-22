#![feature(tool_lints)]
#![warn(clippy::all)]

use std::cell::RefCell;
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

use std::iter::DoubleEndedIterator as DEIterator;

impl Move {
  fn invert(self) -> Self {
    match self {
      Move::North => Move::South,
      Move::South => Move::North,
      Move::East  => Move::West,
      Move::West  => Move::East,
    }
  }

  fn invert_iter(iter: impl DEIterator<Item = Self>) -> impl Iterator<Item = Self> {
    iter.map(|e| e.invert()).rev()
  }
}

#[derive(Clone, Copy)]
enum Conn { Connected = 1, Connectable = 2, NonConnectable = 3 }

impl From<Conn> for u8 {
  fn from(c: Conn) -> u8 {
    match c {
      Conn::Connected => 1,
      Conn::Connectable => 2,
      Conn::NonConnectable => 3,
    }
  }
}

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
struct Location {
  location: usize,
  width: usize,
  height: usize,
}

impl Location {
  fn next_valid_locations(self, board: &Test, c: Color) -> Vec<(Move, Location)> {
    let mut res = Vec::new();

    if let Some(location) = self.north() {
      if let Some(color) = *board.board[location].borrow() {
        if color == c {
          res.push((Move::North, location));
        }
      } else {
        res.push((Move::North, location));
      }
    }

    if let Some(location) = self.east() {
      if let Some(color) = *board.board[location].borrow() {
        if color == c {
          res.push((Move::East, location));
        }
      } else {
        res.push((Move::East, location));
      }
    }

    if let Some(location) = self.south() {
      if let Some(color) = *board.board[location].borrow() {
        if color == c {
          res.push((Move::South, location));
        }
      } else {
        res.push((Move::South, location));
      }
    }

    if let Some(location) = self.west() {
      if let Some(color) = *board.board[location].borrow() {
        if color == c {
          res.push((Move::West, location));
        }
      } else {
        res.push((Move::West, location));
      }
    }

    res
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

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
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

impl Index<Location> for Vec<RefCell<Cell>> {
  type Output = RefCell<Cell>;

  fn index(&self, l: Location) -> &Self::Output {
    &self[l.location - 1]
  }
}

impl IndexMut<Location> for Vec<RefCell<Cell>> {
  fn index_mut(&mut self, l: Location) -> &mut Self::Output {
    &mut self[l.location - 1]
  }
}

impl Index<Location> for [RefCell<Cell>] {
  type Output = RefCell<Cell>;

  fn index(&self, l: Location) -> &Self::Output {
    &self[l.location - 1]
  }
}

impl IndexMut<Location> for [RefCell<Cell>] {
  fn index_mut(&mut self, l: Location) -> &mut Self::Output {
    &mut self[l.location - 1]
  }
}

type Cell = Option<Color>;

struct Test {
  width: usize,
  height: usize,
  board: Vec<RefCell<Cell>>,
  explicit_paths: Vec<Color>,
  explicit_points: Vec<Location>,
}

impl Test {
  fn discover(&self,
              start_location: Location,
              location: Location,
              color: Color,
              history: &mut Vec<Location>) -> bool
  {
    if location == start_location {
      return false;
    }

    if let Some(cell_color) = *self.board[location].borrow() {
      return cell_color == color;
    }

    if history.contains(&location) {
      return false;
    }

    history.push(location);

    if let Some(p) = Test::mov(location, Move::North) {
      if self.discover(start_location, p, color, history) {
        return true;
      }
    }

    if let Some(p) = Test::mov(location, Move::East) {
      if self.discover(start_location, p, color, history) {
        return true;
      }
    }

    if let Some(p) = Test::mov(location, Move::South) {
      if self.discover(start_location, p, color, history) {
        return true;
      }
    }

    if let Some(p) = Test::mov(location, Move::West) {
      if self.discover(start_location, p, color, history) {
        return true;
      }
    }

    false
  }

  fn connectables(&self) -> Vec<u8> {
    let mut res: Vec<u8> = Vec::new();
    res.resize(self.explicit_points.len() / 2, 0);

    let mut seen_colors: Vec<Color> = Vec::new();

    for point in &self.explicit_points {
      let color: Color = (*self.board[*point].borrow()).unwrap();

      // println!("Start at point {} with color {}:", point.0, color.0);

      Test::print_board(PRINT_BOARDS, &self.board, self.width, Some(*point));

      if seen_colors.contains(&color) {
        // println!("Point {} with color {}: Color seen before (SKIP)", point.0, color.0);
        continue;
      }

      seen_colors.push(color);

      let mut has_path = false;

      for path in &self.explicit_paths {
        if &color == path {
          res[color.0 - 1] = u8::from(Conn::Connected);
          has_path = true;
          break;
        }
      }

      if has_path {
        // println!("Point {} with color {}: Already connected (SKIP)", point.0, color.0);
        continue;
      }

      let mut history: Vec<Location> = Vec::new();

      history.push(*point);

      if let Some(p) = Test::mov(*point, Move::North) {
        if self.discover(*point, p, color, &mut history) {
          res[color.0 - 1] = u8::from(Conn::Connectable);
          continue;
        }
      }

      if let Some(p) = Test::mov(*point, Move::East) {
        if self.discover(*point, p, color, &mut history) {
          res[color.0 - 1] = u8::from(Conn::Connectable);
          continue;
        }
      }

      if let Some(p) = Test::mov(*point, Move::South) {
        if self.discover(*point, p, color, &mut history) {
          res[color.0 - 1] = u8::from(Conn::Connectable);
          continue;
        }
      }

      if let Some(p) = Test::mov(*point, Move::West) {
        if self.discover(*point, p, color, &mut history) {
          res[color.0 - 1] = u8::from(Conn::Connectable);
          continue;
        }
      }

      res[color.0 - 1] = u8::from(Conn::NonConnectable);
    }

    res
  }

  fn mov(l: Location, m: Move) -> Option<Location> {
    match m {
      Move::North => l.north(),
      Move::East  => l.east(),
      Move::South => l.south(),
      Move::West  => l.west(),
    }
  }

  fn print_board(really_print: bool,
                 board: &[RefCell<Cell>],
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

        if let Some(color) = *cell.borrow() {
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
    let mut board: Vec<RefCell<Cell>> = Vec::new();

    board.resize(width * height, RefCell::new(None));

    // println!("Width = {}, Height = {}, {} points", width, height, npoints);

    for _i in 0 .. npoints {
      let location: Location = Location {
        location: usize::from_str(iter.next().unwrap()).unwrap(),
        width: width,
        height: height,
      };

      let color: Color = Color::from(usize::from_str(iter.next().unwrap()).unwrap());
      board[location] = RefCell::new(Some(color));

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

      assert_eq!(*board[start_location].borrow(), Some(color));

      let mut current_location: Location = start_location;

      for j in 0 .. nmoves {
        let m = Move::from(char::from_str(iter.next().unwrap()).unwrap());
        current_location = Test::mov(current_location, m).unwrap();

        if j == nmoves - 1 {
          assert_eq!(*board[current_location].borrow(), Some(color));
        } else {
          board[current_location] = RefCell::new(Some(color));
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

  fn connectables(&self) -> Vec<u8> {
    let mut res: Vec<u8> = Vec::new();

    for test in &self.0 {
      res.extend(test.connectables().iter());
    }

    res
  }
}

fn l5(file: &str) {
  let data = read_file(file);
  // print!("{} contents: {}", file, data);

  let mut iter = data.split_whitespace();
  let tests = Tests::parse(&mut iter);
  let res = tests.connectables();

  print!("{} ({} tests):", file, tests.0.len());
  for r in res {
    print!(" {}", r);
  }
  println!();
}

struct Path {
  color: Color,
  starting_location: Location,
  moves: Vec<Move>,
}

fn partial_path(test_num: usize,
                location: Location,
                board: &Test,
                path: &mut Path) -> bool
{
  let next_valid_locs = location.next_valid_locations(board, path.color);

  if next_valid_locs.len() == 1 {
    let (m, l) = (next_valid_locs[0].0, next_valid_locs[0].1);

    path.moves.push(m);

    if let Some(c) = *board.board[l].borrow() {
      assert_eq!(c, path.color);
      return false;
    }

    let cell: &RefCell<Cell> = &board.board[l];
    *cell.borrow_mut() = Some(path.color);

    return partial_path(test_num, l, board, path);
  }

  true
}

fn l6(file: &str) {
  let data = read_file(file);
  // println!("{} contents: {}", file, data);

  let mut iter = data.split_whitespace();
  let tests = Tests::parse(&mut iter);

  let mut test_results: Vec<Vec<Path>> = Vec::with_capacity(tests.0.len());

  for (t, mut test) in tests.0.iter().enumerate() {
    let mut finished_colors: Vec<Color> =
      Vec::with_capacity(test.explicit_points.len() / 2);

    if t == 20 {
      Test::print_board(true, &test.board, test.width, None);
    }

    let mut paths: Vec<Path> = Vec::new();

    for point in &test.explicit_points {
      let color: Color = (*test.board[*point].borrow()).unwrap();

      if finished_colors.contains(&color) {
        continue;
      }

      let mut path = Path {
        color: color,
        starting_location: *point,
        moves: Vec::new(),
      };

      let is_partial_path = partial_path(t, *point, &test, &mut path);

      if !is_partial_path {
        paths.push(path);
        finished_colors.push(color);
      } else if !path.moves.is_empty() {
        paths.push(path);
      }
    }

    test_results.push(paths);
  }

  for test_res in &mut test_results {
  }

  print!("{}: {}", file, test_results.len());

  for (i, res) in test_results.into_iter().enumerate() {
    print!(" {}", res.len());

    for path in res {
      print!(" {} {} {}",
             path.color.0,
             path.starting_location.location,
             path.moves.len());

      for m in path.moves {
        print!(" {}", char::from(m));
      }
    }
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

  l6("data/level6-1.in");
  // l6("data/level6-2.in");
  // l6("data/level6-3.in");
  // l6("data/level6-4.in");
  // l6("data/level6-5.in");

  // println!("--------------");
}
