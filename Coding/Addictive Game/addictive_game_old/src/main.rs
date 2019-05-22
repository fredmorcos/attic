#![feature(tool_lints)]
#![warn(clippy::all)]

use std::str::FromStr;
use std::fmt::Display;

#[derive(PartialEq, Eq, Clone, Copy)]
enum Connectivity { Connected, Connectable, Unconnectable }

impl From<Connectivity> for u8 {
  fn from(c: Connectivity) -> u8 {
    match c {
      Connectivity::Connected => 1,
      Connectivity::Connectable => 2,
      Connectivity::Unconnectable => 3,
    }
  }
}

#[derive(Clone, Copy, PartialEq, Eq)]
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

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
struct Location {
  row: i64,
  col: i64,
}

impl Location {
  fn new(pos: u32, cols: u32) -> Location {
    Location {
      row: i64::from(((pos - 1) / cols) + 1),
      col: i64::from(((pos - 1) % cols) + 1),
    }
  }

  fn reloc(&mut self, m: Move, rows: u32, cols: u32) -> bool {
    match m {
      Move::North => if self.row == 1 {
        return false;
      } else {
        self.row -= 1;
      },
      Move::East => if self.col == i64::from(cols) {
        return false;
      } else {
        self.col += 1;
      },
      Move::South => if self.row == i64::from(rows) {
        return false;
      } else {
        self.row += 1;
      },
      Move::West => if self.col == 1 {
        return false;
      } else {
        self.col -= 1;
      },
    }

    true
  }

  fn position(&self, cols: u32) -> u32 {
    ((((self.row - 1) * i64::from(cols)) + (self.col - 1)) + 1) as u32
  }

  fn index(&self, cols: u32) -> u32 {
    self.position(cols) - 1
  }
}

mod location_tests {
  #[test]
  fn test() {
    let loc = ::Location { row: 4, col: 3 };
    assert_eq!(::Location::new(15, 4), loc);
    assert_eq!(loc.position(4), 15);
    let loc = ::Location { row: 6, col: 4 };
    assert_eq!(::Location::new(24, 4), loc);
    assert_eq!(loc.position(4), 24);
  }
}

#[derive(Clone)]
struct Line {
  color: usize,
  start_loc: Location,
  n_moves: usize,
  moves: Vec<Move>,
  locations: Vec<Location>,
}

impl Line {
  fn new(color: usize, start_loc: Location, n_moves: usize, moves: Vec<Move>) -> Line {
    Line { color, start_loc, n_moves, moves, locations: Vec::with_capacity(n_moves + 2) }
  }

  fn crosses(&self, other: &Self) -> bool {
    for l1 in &self.locations {
      for l2 in &other.locations {
        if l1 == l2 {
          return true;
        }
      }
    }

    false
  }

  fn parse<'a, I: Iterator<Item = &'a str>>(iter: &mut I, cols: u32) -> Line {
    let color = usize::from_str(iter.next().unwrap()).unwrap();

    let start_loc = Location::new(u32::from_str(iter.next().unwrap()).unwrap(), cols);
    let n_moves = usize::from_str(iter.next().unwrap()).unwrap();
    let mut moves = Vec::with_capacity(n_moves);

    for _j in 0 .. n_moves {
      let m = Move::from(char::from_str(iter.next().unwrap()).unwrap());
      moves.push(m);
    }

    assert_eq!(n_moves, moves.len());

    Line::new(color, start_loc, n_moves, moves)
  }
}

struct Board {
  rows: u32,
  cols: u32,
  n_points: usize,
  points: Vec<(Location, usize)>,
  points_by_color: Vec<(Location, Location)>,
  n_lines: usize,
  lines: Vec<Line>,
  valid_lines: Vec<Line>,
  board: Vec<Option<usize>>,
}

impl Board {
  fn new(rows: u32, cols: u32,
         n_points: usize, points: Vec<(Location, usize)>,
         points_by_color: Vec<(Location, Location)>,
         n_lines: usize, lines: Vec<Line>) -> Board
  {
    let mut board: Vec<Option<usize>> = Vec::with_capacity((rows * cols) as usize);
    board.resize((rows * cols) as usize, None);
    let valid_lines = Vec::with_capacity(n_lines);
    Board {
      rows, cols,
      n_points, points,
      points_by_color,
      n_lines, lines, valid_lines,
      board,
    }
  }

  fn render(&mut self) {
    for (loc, col) in &self.points {
      self.board[loc.index(self.cols) as usize] = Some(*col);
    }

    for line in &self.valid_lines {
      for loc in &line.locations {
        self.board[loc.index(self.cols) as usize] = Some(line.color);
      }
    }
  }

  fn parse<'a, I: Iterator<Item = &'a str>>(iter: &mut I) -> Board {
    let rows = u32::from_str(iter.next().unwrap()).unwrap();
    let cols = u32::from_str(iter.next().unwrap()).unwrap();

    let n_points = usize::from_str(iter.next().unwrap()).unwrap();
    let mut points = Vec::with_capacity(n_points);

    for _i in 0 .. n_points {
      let loc = Location::new(u32::from_str(iter.next().unwrap()).unwrap(), cols);
      let color = usize::from_str(iter.next().unwrap()).unwrap();
      assert!(color <= n_points / 2);
      points.push((loc, color));
    }

    let mut points_by_color = Vec::with_capacity(n_points / 2);

    for col in 0 .. n_points / 2 {
      let pair: Vec<Location> = points.iter()
                                      .filter(|(_, color)| *color == col + 1)
                                      .map(|(loc, _)| *loc)
                                      .collect();

      assert_eq!(pair.len(), 2);

      points_by_color.push((pair[0], pair[1]));
    }

    assert_eq!(points_by_color.len(), n_points / 2);

    let n_lines = usize::from_str(iter.next().unwrap()).unwrap();
    let mut lines = Vec::with_capacity(n_lines);

    for _i in 0 .. n_lines {
      lines.push(Line::parse(iter, cols));
    }

    Board::new(rows, cols, n_points, points, points_by_color, n_lines, lines)
  }
}

impl Display for Board {
  fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
    for (i, e) in self.board.iter().enumerate() {
      if e.is_some() {
        write!(f, "#")?;
      } else {
        write!(f, " ")?;
      }

      if (i + 1) as u32 % self.cols == 0 {
        writeln!(f)?;
      }
    }

    Ok(())
  }
}

struct Boards {
  n_boards: usize,
  boards: Vec<Board>,
}

impl Boards {
  fn parse<'a, I: Iterator<Item = &'a str>>(iter: &mut I) -> Boards {
    let n_boards = usize::from_str(iter.next().unwrap()).unwrap();
    let mut boards = Vec::with_capacity(n_boards);

    for _i in 0 .. n_boards {
      boards.push(Board::parse(iter))
    }

    assert!(iter.next().is_none());

    Boards { n_boards, boards }
  }
}

fn addictive_game_read(path: &str) -> (u32, u32, u32, Vec<u32>) {
  let data = read_file(path);
  let mut iter = data.split_whitespace();

  let rows = iter.next().unwrap();
  let cols = iter.next().unwrap();
  let num = iter.next().unwrap();

  let rows = u32::from_str(rows).unwrap();
  let cols = u32::from_str(cols).unwrap();
  let num = u32::from_str(num).unwrap();

  let vals: Vec<u32> = iter.map(|e| u32::from_str(e).unwrap()).collect();

  (rows, cols, num, vals)
}

fn addictive_game_l1(path: &str) {
  let (_, cols, _, vals) = addictive_game_read(path);

  let mut res = Vec::new();

  for pos in vals {
    let loc = Location::new(pos, cols);
    res.push(loc.row);
    res.push(loc.col);
  }

  print!("{}:", path);

  for r in res {
    print!(" {}", r);
  }

  println!();
}

fn addictive_game_l2(path: &str) {
  let (_, cols, _, vals) = addictive_game_read(path);

  let mut colors = Vec::new();
  let mut iter = vals.into_iter();

  while let Some(pos) = iter.next() {
    let loc = Location::new(pos, cols);

    if let Some(color) = iter.next() {
      if color as usize > colors.len() {
        colors.resize(color as usize, Vec::new());
      }

      colors[(color - 1) as usize].push(loc);
    } else {
      panic!("Invalid input");
    }
  }

  print!("{}:", path);

  for c in colors {
    assert_eq!(c.len(), 2);

    let src = c[0];
    let dst = c[1];

    let dist =
      i64::abs(src.row - dst.row) +
      i64::abs(src.col - dst.col);

    print!(" {}", dist);
  }

  println!();
}

fn addictive_game_l3(path: &str) {
  let data = read_file(path);
  let mut iter = data.split_whitespace();

  let mut board = Board::parse(&mut iter);

  print!("{}:", path);

  for line in &mut board.lines {
    let mut cur_loc: Location = line.start_loc;

    for (i, m) in line.moves.iter().enumerate() {
      line.locations.push(cur_loc);

      if !cur_loc.reloc(*m, board.rows, board.cols) {
        print!(" -1 {}", i + 1);
        break;
      }

      if line.locations.contains(&cur_loc) {
        print!(" -1 {}", i + 1);
        break;
      }

      let mut leave = false;

      for (color, points) in board.points_by_color.iter().enumerate() {
        if color + 1 != line.color && (cur_loc == points.0 || cur_loc == points.1) {
          print!(" -1 {}", i + 1);
          leave = true;
          break;
        }
      }

      if leave {
        break;
      }
    }

    if
      (board.points_by_color[line.color - 1].0 == line.start_loc &&
       board.points_by_color[line.color - 1].1 == cur_loc) ||
      (board.points_by_color[line.color - 1].1 == line.start_loc &&
       board.points_by_color[line.color - 1].0 == cur_loc)
    {
      print!(" 1 {}", line.n_moves);
    } else {
      print!(" -1 {}", line.n_moves);
    }
  }

  println!();
}

fn addictive_game_l4(path: &str) {
  let data = read_file(path);
  let mut iter = data.split_whitespace();

  let mut board = Board::parse(&mut iter);

  println!("{}:", path);

  for (line_num, ref mut line) in board.lines.iter_mut().enumerate() {
    let mut cur_loc: Location = line.start_loc;
    let mut valid = true;

    for m in &line.moves {
      line.locations.push(cur_loc);

      if !cur_loc.reloc(*m, board.rows, board.cols) {
        valid = false;
        break;
      }

      if line.locations.contains(&cur_loc) {
        valid = false;
        break;
      }

      for (color, points) in board.points_by_color.iter().enumerate() {
        if color + 1 != line.color && (cur_loc == points.0 || cur_loc == points.1) {
          valid = false;
          break;
        }
      }

      if !valid {
        break;
      }
    }

    if valid {
      for other_line in &board.valid_lines {
        if line.crosses(other_line) {
          valid = false;
          break;
        }
      }
    }

    if valid &&
      ((board.points_by_color[line.color - 1].0 == line.start_loc &&
        board.points_by_color[line.color - 1].1 == cur_loc) ||
       (board.points_by_color[line.color - 1].1 == line.start_loc &&
        board.points_by_color[line.color - 1].0 == cur_loc))
    {
      board.valid_lines.push(line.clone());
    }

    if line_num % 10000 == 0 {
      println!("Processed line {}/{}", line_num + 1, board.n_lines);
    }
  }

  board.render();
  println!("{}", board);
}

fn addictive_game_l5(path: &str) {
  let data = read_file(path);
  let mut iter = data.split_whitespace();

  let mut boards: Boards = Boards::parse(&mut iter);

  let mut color_history: Vec<usize> = Vec::new();
  let mut res: Vec<u8> = Vec::new();

  fn connectable(m: Move, orig_loc: Location, mut loc: Location,
                 color: usize, board: &Board, vis: &mut Vec<Location>) -> bool
  {
    if !loc.reloc(m, board.rows, board.cols) {
      return false;
    }

    if vis.contains(&loc) {
      return false;
    }

    vis.push(loc);

    if loc == orig_loc {
      return false;
    }

    for line in &board.lines {
      for line_loc in &line.locations {
        if line_loc == &loc {
          return false;
        }
      }
    }

    for (point_location, point_color) in &board.points {
      if point_location == &orig_loc {
        continue;
      }

      if point_location == &loc {
        return point_color == &color;
      }
    }

    if connectable(Move::North, orig_loc, loc, color, board, vis) {
      return true;
    }

    if connectable(Move::East, orig_loc, loc, color, board, vis) {
      return true;
    }

    if connectable(Move::South, orig_loc, loc, color, board, vis) {
      return true;
    }

    if connectable(Move::West, orig_loc, loc, color, board, vis) {
      return true;
    }

    false
  }

  for board in &mut boards.boards {
    for line in &mut board.lines {
      let mut cur_loc: Location = line.start_loc;

      for m in &line.moves {
        line.locations.push(cur_loc);
        assert_eq!(cur_loc.reloc(*m, board.rows, board.cols), true);
      }
    }

    for (location, color) in &board.points {
      if color_history.contains(&color) {
        continue;
      }

      // if explicitly connected by a line
      if board.lines.iter().any(|l| l.color == *color) {
        res.push(u8::from(Connectivity::Connected));
        color_history.push(*color);
        continue;
      }

      let mut vis = Vec::new();

      if
        connectable(Move::North, *location, *location, *color, board, &mut vis) ||
        connectable(Move::East, *location, *location, *color, board, &mut vis)  ||
        connectable(Move::South, *location, *location, *color, board, &mut vis) ||
        connectable(Move::West, *location, *location, *color, board, &mut vis)
      {
        res.push(u8::from(Connectivity::Connectable));
        color_history.push(*color);
        continue;
      }

      res.push(u8::from(Connectivity::Unconnectable));
      color_history.push(*color);
    }
  }

  print!("{} ({} tests):", path, boards.n_boards);

  for r in res {
    print!(" {}", r);
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
  addictive_game_l1("data/level1-0.in");
  addictive_game_l1("data/level1-1.in");
  addictive_game_l1("data/level1-2.in");
  addictive_game_l1("data/level1-3.in");

  println!("--------------");

  addictive_game_l2("data/level2-0.in");
  addictive_game_l2("data/level2-1.in");
  addictive_game_l2("data/level2-2.in");
  addictive_game_l2("data/level2-3.in");

  println!("--------------");

  addictive_game_l3("data/level3-0.in");
  addictive_game_l3("data/level3-01.in");
  addictive_game_l3("data/level3-02.in");
  addictive_game_l3("data/level3-03.in");
  addictive_game_l3("data/level3-04.in");
  addictive_game_l3("data/level3-1.in");
  addictive_game_l3("data/level3-2.in");
  addictive_game_l3("data/level3-3.in");
  addictive_game_l3("data/level3-4.in");
  addictive_game_l3("data/level3-5.in");
  addictive_game_l3("data/level3-6.in");
  addictive_game_l3("data/level3-7.in");

  println!("--------------");

  // addictive_game_l4("data/level4-0.in");
  // addictive_game_l4("data/level4-1.in");
  // addictive_game_l4("data/level4-2.in");
  // addictive_game_l4("data/level4-3.in");
  // addictive_game_l4("data/level4-4.in");
  // addictive_game_l4("data/level4-5.in");

  println!("--------------");

  addictive_game_l5("data/level5-0.in");
  addictive_game_l5("data/level5-1.in");
  addictive_game_l5("data/level5-2.in");
  addictive_game_l5("data/level5-3.in");
  addictive_game_l5("data/level5-4.in");

  println!("--------------");
}
