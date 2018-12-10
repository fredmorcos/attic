#![warn(clippy::all)]

use std::cmp::Ordering;
use std::fs::{self, File};
use std::io::{BufRead, BufReader};
use std::str::FromStr;
use std::string::ParseError as ParseStringError;
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
    for (j, (c1, c2)) in id1.chars()
                            .zip(id2.chars())
                            .enumerate()
    {
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
  ParseStr(ParseStringError),
  UnexpectedEOF,
  Other(String),
}

impl From<ParseIntError> for ParseError {
  fn from(e: ParseIntError) -> Self {
    ParseError::ParseInt(e)
  }
}

impl From<ParseStringError> for ParseError {
  fn from(e: ParseStringError) -> Self {
    ParseError::ParseStr(e)
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

fn skip_n<I>(iter: &mut Peekable<I>, n: usize)
             -> Result<(), ParseError>
  where I: Iterator<Item = char>,
{
  for _ in 0 .. n {
    if iter.peek().is_some() {
      iter.next();
    } else {
      return Err(ParseError::UnexpectedEOF);
    }
  }

  Ok(())
}

fn single_char<I>(iter: &mut Peekable<I>)
                  -> Result<char, ParseError>
  where I: Iterator<Item = char>,
{
  if let Some(c) = iter.next() {
    Ok(c)
  } else {
    Err(ParseError::UnexpectedEOF)
  }
}

fn single<I>(iter: &mut Peekable<I>, c: char)
             -> Result<(), ParseError>
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

fn multi<T, I, P>(iter: &mut Peekable<I>, p: P)
                  -> Result<T, <T as FromStr>::Err>
  where T: FromStr,
        I: Iterator<Item = char>,
        P: Fn(I::Item) -> bool,
{
  let mut id = String::new();

  while let Some(&c) = iter.peek() {
    // if c.is_digit(10) {
    if p(c) {
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

    let id = multi::<usize, _, _>
      (&mut iter, |c| c.is_digit(10))?;

    skip_ws(&mut iter);
    single(&mut iter, '@')?;
    skip_ws(&mut iter);

    let x = multi::<usize, _, _>
      (&mut iter, |c| c.is_digit(10))?;

    single(&mut iter, ',')?;

    let y = multi::<usize, _, _>
      (&mut iter, |c| c.is_digit(10))?;

    single(&mut iter, ':')?;
    skip_ws(&mut iter);

    let w = multi::<usize, _, _>
      (&mut iter, |c| c.is_digit(10))?;

    single(&mut iter, 'x')?;

    let h = multi::<usize, _, _>
      (&mut iter, |c| c.is_digit(10))?;

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

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Mode { Sleep, Wake }

#[derive(Debug, PartialEq, Eq)]
struct Event {
  year: usize,
  month: usize,
  day: usize,
  hour: usize,
  minute: usize,
  guard: Option<usize>,
  mode: Mode,
}

impl PartialOrd for Event {
  fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
    Some(self.year.partial_cmp(&other.year)?.then(
      self.month.partial_cmp(&other.month)?.then(
        self.day.partial_cmp(&other.day)?.then(
          self.hour.partial_cmp(&other.hour)?.then(
            self.minute.partial_cmp(&other.minute)?)))))
  }
}

impl Ord for Event {
  fn cmp(&self, other: &Self) -> Ordering {
    self.year.cmp(&other.year).then(
      self.month.cmp(&other.month).then(
        self.day.cmp(&other.day).then(
          self.hour.cmp(&other.hour).then(
            self.minute.cmp(&other.minute)))))
  }
}

impl FromStr for Event {
  type Err = ParseError;

  fn from_str(input: &str) -> Result<Self, Self::Err> {
    let mut iter = input.chars().peekable();

    single(&mut iter, '[')?;
    let year = multi(&mut iter, |c| c.is_digit(10))?;
    single(&mut iter, '-')?;
    let month = multi(&mut iter, |c| c.is_digit(10))?;
    single(&mut iter, '-')?;
    let day = multi(&mut iter, |c| c.is_digit(10))?;

    skip_ws(&mut iter);

    let hour = multi(&mut iter, |c| c.is_digit(10))?;
    single(&mut iter, ':')?;
    let minute = multi(&mut iter, |c| c.is_digit(10))?;
    single(&mut iter, ']')?;

    skip_ws(&mut iter);

    let (guard, mode) = if let Some(&c) = iter.peek() {
      if c == 'G' || c == 'g' {
        multi::<String, _, _>(&mut iter, |c| !c.is_whitespace())?;
        skip_ws(&mut iter);
        single(&mut iter, '#')?;

        (Some(multi(&mut iter, |c| c.is_digit(10))?), Mode::Wake)
      } else if c == 'f' || c == 'F' {
        (None, Mode::Sleep)
      } else if c == 'w' || c == 'W' {
        (None, Mode::Wake)
      } else {
        return Err(ParseError::Other(format!("Unexpected char '{}'", c)));
      }
    } else {
      return Err(ParseError::UnexpectedEOF);
    };

    Ok(Event { year, month, day, hour, minute, guard, mode })
  }
}

fn d4_1_2() {
  let mut events: Vec<Event> = read_lines("day4").collect();
  events.sort();

  let mut guard: Option<usize> = None;
  let mut mode: Mode = Mode::Wake;
  let mut sleep_start: usize = 0;
  let mut guards: Map<usize, Map<usize, usize>> = Map::new();

  for ev in events {
    if ev.guard.is_some() {
      assert_eq!(ev.mode, Mode::Wake);
      guard = ev.guard;
      mode = Mode::Wake;
    } else if ev.mode == Mode::Sleep {
      assert!(guard.is_some());
      assert_eq!(mode, Mode::Wake, "Guard: {}", guard.unwrap());
      mode = ev.mode;
      sleep_start = ev.minute;
    } else if ev.mode == Mode::Wake {
      assert!(guard.is_some());
      assert_eq!(mode, Mode::Sleep, "Guard: {}", guard.unwrap());
      mode = ev.mode;

      for minute in sleep_start .. ev.minute {
        let guard = guard.unwrap();

        if let Some(minutes) = guards.get_mut(&guard) {
          if let Some(count) = minutes.get_mut(&minute) {
            *count += 1;
          } else {
            minutes.insert(minute, 1);
          }
        } else {
          let mut minutes = Map::new();
          minutes.insert(minute, 1);
          guards.insert(guard, minutes);
        }
      }
    }
  }

  let mut res_guard = 0;
  let mut res_minute = 0;
  let mut total_minutes = 0;

  for (&guard, minutes) in &guards {
    let t = minutes.values().sum();

    if t > total_minutes {
      res_guard = guard;
      total_minutes = t;
      res_minute = 0;

      let mut minute_count = 0;

      for (&minute, &count) in minutes {
        if count > minute_count {
          res_minute = minute;
          minute_count = count;
        }
      }
    }
  }

  println!("d4_1 ID={} * Minute={} = {}", res_guard, res_minute, res_guard * res_minute);

  let mut res_guard = 0;
  let mut res_minute = 0;
  let mut minute_count = 0;

  for (guard, minutes) in guards {
    for (minute, count) in minutes {
      if count > minute_count {
        res_minute = minute;
        minute_count = count;
        res_guard = guard;
      }
    }
  }

  println!("d4_2 ID={} * Minute={} = {}", res_guard, res_minute, res_guard * res_minute);
}

fn react_polymer(data: &mut Vec<u8>) {
  let mut delete: Vec<usize> = vec![];
  let mut found: bool = false;

  loop {
    let mut skip = false;

    for (i, c) in data[.. data.len() - 1].iter().enumerate() {
      if skip {
        skip = false;
        continue;
      }

      if
        ((c.is_ascii_lowercase() && data[i + 1].is_ascii_uppercase()) ||
         (c.is_ascii_uppercase() && data[i + 1].is_ascii_lowercase())) &&
        c.to_ascii_lowercase() == data[i + 1].to_ascii_lowercase()
      {
        found = true;
        delete.push(i);
        skip = true;
      }
    }

    if !found {
      break;
    }

    for i in delete.iter().rev() {
      data.remove(i + 1);
      data.remove(*i);
    }

    delete.clear();
    found = false;
  }
}

fn find_all_units(data: &[u8]) -> Set<u8> {
  let mut units = Set::new();

  for c in data {
    units.insert(c.to_ascii_lowercase());
  }

  units
}

fn remove_unit(unit: u8, data: &[u8]) -> Vec<u8> {
  data.iter().filter(|&&c| c != unit && c != unit.to_ascii_uppercase())
             .cloned()
             .collect()
}

fn d5_1() {
  let data: String = fs::read_to_string("day5").unwrap();
  let mut data: Vec<u8> = data.trim_end().to_string().into_bytes();

  react_polymer(&mut data);

  println!("d5_1 {}", data.len());
}

fn d5_2() {
  let data: String = fs::read_to_string("day5").unwrap();
  let data: Vec<u8> = data.trim_end().to_string().into_bytes();

  let mut shortest = None;

  for unit in find_all_units(&data) {
    let mut new_data = remove_unit(unit, &data);
    react_polymer(&mut new_data);

    if let Some(ref mut shortest) = shortest {
      if *shortest > new_data.len() {
        *shortest = new_data.len();
      }
    } else {
      shortest = Some(new_data.len());
    }
  }

  println!("d5_2 {:?}", shortest);
}

struct Coord {
  x: usize,
  y: usize,
}

impl FromStr for Coord {
  type Err = ParseError;

  fn from_str(input: &str) -> Result<Self, Self::Err> {
    let mut iter = input.chars().peekable();

    let x = multi::<usize, _, _>(&mut iter, |c| c.is_digit(10))?;

    single(&mut iter, ',')?;
    skip_ws(&mut iter);

    let y = multi::<usize, _, _>(&mut iter, |c| c.is_digit(10))?;

    Ok(Coord { x, y })
  }
}

impl Coord {
  fn dist(&self, x: usize, y: usize) -> usize {
    let dx = self.x.max(x) - self.x.min(x);
    let dy = self.y.max(y) - self.y.min(y);
    dx + dy
  }

  fn minmax(coords: &[Coord]) -> (usize, usize, usize, usize) {
    let mut min_x = coords[0].x;
    let mut max_x = coords[0].x;
    let mut min_y = coords[0].y;
    let mut max_y = coords[0].y;

    for c in coords {
      if c.x < min_x { min_x = c.x; }
      if c.x > max_x { max_x = c.x; }
      if c.y < min_y { min_y = c.y; }
      if c.y > max_y { max_y = c.y; }
    }

    (min_x, max_x, min_y, max_y)
  }
}

fn d6_1() {
  let coords: Vec<Coord> = read_lines::<_, Coord>("day6").collect();
  let (min_x, max_x, min_y, max_y) = Coord::minmax(&coords);

  let mut infinites: Set<usize> = Set::new();
  let mut counts: Vec<usize> = vec![0; coords.len()];

  for j in min_y ..= max_y {
    for i in min_x ..= max_x {
      let mut dists: Vec<(usize, usize)> =
        coords.iter()
              .enumerate()
              .map(|(idx, c)| (idx, c.dist(i, j)))
              .collect();

      dists.sort_by(|a, b| a.1.cmp(&b.1));

      if dists[0].1 != dists[1].1 {
        counts[dists[0].0] += 1;
      }

      if j == 0 || j == max_y || i == 0 || i == max_x {
        infinites.insert(dists[0].0);
      }
    }
  }

  println!("d6_1 {}", counts
           .iter()
           .max_by(|e1, e2| {
             if infinites.contains(&e1) {
               Ordering::Less
             } else if infinites.contains(&e2) {
               Ordering::Greater
             } else {
               e1.cmp(e2)
             }
           })
           .unwrap());
}

fn d6_2() {
  let coords: Vec<Coord> = read_lines::<_, Coord>("day6").collect();

  let (min_x, max_x, min_y, max_y) = Coord::minmax(&coords);

  let mut n_points: usize = 0;

  for j in min_y ..= max_y {
    for i in min_x ..= max_x {
      let total_dist: usize = coords.iter().map(|c| c.dist(i, j)).sum();

      if total_dist < 10000 {
        n_points += 1;
      }
    }
  }

  println!("d6_2 {}", n_points);
}

#[derive(Debug, Clone, Copy)]
struct Dep(char, char);

impl FromStr for Dep {
  type Err = ParseError;

  fn from_str(input: &str) -> Result<Self, Self::Err> {
    let mut iter = input.chars().peekable();

    skip_ws(&mut iter);
    skip_n(&mut iter, "Step".len())?;
    skip_ws(&mut iter);
    let c1 = single_char(&mut iter)?;
    skip_ws(&mut iter);
    skip_n(&mut iter, "must be finished before step".len())?;
    skip_ws(&mut iter);
    let c2 = single_char(&mut iter)?;
    skip_ws(&mut iter);
    skip_n(&mut iter, "can begin.".len())?;
    skip_ws(&mut iter);

    Ok(Dep(c1, c2))
  }
}

fn d7_1() {
  let deps: Vec<Dep> = read_lines::<_, Dep>("day7").collect();
  let mut tree: Map<char, Set<char>> = Map::new();

  for dep in deps {
    tree.entry(dep.1).or_insert_with(Set::new).insert(dep.0);
    tree.entry(dep.0).or_insert_with(Set::new);
  }

  let mut queue: Vec<char> = Vec::new();
  let mut res: Vec<char> = Vec::new();

  loop {
    let mut remove = vec![];

    for (k, v) in &mut tree {
      if v.is_empty() {
        queue.push(*k);
        remove.push(*k);
      }
    }

    for r in remove {
      tree.remove(&r);
    }

    if queue.is_empty() {
      break;
    }

    queue.sort();
    let e = queue[0];
    res.push(e);
    queue.remove(0);

    for v in tree.values_mut() {
      v.remove(&e);
    }
  }

  print!("d7_1 ");
  res.iter().for_each(|i| print!("{}", i));
  println!();
}

#[derive(Debug, Clone, Copy)]
struct Task {
  name: char,
  time: usize,
}

impl PartialEq for Task {
  fn eq(&self, other: &Self) -> bool {
    self.name == other.name
  }
}

impl Eq for Task {}

impl PartialOrd for Task {
  fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
    self.name.partial_cmp(&other.name)
  }
}

impl Ord for Task {
  fn cmp(&self, other: &Self) -> Ordering {
    self.name.cmp(&other.name)
  }
}

enum TaskStatus {
  Finished(char),
  Running,
}

impl Task {
  fn new(name: char) -> Self {
    let time: usize = 60 +
      "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
      .chars()
      .enumerate()
      .find(|(_, c)| *c == name)
      .map(|(i, _)| i + 1)
      .unwrap();

    Self { name, time }
  }

  fn step(&mut self) -> TaskStatus {
    self.time -= 1;

    if self.time == 0 {
      TaskStatus::Finished(self.name)
    } else {
      TaskStatus::Running
    }
  }
}

enum WorkerStatus {
  Idle,
  Running,
  Finished(char),
}

#[derive(Debug, Clone, Copy)]
struct Worker {
  task: Option<Task>,
}

impl Worker {
  fn new() -> Self {
    Self { task: None }
  }

  fn step(&mut self, queue: &mut Vec<Task>) -> WorkerStatus {
    if let Some(ref mut task) = self.task {
      match task.step() {
        TaskStatus::Running => WorkerStatus::Running,
        TaskStatus::Finished(name) => {
          self.task = None;
          WorkerStatus::Finished(name)
        },
      }
    } else if queue.is_empty() {
      WorkerStatus::Idle
    } else {
      self.task = Some(queue[0]);
      queue.remove(0);
      self.step(queue)
    }
  }
}

fn d7_2() {
  let deps: Vec<Dep> = read_lines::<_, Dep>("day7").collect();
  let mut tree: Map<char, Set<char>> = Map::new();
  let mut workers: Vec<Worker> = vec![Worker::new(); 5];
  let mut time: usize = 0;

  for dep in deps {
    tree.entry(dep.1).or_insert_with(Set::new).insert(dep.0);
    tree.entry(dep.0).or_insert_with(Set::new);
  }

  let mut queue: Vec<Task> = Vec::new();

  loop {
    let mut remove = vec![];

    for (k, v) in &mut tree {
      if v.is_empty() {
        queue.push(Task::new(*k));
        remove.push(*k);
      }
    }

    for r in &remove {
      tree.remove(&r);
    }

    queue.sort();

    let mut any_running = false;

    for worker in &mut workers {
      remove.clear();

      match worker.step(&mut queue) {
        WorkerStatus::Idle => {},
        WorkerStatus::Running => any_running = true,
        WorkerStatus::Finished(name) => {
          any_running = true;
          remove.push(name);
        },
      }

      for v in tree.values_mut() {
        for r in &remove {
          v.remove(&r);
        }
      }
    }

    if !any_running {
      break;
    }

    time += 1;
  }

  println!("d7_2 {}", time);
}

#[derive(Debug, Clone)]
struct Node {
  metadata: Vec<usize>,
  children: Vec<Node>,
}

impl Node {
  fn new(mut input: Vec<usize>) -> (Self, Vec<usize>) {
    let n_children: usize = input.remove(0);
    let n_metadata: usize = input.remove(0);

    let mut children: Vec<Node> = Vec::new();
    let mut metadata: Vec<usize> = Vec::new();

    for _ in 0 .. n_children {
      let (node, new_input) = Node::new(input);
      children.push(node);
      input = new_input;
    }

    for _ in 0 .. n_metadata {
      metadata.push(input.remove(0));
    }

    (Self { metadata, children }, input)
  }

  fn metadata_sum(&self) -> usize {
    self.metadata.iter().sum::<usize>() +
      self.children.iter().map(|c| c.metadata_sum()).sum::<usize>()
  }

  fn value(&self) -> usize {
    if self.children.is_empty() {
      self.metadata_sum()
    } else {
      let mut sum = 0;

      for &i in &self.metadata {
        if i != 0 {
          let i = i - 1;

          if i < self.children.len() {
            sum += self.children[i].value();
          }
        }
      }

      sum
    }
  }
}

fn d8_1() {
  let data: Vec<usize> = fs::read_to_string("day8")
    .unwrap()
    .split_whitespace()
    .map(|w| usize::from_str(w).unwrap())
    .collect();

  let (root, input) = Node::new(data);

  assert!(input.is_empty());

  println!("d8_1 {}", root.metadata_sum());
  println!("d8_2 {}", root.value());
}

fn main() {
  d1_1();
  d1_2();
  d2_1();
  d2_2();
  d3_1_2();
  d4_1_2();
  d5_1();
  d5_2();
  d6_1();
  d6_2();
  d7_1();
  d7_2();
  d8_1();
}
