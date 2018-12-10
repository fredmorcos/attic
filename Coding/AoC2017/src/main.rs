use std::str::FromStr;

fn read_file(name: &str) -> String {
  use ::std::fs::File;
  use ::std::io::prelude::*;

  let mut file = File::open(name).expect(&format!("Cannot open file {}", name));
  let mut contents = String::new();
  file.read_to_string(&mut contents).expect(&format!("Cannot read file {}", name));
  String::from(contents.trim())
}

fn day1_helper(prev: u32, index: usize, v: &[u32]) -> u32 {
  if v.is_empty() {
    0
  } else if index == v.len() {
    if prev == v[0] {
      u32::from(prev)
    } else {
      0
    }
  } else {
    if prev == v[index] {
      u32::from(prev) + day1_helper(v[index], index + 1, v)
    } else {
      day1_helper(v[index], index + 1, v)
    }
  }
}

fn day1(v: &[u32]) {
  // print!("day1 {:?} --> ", v);
  print!("day1: ");
  println!("{}", day1_helper(v[0], 1, &v));
}

fn day1_2_halfway(index: usize, v: &[u32]) -> u32 {
  v[(index + (v.len() / 2)) % v.len()]
}

fn day1_2_helper(v: &[u32]) -> u32 {
  let mut sum: u32 = 0;

  for (idx, d) in v.iter().enumerate() {
    if *d == day1_2_halfway(idx, v) {
      sum += u32::from(*d);
    }
  }

  sum
}

fn day1_2(v: &[u32]) {
  // print!("day1_2 {:?} --> ", v);
  print!("day1_2: ");
  println!("{}", day1_2_helper(&v));
}

fn day2_helper_row_minmax(row: &[u32]) -> (u32, u32) {
  if row.is_empty() {
    (0, 0)
  } else {
    let mut min = row[0];
    let mut max = row[0];

    for e in row {
      if *e < min {
        min = *e;
      }

      if *e > max {
        max = *e;
      }
    }

    (min, max)
  }
}

fn day2(v: &[Vec<u32>]) {
  let mut sum = 0;

  for row in v {
    let (row_min, row_max) = day2_helper_row_minmax(row);
    sum += row_max - row_min;
  }

  println!("day2 {}", sum);
}

fn day2_2_helper_row_divisible(row: &[u32]) -> Option<u32> {
  for n1 in row {
    for n2 in row {
      if n1 > n2 && n1 % n2 == 0 {
        // println!("day2_2: {} and {}", n1, n2);
        return Some(n1 / n2);
      }
    }
  }

  None
}

fn day2_2(v: &[Vec<u32>]) {
  let mut sum = 0;

  for row in v {
    sum += day2_2_helper_row_divisible(row).unwrap();
  }

  println!("day2_2 {}", sum);
}

fn up   (pos: (i64, i64)) -> (i64, i64) { (pos.0, pos.1 + 1) }
fn down (pos: (i64, i64)) -> (i64, i64) { (pos.0, pos.1 - 1) }
fn left (pos: (i64, i64)) -> (i64, i64) { (pos.0 - 1, pos.1) }
fn right(pos: (i64, i64)) -> (i64, i64) { (pos.0 + 1, pos.1) }

fn day3_helper(i: usize, print: bool) -> (i64, i64) {
  let mut pos = (0, 0);
  let mut moves = 0;
  let mut max_moves = 1;

  enum Move { First, Second };
  let mut move_stage = Move::First;

  enum Direction { Up, Down, Left, Right };
  let mut direction = Direction::Right;

  for _ in 1 .. i {
    if moves == max_moves {
      if let Move::First = move_stage {
        move_stage = Move::Second;
      } else if let Move::Second = move_stage {
        move_stage = Move::First;
        max_moves += 1;
      }

      moves = 0;

      direction = match direction {
        Direction::Right => Direction::Up,
        Direction::Up    => Direction::Left,
        Direction::Left  => Direction::Down,
        Direction::Down  => Direction::Right,
      };
    }

    pos = match direction {
      Direction::Right => {
        if print {
          println!("Move Right {}/{}", moves, max_moves);
        }

        right(pos)
      },
      Direction::Left => {
        if print {
          println!("Move Left {}/{}", moves, max_moves);
        }

        left(pos)
      },
      Direction::Up => {
        if print {
          println!("Move Up {}/{}", moves, max_moves);
        }

        up(pos)
      },
      Direction::Down => {
        if print {
          println!("Move Down {}/{}", moves, max_moves);
        }

        down(pos)
      },
    };

    moves += 1;
  }

  pos
}

fn day3(i: usize, print: bool) {
  let pos: (i64, i64) = day3_helper(i, print);
  println!("day3: {} at position ({}, {}), {} moves to (0, 0)",
           i, pos.0, pos.1, pos.0.abs() + pos.1.abs());
}

fn day3_2_sum(pos: (i64, i64), values: &::std::collections::HashMap<(i64, i64), u64>) -> u64 {
  let mut sum: u64 = 0;
  if let Some(v) = values.get(&(pos.0 - 1, pos.1 - 1)) { sum += *v };
  if let Some(v) = values.get(&(pos.0 - 1, pos.1    )) { sum += *v };
  if let Some(v) = values.get(&(pos.0 - 1, pos.1 + 1)) { sum += *v };
  if let Some(v) = values.get(&(pos.0 + 1, pos.1 - 1)) { sum += *v };
  if let Some(v) = values.get(&(pos.0 + 1, pos.1    )) { sum += *v };
  if let Some(v) = values.get(&(pos.0 + 1, pos.1 + 1)) { sum += *v };
  if let Some(v) = values.get(&(pos.0    , pos.1 - 1)) { sum += *v };
  if let Some(v) = values.get(&(pos.0    , pos.1 + 1)) { sum += *v };
  sum
}

fn day3_2_helper(i: u64) -> u64 {
  let mut pos = (0, 0);
  let mut moves = 0;
  let mut max_moves = 1;

  enum Move { First, Second };
  let mut move_stage = Move::First;

  enum Direction { Up, Down, Left, Right };
  let mut direction = Direction::Right;

  use ::std::collections::HashMap as Map;
  let mut values: Map<(i64, i64), u64> = Map::new();

  loop {
    if moves == max_moves {
      if let Move::First = move_stage {
        move_stage = Move::Second;
      } else if let Move::Second = move_stage {
        move_stage = Move::First;
        max_moves += 1;
      }

      moves = 0;

      direction = match direction {
        Direction::Right => Direction::Up,
        Direction::Up    => Direction::Left,
        Direction::Left  => Direction::Down,
        Direction::Down  => Direction::Right,
      };
    }

    if pos.0 == 0 && pos.1 == 0 {
      if let Some(_) = values.insert(pos, 1) {
        unreachable!();
      }
    } else {
      let sum = day3_2_sum(pos, &values);

      // println!("{:?} ==> {}", pos, sum);

      if sum > i {
        return sum;
      }

      if let Some(_) = values.insert(pos, sum) {
        unreachable!();
      }
    }

    // println!("{:?}", values);

    pos = match direction {
      Direction::Right => right(pos),
      Direction::Left => left(pos),
      Direction::Up => up(pos),
      Direction::Down => down(pos),
    };

    moves += 1;
  }
}

fn day3_2(i: u64) {
  println!("day3_2: {} -> {}", i, day3_2_helper(i));
}

fn day4_helper(s: &str) -> bool {
  use ::std::collections::HashSet as Set;

  let mut words: Set<&str> = Set::new();

  for word in s.split_whitespace() {
    if !words.insert(word) {
      return false;
    }
  }

  return true;
}

fn day4(passphrases: &Vec<&str>) {
  let mut n_valid = 0;

  for passphrase in passphrases {
    if day4_helper(*passphrase) {
      n_valid += 1;
    }
  }

  println!("day4: {} valid", n_valid);
}

// fn day4_2_anagrams(s1: &mut String, s2: &mut String) -> bool {
//   if (s1.is_empty() || s2.is_empty()) || (s1.len() != s2.len()) {
//     return false;
//   }

//   let mut remove = None;

//   for (i, c1) in s1.chars().enumerate() {
//     for (j, c2) in s2.chars().enumerate() {
//       if c1 == c2 {
//         remove = Some((i, j));
//       }
//     }
//   }

//   if let Some((i, j)) = remove {
//     s1.remove(i);
//     s2.remove(j);
//     day4_2_anagrams(s1, s2)
//   } else {
//     false
//   }
// }

fn day4_2_helper(s: String) -> bool {
  use ::std::collections::HashSet as Set;
  use ::std::iter::FromIterator;

  let mut words: Set<String> = Set::new();

  for word in s.split_whitespace() {
    let mut chars: Vec<char> = word.chars().collect();
    chars.sort_by(|a, b| b.cmp(a));
    let w = String::from_iter(chars);

    if !words.insert(w) {
      return false;
    }
  }

  return true;
}

fn day4_2(passphrases: &Vec<&str>) {
  let mut n_valid = 0;

  for passphrase in passphrases {
    if day4_2_helper(String::from(*passphrase)) {
      n_valid += 1;
    }
  }

  println!("day4_2: {} valid", n_valid);
}

fn day5_helper(mut insts: Vec<i32>) -> u64 {
  let mut idx: i64 = 0;
  let mut jumps: u64 = 0;

  loop {
    if idx < 0 || idx >= insts.len() as i64 {
      break;
    }

    let new_idx: i64 = idx + insts[idx as usize] as i64;
    insts[idx as usize] += 1;
    idx = new_idx;
    jumps += 1
  }

  jumps
}

fn day5(insts: &Vec<i32>) {
  println!("day5: {} jumps", day5_helper(insts.clone()));
}

fn day5_2_helper(insts: &mut Vec<i32>) -> u64 {
  let mut idx: i64 = 0;
  let mut jumps: u64 = 0;

  let len = insts.len();

  loop {
    if idx < 0 || idx >= len as i64 {
      break;
    }

    let idx_usize: usize = idx as usize;
    let new_idx: i64 = idx + insts[idx_usize] as i64;

    if insts[idx_usize] >= 3 {
      insts[idx_usize] -= 1;
    } else {
      insts[idx_usize] += 1;
    }

    idx = new_idx;
    jumps += 1
  }

  jumps
}

fn day5_2(insts: &mut Vec<i32>) {
  println!("day5_2: {} jumps", day5_2_helper(insts));
}

fn day6_index_max(mem: &[u64]) -> usize {
  let mut max_idx = 0;
  let mut max_val = mem[0];

  for i in 1 .. mem.len() {
    if mem[i] > max_val {
      max_idx = i;
      max_val = mem[i]
    }
  }

  max_idx
}

fn day6_spread(mem: &mut Vec<u64>, n: u64, i: usize) {
  if n == 0 {
    return;
  } else {
    if i == mem.len() {
      day6_spread(mem, n, 0);
    } else {
      mem[i] += 1;
      day6_spread(mem, n - 1, i + 1)
    }
  }
}

fn day6_helper(mut mem: Vec<u64>) -> (u64, u64) {
  use ::std::collections::HashMap as Map;
  let mut seen: Map<Vec<u64>, u64> = Map::new();

  let mut rounds: u64 = 0;

  loop {
    let max_idx = day6_index_max(&mem);
    let max_val = mem[max_idx];
    mem[max_idx] = 0;

    day6_spread(&mut mem, max_val, max_idx + 1);

    rounds += 1;

    if let Some(old_rounds) = seen.insert(mem.clone(), rounds) {
      return (rounds, rounds - old_rounds);
    }
  }
}

fn day6(mem: Vec<u64>) {
  let (rounds, cycles) = day6_helper(mem);
  println!("day6: {} rounds, {} cycles", rounds, cycles);
}

use std::collections::{HashMap as Map, HashSet as Set};

#[derive(Debug)]
struct Prog {
  name: String,
  weight: i32,
  subprogs: Vec<String>,
}

struct ProgTree {
  #[allow(unused)]
  name: String,
  weight: i32,
  subprogs: Vec<ProgTree>,
}

fn day7_helper(progs: &Map<String, Prog>) -> String {
  let mut prognames = Set::new();

  for key in progs.keys() {
    prognames.insert(key.clone());
  }

  for prog in progs.values() {
    for subprog in &prog.subprogs {
      prognames.remove(subprog);
    }
  }

  assert_eq!(prognames.len(), 1);

  for value in prognames.drain() {
    return value;
  }

  unreachable!();
}

fn day7_parse(list: String) -> Map<String, Prog> {
  let mut progs: Map<String, Prog> = Map::new();

  for line in list.lines() {
    let mut words = line.split_whitespace();

    let name = words.next().unwrap().to_string();
    let weight = words.next().unwrap().to_string();
    let weight = weight[1 .. weight.len() - 1].to_string();
    let weight = i32::from_str_radix(&weight, 10).unwrap();

    let mut subprogs: Vec<String> = vec![];

    if let Some(arrow) = words.next() {
      assert_eq!(arrow, "->");

      while let Some(subprog) = words.next() {
        subprogs.push(subprog.to_string())
      }

      let mut new_subprogs = vec![];

      for i in 0 .. subprogs.len() - 1 {
        new_subprogs.push(subprogs[i][0 .. subprogs[i].len() - 1].to_string());
      }

      new_subprogs.push(subprogs[subprogs.len() - 1].to_string());
      subprogs = new_subprogs;
    }

    let prog = Prog {
      name: name.clone(),
      weight: weight,
      subprogs: subprogs,
    };

    if let Some(_) = progs.insert(name.clone(), prog) {
      unreachable!();
    }
  }

  progs
}

fn day7_2_construct(root: &Prog, progs: &Map<String, Prog>) -> ProgTree {
  let mut subprogs = Vec::new();

  for subprog in &root.subprogs {
    subprogs.push(day7_2_construct(progs.get(subprog).unwrap(), progs));
  }

  ProgTree {
    name: root.name.clone(),
    weight: root.weight,
    subprogs: subprogs,
  }
}

fn day7_2_weight(root: &ProgTree) -> i32 {
  root.weight + root.subprogs.iter().map(|p| day7_2_weight(p)).sum::<i32>()
}

fn day7(list: String) {
  let progs = day7_parse(list);
  // eprintln!("DEBUG: {:?}", progs);
  println!("day7: root = {}", day7_helper(&progs));
}

fn day7_2_helper(root: &ProgTree) -> i32 {
  for (i, subprog1) in root.subprogs.iter().enumerate() {
    let mut diffs = 0;
    let mut weight_diff = 0;
    let total_weight1 = day7_2_weight(subprog1);

    for (j, subprog2) in root.subprogs.iter().enumerate() {
      if i != j {
        let total_weight2 = day7_2_weight(subprog2);

        if total_weight2 != total_weight1 {
          diffs += 1;
          // weight_diff = subprog1.weight - subprog2.weight;
          weight_diff = (total_weight1 - total_weight2).abs();
        }
      }
    }

    if diffs > 1 {
      let res = day7_2_helper(subprog1);

      if res == 0 {
        // eprintln!("DEBUG: day7_2: node {} would need to have weight {}",
        //           subprog1.name, subprog1.weight - weight_diff);
        return subprog1.weight - weight_diff;
      } else {
        return res;
      }
    }
  }

  0
}

fn day7_2(list: String) {
  let progs = day7_parse(list);
  let root_name = day7_helper(&progs);
  let root = progs.get(&root_name).unwrap();
  let tree = day7_2_construct(root, &progs);
  let root_weight = day7_2_weight(&tree);
  println!("day7_2: root weight = {}", root_weight);
  println!("day7_2: correct weight = {}", day7_2_helper(&tree));
}

enum Tok {
  Reg(String),
  Lit(i32),
  Inc,
  Dec,
  LT,
  GT,
  LTE,
  GTE,
  EQ,
  NEQ,
}

enum Op {
  Inc(String, i32),
  Dec(String, i32),
}

enum Comp {
  LT (String, i32),
  GT (String, i32),
  LTE(String, i32),
  GTE(String, i32),
  EQ (String, i32),
  NEQ(String, i32),
}

struct Instr {
  comp: Comp,
  op: Op,
}

fn day8_extract_lit(tok: &Tok) -> i32 {
  if let &Tok::Lit(i) = tok {
    i
  } else {
    panic!("day8: Not a literal");
  }
}

fn day8_extract_reg(tok: &Tok) -> String {
  if let &Tok::Reg(ref r) = tok {
    r.clone()
  } else {
    panic!("day8: Not a reg");
  }
}

fn day8_make_instr(toks: Vec<Tok>) -> Instr {
  assert_eq!(toks.len(), 6);

  Instr {
    comp: match toks[4] {
      Tok::LT  => Comp::LT (day8_extract_reg(&toks[3]), day8_extract_lit(&toks[5])),
      Tok::GT  => Comp::GT (day8_extract_reg(&toks[3]), day8_extract_lit(&toks[5])),
      Tok::LTE => Comp::LTE(day8_extract_reg(&toks[3]), day8_extract_lit(&toks[5])),
      Tok::GTE => Comp::GTE(day8_extract_reg(&toks[3]), day8_extract_lit(&toks[5])),
      Tok::EQ  => Comp::EQ (day8_extract_reg(&toks[3]), day8_extract_lit(&toks[5])),
      Tok::NEQ => Comp::NEQ(day8_extract_reg(&toks[3]), day8_extract_lit(&toks[5])),
      _ => panic!("day8: Wrong token at position 4"),
    },
    op: match toks[1] {
      Tok::Inc => Op::Inc(day8_extract_reg(&toks[0]), day8_extract_lit(&toks[2])),
      Tok::Dec => Op::Dec(day8_extract_reg(&toks[0]), day8_extract_lit(&toks[2])),
      _ => panic!("day8: Wrong token at position 1"),
    },
  }
}

fn day8_parse(prog: String) -> (Vec<Instr>, Map<String, i32>) {
  let mut lines: Vec<Instr> = vec![];
  let mut regs: Map<String, i32> = Map::new();

  for line in prog.lines() {
    let mut words = line.split_whitespace();
    let mut line: Vec<Tok> = vec![];

    let tmp = words.next().unwrap();
    line.push(Tok::Reg(String::from(tmp)));
    regs.insert(String::from(tmp), 0);

    let tmp = words.next().unwrap();
    line.push(if      tmp == "inc" { Tok::Inc }
              else if tmp == "dec" { Tok::Dec }
              else { panic!("day8 parsing: Unknown token {}", tmp) });

    let tmp = words.next().unwrap();
    line.push(Tok::Lit(i32::from_str_radix(&tmp, 10).unwrap()));

    let tmp = words.next().unwrap();
    assert_eq!(tmp, "if");

    let tmp = words.next().unwrap();
    line.push(Tok::Reg(String::from(tmp)));
    regs.insert(String::from(tmp), 0);

    let tmp = words.next().unwrap();
    assert!(tmp.len() == 1 || tmp.len() == 2);
    line.push(if      tmp == ">"  { Tok::GT  }
              else if tmp == "<"  { Tok::LT  }
              else if tmp == ">=" { Tok::GTE }
              else if tmp == "<=" { Tok::LTE }
              else if tmp == "==" { Tok::EQ  }
              else if tmp == "!=" { Tok::NEQ }
              else { panic!("day7 parsing: Unknown comparison token {}", tmp) });

    let tmp = words.next().unwrap();
    line.push(Tok::Lit(i32::from_str_radix(&tmp, 10).unwrap()));

    assert_eq!(words.next(), None);
    lines.push(day8_make_instr(line));
  }

  (lines, regs)
}

fn day8_exec_op(op: Op, regs: &mut Map<String, i32>) -> i32 {
  match op {
    Op::Inc(reg, lit) => {
      let regval = *(regs.get(&reg).unwrap());
      regs.insert(reg, regval + lit);
      regval + lit
    },
    Op::Dec(reg, lit) => {
      let regval = *(regs.get(&reg).unwrap());
      regs.insert(reg, regval - lit);
      regval - lit
    },
  }
}

fn day8_exec(prog: Vec<Instr>, mut regs: &mut Map<String, i32>) -> i32 {
  let mut max_val = 0;

  for instr in prog {
    match instr.comp {
      Comp::LT  (reg, lit) => {
        let regval = *(regs.get(&reg).unwrap());
        if regval < lit {
          let res = day8_exec_op(instr.op, &mut regs);
          if res > max_val {
            max_val = res;
          }
        }
      },
      Comp::GT  (reg, lit) => {
        let regval = *(regs.get(&reg).unwrap());
        if regval > lit {
          let res = day8_exec_op(instr.op, &mut regs);
          if res > max_val {
            max_val = res;
          }
        }
      },
      Comp::LTE (reg, lit) => {
        let regval = *(regs.get(&reg).unwrap());
        if regval <= lit {
          let res = day8_exec_op(instr.op, &mut regs);
          if res > max_val {
            max_val = res;
          }
        }
      },
      Comp::GTE (reg, lit) => {
        let regval = *(regs.get(&reg).unwrap());
        if regval >= lit {
          let res = day8_exec_op(instr.op, &mut regs);
          if res > max_val {
            max_val = res;
          }
        }
      },
      Comp::EQ  (reg, lit) => {
        let regval = *(regs.get(&reg).unwrap());
        if regval == lit {
          let res = day8_exec_op(instr.op, &mut regs);
          if res > max_val {
            max_val = res;
          }
        }
      },
      Comp::NEQ (reg, lit) => {
        let regval = *(regs.get(&reg).unwrap());
        if regval != lit {
          let res = day8_exec_op(instr.op, &mut regs);
          if res > max_val {
            max_val = res;
          }
        }
      },
    }
  }

  max_val
}

fn day8_all(prog: String) {
  let (prog, mut regs) = day8_parse(prog);
  let max_val_all = day8_exec(prog, &mut regs);

  let mut max_val: i32 = *(regs.values().next().unwrap());

  for v in regs.values() {
    if *v > max_val {
      max_val = *v;
    }
  }

  println!("day8: max value in registers = {}", max_val);
  println!("day8_2: max value ever held = {}", max_val_all);
}

use std::iter::Peekable;

enum Type {
  #[allow(unused)]
  Garbage,
  Group(Vec<Type>),
}

fn day9_parse_group<I>(i: &mut Peekable<I>, garbage_count: &mut u32) -> Type
  where I: Iterator<Item = char>
{
  if let Some('{') = i.next() {
    let subgroups = day9_parse_list(i, garbage_count);
    assert_eq!(i.next(), Some('}'));
    return Type::Group(subgroups);
  }

  panic!("Expected a { at the beginning of a group");
}

fn day9_parse_garbage<I>(i: &mut Peekable<I>, count: &mut u32) -> Type
  where I: Iterator<Item = char>
{
  if let Some('<') = i.next() {
    loop {
      match i.next() {
        Some('>') => return Type::Garbage,
        Some('!') => { i.next(); },
        Some(_)   => { *count = *count + 1; },
        None      => panic!("Unexpected EOF in garbage"),
      }
    }
  }

  panic!("Expected a < at the beginning of garbage");
}

fn day9_parse_list<I>(i: &mut Peekable<I>, garbage_count: &mut u32) -> Vec<Type>
  where I: Iterator<Item = char>
{
  let mut res = vec![];

  loop {
    if let Some(&'}') = i.peek() {
      return vec![];
    } else if let Some(&'{') = i.peek() {
      res.push(day9_parse_group(i, garbage_count));
    } else if let Some(&'<') = i.peek() {
      res.push(day9_parse_garbage(i, garbage_count));
    } else {
      panic!("Unexpected data in list");
    }

    if let Some(&',') = i.peek() {
      let _ = i.next();
    } else {
      break;
    }
  }

  res
}

fn day9_helper_count(g: &Type, c: usize) -> usize {
  match g {
    &Type::Garbage => 0,
    &Type::Group(ref subgroups) =>
      c + subgroups.iter().map(|g| day9_helper_count(g, c + 1)).sum::<usize>(),
  }
}

fn day9_helper(groups: &str, garbage_count: &mut u32) -> usize {
  let mut chars = groups.chars().peekable();
  let res = day9_parse_group(&mut chars, garbage_count);
  day9_helper_count(&res, 1)
}

fn day9(groups: &str) {
  let mut garbage_count = 0;
  let res = day9_helper(groups, &mut garbage_count);
  println!("day9: {}, garbage count = {}", res, garbage_count);
}

fn day10_swap_range(list: &mut Vec<u8>, idx: usize, len: u8) {
  fn swap(list: &mut Vec<u8>, idx1: usize, idx2: usize) {
    assert_eq!(list.len(), 256);
    let len = list.len();
    let tmp = list[idx1 % len];
    list[idx1 % len] = list[idx2 % len];
    list[idx2 % len] = tmp;
  }

  let mut i: usize = idx as usize;
  let mut j: usize = idx as usize + len as usize - 1;

  while i < j {
    swap(list, i, j);
    i += 1;
    j -= 1;
  }

}

fn day10_helper(list: &mut Vec<u8>, lens: &[u8], pos: &mut usize, skip: &mut u16) {
  for len in lens {
    day10_swap_range(list, *pos, *len);
    // println!("DEBUG: day10: pos:{} len:{} skip:{}: {:?}", pos, len, skip, list);
    *pos += *len as usize + *skip as usize;
    *skip += 1;
  }
}

fn day10(lens: &[u8]) {
  let mut list: Vec<u8> = Vec::new();
  for i in 0..255 {
    list.push(i);
  }
  list.push(255);
  assert_eq!(list.len(), 256);
  day10_helper(&mut list, lens, &mut 0, &mut 0);
  println!("day10: {}", list[0] as u16 * list[1] as u16);
}

fn day10_2_densify(list: &[u8]) -> Vec<u8> {
  assert_eq!(list.len(), 256);
  let mut dense = Vec::with_capacity(16);
  for chunk in list.chunks(16) {
    let mut res = chunk[0];
    for i in 1 .. chunk.len() {
      res ^= chunk[i];
    }
    dense.push(res);
  }
  dense
}

fn day10_hash_sep(list: &[u8]) -> String {
  let mut res = String::new();
  for i in list {
    res.push_str(&format!("{:0>2x} ", i));
  }
  res
}

fn day10_hash(list: &[u8]) -> String {
  let mut res = String::new();
  for i in list {
    res.push_str(&format!("{:0>2x}", i));
  }
  res
}

fn day10_2(input: String) {
  let mut lens: Vec<u8> = input.bytes().collect();
  lens.extend(vec![17,31,73,47,23]);

  let mut list: Vec<u8> = Vec::new();
  for i in 0..255 {
    list.push(i);
  }
  list.push(255);
  assert_eq!(list.len(), 256);
  let mut pos = 0;
  let mut skip = 0;
  for _ in 0..64 {
    day10_helper(&mut list, &lens, &mut pos, &mut skip);
  }
  let dense = day10_2_densify(&list);
  println!("day10_2: {} ({})", day10_hash_sep(&dense), day10_hash(&dense));
}

fn main() {
  let nums: Vec<u32> = read_file("day1").chars().map(|c| c.to_digit(10).unwrap()).collect();
  day1(&vec![1,1,2,2]);
  day1(&vec![1,1,1,1]);
  day1(&vec![1,2,3,4]);
  day1(&vec![9,1,2,1,2,1,2,9]);
  day1(&nums);
  day1_2(&vec![1,2,1,2]);
  day1_2(&vec![1,2,2,1]);
  day1_2(&vec![1,2,3,4,2,5]);
  day1_2(&vec![1,2,3,1,2,3]);
  day1_2(&vec![1,2,1,3,1,4,1,5]);
  day1_2(&nums);

  let table: Vec<Vec<u32>> = {
    let mut table = vec![];
    let data = read_file("day2");
    for line in data.lines() {
      let line = line.trim();
      let nums = line.split_whitespace();
      let row: Vec<u32> = nums.map(|n| u32::from_str_radix(n, 10).unwrap()).collect();
      table.push(row);
    }
    table
  };
  day2(&vec![vec![5,1,9,5], vec![7,5,3], vec![2,4,6,8]]);
  day2(&table);
  day2_2(&vec![vec![5,9,2,8], vec![9,4,7,3], vec![3,8,6,5]]);
  day2_2(&table);

  day3(1, false);
  day3(12, false);
  day3(23, false);
  day3(1024, false);
  day3(325489, false);
  day3_2(1);
  day3_2(2);
  day3_2(3);
  day3_2(4);
  day3_2(5);
  day3_2(325489);

  let day4_data = read_file("day4");
  let passphrases: Vec<&str> = day4_data.lines().collect();
  day4(&vec!["aa bb cc dd ee"]);
  day4(&vec!["aa bb cc dd aa"]);
  day4(&vec!["aa bb cc dd aaa"]);
  day4(&passphrases);
  day4_2(&vec!["abcde fghij"]);
  day4_2(&vec!["abcde xyz ecdab"]);
  day4_2(&vec!["a ab abc abd abf abj"]);
  day4_2(&vec!["iiii oiii ooii oooi oooo"]);
  day4_2(&vec!["oiii ioii iioi iiio"]);
  day4_2(&passphrases);

  let mut instrs_small = vec![0, 3, 0, 1, -3];
  let mut instrs: Vec<i32> = read_file("day5")
    .split_whitespace().map(|i| i32::from_str(i).unwrap()).collect();
  day5(&instrs_small);
  day5(&instrs);
  day5_2(&mut instrs_small);
  day5_2(&mut instrs);

  let membanks = vec![11,11,13,7,0,15,5,5,4,4,1,1,7,1,15,11];
  day6(vec![0, 2, 7, 0]);
  day6(membanks);

  let progs_small = read_file("day7_small");
  let progs = read_file("day7_big");
  day7(progs_small.clone());
  day7(progs.clone());
  day7_2(progs_small);
  day7_2(progs);

  day8_all(read_file("day8_1"));
  day8_all(read_file("day8_2"));

  let stream = read_file("day9");
  day9("{}");
  day9("{{{}}}");
  day9("{{},{}}");
  day9("{{{},{},{{}}}}");
  day9("{<a>,<a>,<a>,<a>}");
  day9("{{<ab>},{<ab>},{<ab>},{<ab>}}");
  day9("{{<!!>},{<!!>},{<!!>},{<!!>}}");
  day9("{{<a!>},{<a!>},{<a!>},{<ab>}}");
  day9(&stream);

  let knots = read_file("day10_1");
  day10(&knots.split(",")
        .map(|i| u8::from_str(i).unwrap())
        .collect::<Vec<u8>>());
  day10_2("".to_string());
  day10_2("1,2,3".to_string());
  day10_2("1,2,4".to_string());
  day10_2(knots);
}
