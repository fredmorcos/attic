#![feature(tool_lints)]
#![warn(clippy::all)]

use std::str::FromStr;
use std::fmt::Display;

struct Tables {
  sales: Vec<(usize, usize, u64)>,
  payments: Vec<(usize, u64)>,
}

impl Tables {
  fn parse<'a>(mut iter: impl Iterator<Item = &'a str>) -> Tables {
    let mut tables: Tables = Tables {
      sales: Vec::new(),
      payments: Vec::new(),
    };

    while let Some(typ) = iter.next() {
      let typ: char = char::from_str(typ).unwrap();
      assert!(typ == 'F' || typ == 'B');

      let day: usize = usize::from_str(iter.next().unwrap()).unwrap();

      if typ == 'F' {
        let time_frame: usize = usize::from_str(iter.next().unwrap()).unwrap();
        let val: u64 = u64::from_str(iter.next().unwrap()).unwrap();
        tables.sales.push((day, time_frame, val));
      } else if typ == 'B' {
        let val: u64 = u64::from_str(iter.next().unwrap()).unwrap();
        tables.payments.push((day, val));
      } else {
        unreachable!();
      }
    }

    tables.sales.sort_by(|(a, _, _), (b, _, _)| a.cmp(&b));
    tables.payments.sort_by(|(a, _), (b, _)| a.cmp(&b));
    tables
  }

  fn find_fit(salary_day: usize,
              time_frame: usize,
              salary: u64,
              count: usize,
              payments: &[(usize, u64)])
              -> (Vec<usize>, bool)
  {
    if salary == 0 {
      (Vec::new(), true)
    } else if count == 0 || payments.is_empty() {
      (Vec::new(), false)
    } else {
      let mut res: Vec<_> = Vec::new();

      for (i, &(day, payment)) in payments.iter().enumerate() {
        if (day < salary_day + time_frame) && payment <= salary {
          res.push(i);

          let new_payments: Vec<(usize, u64)> =
            payments.iter().skip(i + 1).cloned().collect();

          let (mut to_delete, finished) =
            Tables::find_fit(salary_day,
                             time_frame,
                             salary - payment,
                             count - 1,
                             &new_payments);

          if finished {
            for d in &mut to_delete {
              *d += i + 1;
            }
            res.extend(to_delete);
            return (res, true);
          }

          res.pop().unwrap();
        }
      }

      (Vec::new(), false)
    }
  }

  fn unpaid_days(&mut self) -> Vec<usize> {
    let sales: Vec<(usize, usize, u64)> = self.sales.clone();
    let mut unpaid: Vec<usize> = Vec::new();

    for (day, time_frame, salary) in sales {
      let (to_delete, finished) = Tables::find_fit(day, time_frame, salary, 4, &self.payments);

      println!("Day {}: todel = {:?}", day, to_delete);

      if finished {
        for del in to_delete.iter().rev() {
          self.payments.remove(*del);
        }
      } else {
        unpaid.push(day);
      }

      println!("Payments is now = {:?}", self.payments);
    }

    unpaid
  }
}

fn print_list(res: &[impl Display]) {
  for e in res {
    print!(" {}", e);
  }
  println!();
}

fn run(input: &str) {
  let mut tables = Tables::parse(input.split_whitespace());

  println!("Before:");
  println!("  Sales:    {:?}", tables.sales);
  println!("  Payments: {:?}", tables.payments);

  print!("Result =");
  print_list(&tables.unpaid_days());

  // println!("After:");
  // println!("  Sales:    {:?}", tables.sales);
  // println!("  Payments: {:?}", tables.payments);
}

fn main() {
  // output: 1 2
  run("F 1 2 150 F 2 4 170 F 3 4 150 B 4 150 B 2 10 B 3 90");
  run("F 1 4 18 F 2 4 340 F 3 4 256 B 1 9 B 2 170 B 3 3 B 3 128 B 4 170 B 5 82");
  run("F 1 4 135 F 2 4 103 F 3 4 232 B 1 67 B 2 51 B 3 68 B 3 116 B 4 52 B 5 105");

  run("F 1 4 5637 F 2 4 2492 F 3 4 4050 F 4 4 4525 F 5 4 8290 F 6 4 9636 F 7 4 4290 F 8 4 154 F 9 4 6516 F 10 4 2115 F 11 4 3919 F 12 4 8205 F 13 4 2946 F 14 4 3343 F 15 4 9399 F 17 4 3666 F 16 4 745 F 19 4 5681 F 18 4 192 F 21 4 1083 F 20 4 8239 F 23 4 9581 F 22 4 2113 F 25 4 204 F 24 4 296 F 27 4 8094 F 26 4 7396 F 29 4 2891 F 28 4 4351 F 31 4 915 F 30 4 3790 F 34 4 6543 F 35 4 4174 F 32 4 8114 F 33 4 9834 F 38 4 2152 F 39 4 5463 F 36 4 9914 F 37 4 5895 B 1 2818 B 2 1410 B 2 1246 B 3 1409 B 3 2025 B 4 1246 B 4 2262 B 5 2025 B 5 4145 B 6 2263 B 6 4818 B 7 4145 B 7 2409 B 7 2145 B 8 2409 B 8 1073 B 8 77 B 9 1072 B 9 39 B 9 3258 B 10 38 B 10 1057 B 11 3258 B 11 1959 B 12 1058 B 12 980 B 12 4102 B 13 980 B 13 2052 B 13 1473 B 14 2051 B 14 1671 B 15 1473 B 15 4699 B 17 4700 B 17 135 B 17 1833 B 16 1672 B 16 372 B 19 916 B 19 48 B 19 2840 B 18 186 B 18 917 B 18 96 B 21 1420 B 21 541 B 20 48 B 20 1421 B 20 4119 B 23 271 B 23 529 B 23 4790 B 22 4120 B 22 271 B 22 1056 B 25 4791 B 25 74 B 25 102 B 24 528 B 24 148 B 27 51 B 27 4047 B 26 74 B 26 51 B 26 3698 B 29 4047 B 29 1088 B 29 1445 B 28 3698 B 28 2175 B 31 1446 B 31 457 B 30 1088 B 30 1895 B 34 2028 B 34 2459 B 34 3271 B 35 2458 B 35 1636 B 35 2087 B 32 1526 B 32 229 B 32 4057 B 33 229 B 33 2029 B 33 4917 B 38 2478 B 38 1076 B 39 2948 B 39 2731 B 36 1636 B 36 4957 B 37 2087 B 37 2479 B 37 2947 B 40 1076 B 40 1366 B 41 1366");

  // run("F 1 4 2357 F 2 4 9353 F 3 4 8627 F 4 4 487 F 5 4 3887 F 6 4 7146 F 7 4 6135 F 8 4 1982 F 9 4 5183 F 10 4 8717 F 11 4 7707 F 12 4 4786 F 13 4 6902 F 14 4 852 F 15 4 5589 F 17 4 2139 F 16 4 4851 F 19 4 1817 F 18 4 2593 F 21 4 1163 F 20 4 7962 F 23 4 4457 F 22 4 6636 F 25 4 4406 F 24 4 3412 F 27 4 6239 F 26 4 2004 F 29 4 7435 F 28 4 1651 F 31 4 2545 F 30 4 1847 F 34 4 8705 F 35 4 8783 F 32 4 6008 F 33 4 5504 F 38 4 6374 F 39 4 8840 F 36 4 7527 F 37 4 1184 B 1 1178 B 2 590 B 2 4676 B 3 4313 B 4 4677 B 4 243 B 5 4314 B 5 122 B 5 1943 B 6 589 B 6 122 B 6 972 B 6 3573 B 7 972 B 7 3067 B 8 3573 B 8 1534 B 8 991 B 9 1534 B 9 2591 B 10 991 B 10 4358 B 11 2592 B 11 3853 B 12 4359 B 12 2393 B 13 1936 B 13 3451 B 14 2393 B 14 1726 B 14 426 B 15 1725 B 15 2794 B 17 1397 B 17 1213 B 17 1069 B 16 426 B 16 1398 B 16 2425 B 19 535 B 19 908 B 18 1213 B 18 535 B 18 1296 B 21 454 B 21 581 B 20 1297 B 20 455 B 20 3981 B 23 582 B 23 2228 B 22 3981 B 22 3318 B 25 1114 B 25 2203 B 24 3318 B 24 1026 B 24 1706 B 27 1101 B 27 3119 B 26 1706 B 26 1102 B 26 1002 B 29 3120 B 29 413 B 29 3717 B 28 1002 B 28 825 B 31 1859 B 31 1272 B 30 413 B 30 1859 B 30 923 B 34 1502 B 34 4352 B 35 2752 B 35 2177 B 35 4391 B 32 924 B 32 3004 B 33 1273 B 33 1502 B 33 2752 B 38 1882 B 38 296 B 38 3187 B 39 296 B 39 1594 B 39 4420 B 36 2176 B 36 2196 B 36 3763 B 37 2196 B 37 159 B 37 592 B 40 1593 B 41 4420");
}
