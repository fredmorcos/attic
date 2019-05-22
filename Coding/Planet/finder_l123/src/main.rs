#![feature(tool_lints)]
#![warn(clippy::all)]

use std::str::FromStr;

struct Star {
  name: String,
  light_curve: Vec<i32>,
}

impl Star {
  fn group_brightness_values(&self) -> Vec<(i32, usize)> {
    let mut res: Vec<(i32, usize)> = Vec::new();
    let mut iter = self.light_curve.iter();

    let mut curr: i32 = *iter.next().unwrap();
    let mut n: usize = 1;

    for val in iter {
      if *val == curr {
        n += 1;
      } else {
        res.push((curr, n));
        curr = *val;
        n = 1;
      }
    }

    res.push((curr, n));

    res
  }

  fn normal_luminosity(&self) -> i32 {
    self.light_curve[0]
  }

  fn is_flare(&self, value: i32) -> bool {
    value > self.normal_luminosity()
  }

  fn is_transit(&self, value: i32, n: usize) -> bool {
    let lum: i32 = self.normal_luminosity();
    let f_lum: f64 = f64::from(lum);
    let transit_lum: f64 = f_lum - (f_lum * (0.5 / 100.0));
    f64::from(value) <= transit_lum && n >= 5 && n <= 15
  }

  fn flares_and_transits(&self) -> (usize, usize) {
    let mut flares: Vec<i32> = Vec::new();
    let mut transits: Vec<i32> = Vec::new();

    for (value, n) in self.group_brightness_values() {
      if self.is_flare(value) {
        flares.push(value);
      } else if self.is_transit(value, n) {
        transits.push(value);
      }
    }

    (flares.len(), transits.len())
  }

  // fn find_next_transit(&self,
  //                      brightness_vals: &[(i32, usize)],
  //                      index: usize,
  //                      value_a: i32,
  //                      n_a: usize)
  //                      -> Option<(usize, usize)>
  // {
  //   let mut distance: usize = 0;

  //   for (i, &(value_b, n_b)) in brightness_vals.iter().enumerate().skip(index + 1) {
  //     if
  //       self.is_transit(value_b, n_b)    &&
  //       value_a == value_b && n_a == n_b &&
  //       distance >= 5
  //     {
  //       return Some((i, distance));
  //     }

  //     distance += n_b;
  //   }

  //   None
  // }

  // fn has_planet_candidate(&self) -> bool {
  //   if self.flares_and_transits().1 < 3 {
  //     return false;
  //   }

  //   let brightness_vals: Vec<(i32, usize)> = self.group_brightness_values();

  //   for (i, &(value, n)) in brightness_vals.iter().enumerate() {
  //     if i == 0 {
  //       continue;
  //     }

  //     if self.is_transit(value, n) {
  //       let mut deep_i: usize = i;

  //       for i in deep_i .. brightness_vals.len() {
  //         if let Some((j, j_dist)) = self.find_next_transit(&brightness_vals, i, value, n) {
  //           if let Some((_, k_dist)) = self.find_next_transit(&brightness_vals, j, value, n) {
  //             if j_dist == k_dist {
  //               return true;
  //             }
  //           }
  //         }
  //       }
  //     }
  //   }

  //   false
  // }

  fn next_transits(&self,
                   brightness_vals: &[(i32, usize)],
                   index: usize,
                   value_a: i32,
                   n_a: usize)
                   -> Vec<(usize, usize)>
  {
    let mut res: Vec<(usize, usize)> = Vec::new();
    let mut distance: usize = 0;

    for (i, &(value_b, n_b)) in brightness_vals.iter().enumerate().skip(index + 1) {
      if
        self.is_transit(value_b, n_b)    &&
        value_a == value_b && n_a == n_b &&
        distance >= 5
      {
        res.push((i, distance));
      }

      distance += n_b;
    }

    res
  }

  fn has_planet_candidate(&self) -> bool {
    if self.flares_and_transits().1 < 3 {
      return false;
    }

    let bvals: Vec<(i32, usize)> = self.group_brightness_values();

    for (i, &(value, n)) in bvals.iter().enumerate() {
      if i == 0 {
        continue;
      }

      if self.is_transit(value, n) {
        let mut deep_i: usize = i;

        for i in deep_i .. bvals.len() {
          let j_transits: Vec<(usize, usize)> = self.next_transits(&bvals, i, value, n);

          for (j, j_dist) in j_transits {
            let k_transits: Vec<(usize, usize)> = self.next_transits(&bvals, j, value, n);

            for (_, k_dist) in k_transits {
              if j_dist == k_dist {
                return true;
              }
            }
          }
        }
      }
    }

    false
  }
}

fn parse_stars<'a>(mut iter: impl Iterator<Item = &'a str>) -> Vec<Star> {
  let mut m: usize = usize::from_str(iter.next().unwrap()).unwrap();
  let mut stars: Vec<Star> = Vec::with_capacity(m);

  while m > 0 {
    let name: String = iter.next().unwrap().to_string();
    let mut n: usize = usize::from_str(iter.next().unwrap()).unwrap();
    let mut light_curve: Vec<i32> = Vec::with_capacity(n);

    while n > 0 {
      let v: i32 = i32::from_str(iter.next().unwrap()).unwrap();
      light_curve.push(v);
      n -= 1;
    }

    stars.push(Star { name, light_curve });
    m -= 1;
  }

  assert_eq!(None, iter.next());

  stars
}

fn read_file(path: &str) -> String {
  use ::std::io::Read;
  let mut f = ::std::fs::File::open(path).unwrap();
  let mut buf = String::new();
  f.read_to_string(&mut buf).unwrap();
  buf
}

fn l1(file: &str) {
  let stars: Vec<Star> = parse_stars(read_file(file).split_whitespace());
  print!("{}:", file);
  for star in stars {
    print!(" {}", star.name);
    let grouped_values = star.group_brightness_values();
    for (value, n) in grouped_values {
      print!(" {} {}", value, n);
    }
  }
  println!();
  println!("----------------");
}

fn l2(file: &str) {
  let stars: Vec<Star> = parse_stars(read_file(file).split_whitespace());
  print!("{}:", file);
  for star in stars {
    print!(" {}", star.name);
    let (flares_n, transits_n) = star.flares_and_transits();
    print!(" {} {}", flares_n, transits_n);
  }
  println!();
  println!("----------------");
}

fn l3(file: &str) {
  let stars: Vec<Star> = parse_stars(read_file(file).split_whitespace());
  print!("{}:", file);
  for star in stars {
    print!(" {}", star.name);

    // if file == "data/level1-2.in" {
    //   for (value, n) in star.group_brightness_values() {
    //     print!(" ({}, {})", value, n);
    //   }
    // }

    print!(" {}", if star.has_planet_candidate() { "YES" } else { "NO" });
  }
  println!();
  println!("----------------");
}

fn main() {
  // l1("data/level1-0.in"); // output: Star1 100 4 90 5 100 1 Star2 100 3 80 1 100 2
  // l1("data/level1-1.in");
  // l1("data/level1-2.in");
  // l1("data/level1-3.in");
  // l1("data/level1-4.in");
  // l1("data/level1-5.in");

  // l2("data/level2-0.in"); // output: Star1 1 1
  // l2("data/level1-1.in");
  // l2("data/level1-2.in");
  // l2("data/level1-3.in");
  // l2("data/level1-4.in");
  // l2("data/level1-5.in");

  l3("data/level3-0.in"); // output: Star1 YES Star2 NO
  l3("data/level1-1.in");
  l3("data/level1-2.in");
  l3("data/level1-3.in");
  l3("data/level1-4.in");
  l3("data/level1-5.in");
}
