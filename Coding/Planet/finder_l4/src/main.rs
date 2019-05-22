#![feature(tool_lints)]
#![warn(clippy::all)]

use std::str::FromStr;

struct Star {
  name: String,
  light_curve: Vec<i32>,
}

impl Star {
  fn group_brightness_values(&self) -> Vec<Vec<i32>> {
    let mut res: Vec<Vec<i32>> = Vec::new();
    let mut iter = self.light_curve.iter();

    let mut curr: i32 = *iter.next().unwrap();
    let mut curr_vec: Vec<i32> = vec![curr];

    for &val in iter {
      if self.roughly_equal(curr, val) || self.within_pulsation(curr, val) {
        curr = val;
        curr_vec.push(val);
      } else {
        res.push(curr_vec);
        curr_vec = vec![val];
      }
    }

    res.push(curr_vec);

    res
  }

  fn within_pulsation(&self, i: i32, j: i32) -> bool {
    (i - j).abs() <= 200
  }

  fn low(&self, v: i32) -> f64 {
    f64::from(v) * (1.0 - (0.05 / 100.0))
  }

  fn high(&self, v: i32) -> f64 {
    f64::from(v) * (1.0 + (0.05 / 100.0))
  }

  fn roughly_equal(&self, i: i32, j: i32) -> bool {
    if i == j {
      true
    } else {
      let low = f64::max(self.low(i), self.low(j));
      let high = f64::min(self.high(i), self.high(j));
      high >= low
    }
  }

  // fn normal_luminosity(&self) -> i32 {
  //   self.light_curve[0]
  // }

  // fn is_flare(&self, value: i32) -> bool {
  //   value > self.normal_luminosity()
  // }

  // fn is_transit(&self, value: i32, n: usize) -> bool {
  //   let lum: i32 = self.normal_luminosity();
  //   let f_lum: f64 = f64::from(lum);
  //   let transit_lum: f64 = f_lum - (f_lum * (0.5 / 100.0));
  //   f64::from(value) <= transit_lum && n >= 5 && n <= 15
  // }

  // fn flares_and_transits(&self) -> (usize, usize) {
  //   let mut flares: Vec<i32> = Vec::new();
  //   let mut transits: Vec<i32> = Vec::new();

  //   for (value, n) in self.group_brightness_values() {
  //     if self.is_flare(value) {
  //       flares.push(value);
  //     } else if self.is_transit(value, n) {
  //       transits.push(value);
  //     }
  //   }

  //   (flares.len(), transits.len())
  // }

  // fn next_transits(&self,
  //                  brightness_vals: &[(i32, usize)],
  //                  index: usize,
  //                  value_a: i32,
  //                  n_a: usize)
  //                  -> Vec<(usize, usize)>
  // {
  //   let mut res: Vec<(usize, usize)> = Vec::new();
  //   let mut distance: usize = 0;

  //   for (i, &(value_b, n_b)) in brightness_vals.iter().enumerate().skip(index + 1) {
  //     if
  //       self.is_transit(value_b, n_b)    &&
  //       value_a == value_b && n_a == n_b &&
  //       distance >= 5
  //     {
  //       res.push((i, distance));
  //     }

  //     distance += n_b;
  //   }

  //   res
  // }

  // fn has_planet_candidate(&self) -> bool {
  //   if self.flares_and_transits().1 < 3 {
  //     return false;
  //   }

  //   let bvals: Vec<(i32, usize)> = self.group_brightness_values();

  //   for (i, &(value, n)) in bvals.iter().enumerate() {
  //     if i == 0 {
  //       continue;
  //     }

  //     if self.is_transit(value, n) {
  //       let mut deep_i: usize = i;

  //       for i in deep_i .. bvals.len() {
  //         let j_transits: Vec<(usize, usize)> = self.next_transits(&bvals, i, value, n);

  //         for (j, j_dist) in j_transits {
  //           let k_transits: Vec<(usize, usize)> = self.next_transits(&bvals, j, value, n);

  //           for (_, k_dist) in k_transits {
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

fn l4(file: &str) {
  let stars: Vec<Star> = parse_stars(read_file(file).split_whitespace());
  print!("{}:", file);
  for star in stars {
    print!(" {}", star.name);
    print!(" {:?}", star.group_brightness_values());
  }
  println!();
  println!("----------------");
}

fn main() {
  l4("data/level4-0.in"); // output: Star1 YES Star2 NO
  // l4("data/level4-1.in");
  // l4("data/level4-2.in");
  // l4("data/level4-3.in");
  // l4("data/level4-4.in");
  // l4("data/level4-5.in");
  // l4("data/level4-6.in");
}
