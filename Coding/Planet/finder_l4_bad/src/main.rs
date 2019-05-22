#![feature(tool_lints)]
#![warn(clippy::all)]

use std::str::FromStr;

struct Star {
  name: String,
  light_curve: Vec<i32>,
}

enum Feature {
  Flare,
  Transit,
  Random,
  NoEvent,
}

impl ::std::fmt::Display for Feature {
  fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
    match self {
      Feature::Flare   => write!(f, "flare"),
      Feature::Transit => write!(f, "transit"),
      Feature::Random  => write!(f, "something"),
      Feature::NoEvent => write!(f, "nothing"),
    }
  }
}

impl Star {
  fn feature(&self) -> Feature {
    let mut last_vals: Vec<i32> = Vec::new();

    for (i, (values, n)) in self.group_brightness_values().iter().enumerate() {
      if i == 0 {
        last_vals = values.clone();
      } else {
        let last_val = *last_vals.last().unwrap();
        let val = *values.first().unwrap();

        if Star::is_transit(last_val, val) && *n >= 5 && *n <= 15 {
          return Feature::Transit;
        } else if val > last_val {
          return Feature::Flare;
        } else if *n >= 1 && *n <= 15 {
          return Feature::Random;
        }

        last_vals = values.clone();
      }
    }

    Feature::NoEvent
  }

  fn is_transit(prev_val: i32, val: i32) -> bool {
    let prev_val_f: f64 = f64::from(prev_val);
    let val_f: f64 = f64::from(val);
    (prev_val_f * (1.0 + 0.05 / 100.0)) -
      (val_f * (1.0 - 0.05 / 100.0)) - 200.0 > (prev_val_f * (0.5 / 100.0))

    // let a = prev_val_f + (prev_val_f * (0.05 / 100.0));
    // let b = val_f - (val_f * (0.05 / 100.0));
    // let c = prev_val_f * (0.5 / 100.0);
    // a - b - 200.0 > c

    // L[i] * (1 + 0.05 / 100) - L[i+1] * (1 - 0.05 / 100) - 200 > L[i]*0.5/100
  }

  fn is_within_noise(old_val: i32, new_val: i32) -> bool {
    if old_val == new_val {
      return true;
    }

    let old_val_f: f64 = f64::from(old_val);
    let new_val_f: f64 = f64::from(new_val);

    if old_val < new_val {
      new_val_f <= old_val_f + (old_val_f * (0.05 / 100.0))
    } else {
      new_val_f >= old_val_f - (old_val_f * (0.05 / 100.0))
    }
  }

  fn is_within_pulsation(old_val: i32, new_val: i32) -> bool {
    (new_val - old_val).abs() <= 200
  }

  fn group_brightness_values(&self) -> Vec<(Vec<i32>, usize)> {
    let mut res: Vec<(Vec<i32>, usize)> = Vec::new();
    let mut iter = self.light_curve.iter();

    let mut curr: i32 = *iter.next().unwrap();
    let mut vals: Vec<i32> = vec![curr];
    let mut n: usize = 1;

    for val in iter {
      if Star::is_within_noise(curr, *val) && Star::is_within_pulsation(curr, *val) {
        n += 1;
        vals.push(*val);
        curr = *val;
      } else {
        res.push((vals.clone(), n));
        vals.clear();
        vals.push(*val);
        curr = *val;
        n = 1;
      }
    }

    res.push((vals, n));

    res
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

  // fn normal_luminosity_range(&self) -> (f64, f64) {
  //   let f_lum = f64::from(self.normal_luminosity());
  //   let err = f_lum * (0.5 / 100.0);
  //   (f_lum - err, f_lum + err)
  // }

  // fn is_pulsation(&self, i: usize, j: usize) -> bool {
  //   assert_eq!(i, j - 1);
  //   (self.light_curve[i] - self.light_curve[j]).abs() <= 200
  // }

  // fn is_noise(&self, i: usize) -> bool {
  //   let (l, h) = self.normal_luminosity_range();
  //   let val = f64::from(self.light_curve[i]);
  //   val >= l && val <= h
  // }

  // fn is_flare(&self, i: usize) -> bool {
  //   let (_, h) = self.normal_luminosity_range();
  //   let val = f64::from(self.light_curve[i]);
  //   val > h
  // }

  // fn is_transit(&self, i: usize, j: usize) -> bool {
  //   assert_eq!(i, j - 1);

  //   let val_i = f64::from(self.light_curve[i]);
  //   let val_j = f64::from(self.light_curve[j]);

  //   val_i * (1.0 + 0.05 / 100.0) - val_j * (1.0 - 0.05 / 100.0) - 200.0 > val_i * 0.5 / 100.0
  // }

  // fn feature(&self) -> Feature {
  //   for i in 0 .. self.light_curve.len() - 1 {
  //     let is_flare = self.is_flare(i);
  //     let is_transit = self.is_transit(i, i + 1);
  //     let is_pulsation = self.is_pulsation(i, i + 1);
  //     let is_noise = self.is_noise(i);

  //     if is_flare {
  //       return Feature::Flare;
  //     } else if is_transit {
  //       return Feature::Transit;
  //     } else if !is_pulsation && !is_noise {
  //       return Feature::Random;
  //     }
  //   }

  //   Feature::NoEvent
  // }

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

  //       for i in deep_i + 1 .. brightness_vals.len() {
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

// fn l1(file: &str) {
//   let stars: Vec<Star> = parse_stars(read_file(file).split_whitespace());
//   print!("{}:", file);
//   for star in stars {
//     print!(" {}", star.name);
//     let grouped_values = star.group_brightness_values();
//     for (value, n) in grouped_values {
//       print!(" {} {}", value, n);
//     }
//   }
//   println!();
//   println!("----------------");
// }

// fn l2(file: &str) {
//   let stars: Vec<Star> = parse_stars(read_file(file).split_whitespace());
//   print!("{}:", file);
//   for star in stars {
//     print!(" {}", star.name);
//     let (flares_n, transits_n) = star.flares_and_transits();
//     print!(" {} {}", flares_n, transits_n);
//   }
//   println!();
//   println!("----------------");
// }

// fn l3(file: &str) {
//   let stars: Vec<Star> = parse_stars(read_file(file).split_whitespace());
//   print!("{}:", file);
//   for star in stars {
//     print!(" {}", star.name);

//     if file == "data/level1-2.in" {
//       for (value, n) in star.group_brightness_values() {
//         print!(" ({}, {})", value, n);
//       }
//     }

//     print!(" {}", if star.has_planet_candidate() { "YES" } else { "NO" });
//   }
//   println!();
//   println!("----------------");
// }

fn l4(file: &str) {
  let stars: Vec<Star> = parse_stars(read_file(file).split_whitespace());
  print!("{}:", file);
  for star in stars {
    print!(" {}", star.name);
    print!(" {}", star.feature());
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

  // l3("data/level3-0.in"); // output: Star1 YES Star2 NO
  // l3("data/level1-1.in");
  // l3("data/level1-2.in");
  // l3("data/level1-3.in");
  // l3("data/level1-4.in");
  // l3("data/level1-5.in");

  l4("data/level4-0.in"); // output: f flare t transit s something
  l4("data/level4-1.in");
  l4("data/level4-2.in");
  l4("data/level4-3.in");
  l4("data/level4-4.in");
  l4("data/level4-5.in");
  l4("data/level4-6.in");
}
