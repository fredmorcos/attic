extern crate rand;

use rand::Rng;
use std::collections::HashMap;
use std::time;

fn main() {
  println!("len        v   m");

  for n in 1..1000 {
    // n is the number of elements in the vector and hashmap
    let n = n * 100;

    let mut v: Vec<String> = Vec::with_capacity(n);
    let mut m: HashMap<String, ()> = HashMap::with_capacity(n);

    let mut last_elem: String = "".to_owned();

    for i in 0..n {
      // generate a random string of 5 chars
      let s: String = rand::thread_rng().gen_ascii_chars().take(5).collect();

      // eprintln!("string: {}", s);

      // keep the last generated string, this will be the one we look up: note that
      // this gives a major advantage to the hashmap
      if i == n - 1 {
        last_elem = s.clone();
      }

      v.push(s.clone());
      let _ = m.insert(s, ()).unwrap_or_else(|| {});
    }

    if last_elem == "" {
      panic!("WTF?");
    }

    assert_eq!(v.len(), n);
    assert!(m.len() <= n && m.len() >= n - 10);

    let mut t1: time::Instant;
    let mut t2: time::Instant;
    let duration_vec: time::Duration;
    let duration_map: time::Duration;

    t1 = time::Instant::now();
    for e in v {
      // we check every element to give the vector a disadvantage
      if e == last_elem {
        break;
      }
    }
    t2 = time::Instant::now();

    duration_vec = t2.duration_since(t1);

    t1 = time::Instant::now();
    if !m.contains_key(&last_elem) {
      panic!("WTF2?");
    }
    t2 = time::Instant::now();

    duration_map = t2.duration_since(t1);

    let seconds_vec = duration_vec.as_secs() as f64 + duration_vec.subsec_nanos() as f64 * 1e-9;
    let seconds_map = duration_map.as_secs() as f64 + duration_map.subsec_nanos() as f64 * 1e-9;

    println!("{}        {}   {}", n, seconds_vec, seconds_map);
  }
}
