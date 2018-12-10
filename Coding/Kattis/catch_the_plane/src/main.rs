use std::io::{self, BufRead};
use std::collections::HashMap as Map;

fn main() {
  let stdin = io::stdin();
  let mut lines = stdin.lock().lines();

  // the first line has the number of buses and the number of stations
  // in the city
  let first_line = lines.next().unwrap().unwrap();
  let mut first_line = first_line.split_whitespace();
  let n_buses: u64 = first_line.next().unwrap().parse().unwrap();
  let n_stations: u64 = first_line.next().unwrap().parse().unwrap();

  assert_eq!(first_line.next(), None);

  // the second line has the time at which we arrive at the airport
  let arrival: u64 = lines.next().unwrap().unwrap().trim().parse().unwrap();

  eprintln!("Num of buses = {}, num of stations = {}, arrival = {}",
            n_buses, n_stations, arrival);

  #[derive(Debug, Clone, Copy)]
  struct Entry {
    start: u64,
    dest: u64,
    departure: u64,
    arrival: u64,
    prob: f64,
  }

  let mut table: Map<u64, Vec<Entry>> = Map::new();

  for _ in 0 .. n_buses {
    let line = lines.next().unwrap().unwrap();
    let mut line = line.split_whitespace();
    let entry = Entry {
      start     : line.next().unwrap().parse().unwrap(),
      dest      : line.next().unwrap().parse().unwrap(),
      departure : line.next().unwrap().parse().unwrap(),
      arrival   : line.next().unwrap().parse().unwrap(),
      prob      : line.next().unwrap().parse().unwrap(),
    };

    table.entry(entry.dest).and_modify(|e| e.push(entry)).or_insert(vec![entry]);
  }

  eprintln!("Table = \n{:#?}", table);


}
