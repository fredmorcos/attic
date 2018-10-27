use std::thread;
use std::time::Duration;
use std::sync::{Mutex, Arc};

struct Philosopher {
  name: String,
  left: usize,
  right: usize
}

struct Table {
  forks: Vec<Mutex<()>>
}

impl Philosopher {
  fn new(name: &str, left: usize, right: usize) -> Philosopher {
    Philosopher {
      name: name.to_string(),
      left: left,
      right: right
    }
  }

  fn eat(&self, table: &Table) {
    let _ = table.forks[self.left].lock().unwrap();
    let _ = table.forks[self.right].lock().unwrap();

    println!("{} is eating", self.name);
    thread::sleep(Duration::new(1, 0));
    println!("{} is done eating", self.name);
  }
}

fn main() {
  let table = Arc::new(Table { forks: vec![
    Mutex::new(()),
    Mutex::new(()),
    Mutex::new(()),
    Mutex::new(()),
    Mutex::new(()),
    ]});

  let philosophers = vec![
    Philosopher::new("Judith Butler", 0, 1),
    Philosopher::new("Gilles Deleuze", 1, 2),
    Philosopher::new("Karl Marx", 2, 3),
    Philosopher::new("Emma Goldman", 3, 4),
    Philosopher::new("Michel Foucault", 4, 0),
    ];

  // for p in &philosophers {
  //   p.eat();
  // }

  let handles: Vec<_> = philosophers.into_iter().map(|p| {
    let table = table.clone();

    thread::spawn(move || {
      p.eat(&table);
    })
  }).collect();

  for h in handles {
    h.join().unwrap();
  }
}
