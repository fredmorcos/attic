fn main() {
  let add_one = |x| x + 1;
  let five = add_one(4);
  assert_eq!(5, five);

  let calculate = |a, b| {
    let mut result = a * 2;
    result += b;
    result
  };

  assert_eq!(7, calculate(2, 3));
  assert_eq!(13, calculate(4, 5));

  let add_one = |x: i32| -> i32 { x + 1 };
  assert_eq!(2, add_one(1));

  let x = 4;
  let eq_to_x = |z| z == x;
  let y = 4;
  assert!(eq_to_x(y));

  let answer = call_with_one(|x| x + 2);
  assert_eq!(3, answer);

  let v1: Vec<i32> = vec![1, 2, 3];
  let v2: Vec<i32> = v1.iter().map(|x| x + 1).collect();
  assert_eq!(v2, [2, 3, 4]);

  let counter = Counter::new();
  for i in counter {
    println!("{}", i);
  }

  let sum: u32 = Counter::new()
    .take(5)
    .zip(Counter::new().skip(1))
    .map(|(a, b)| a * b)
    .filter(|x| x % 3 == 0)
    .sum();
  assert_eq!(18, sum);
}

fn call_with_one<F>(some_closure: F) -> i32
where
  F: Fn(i32) -> i32,
{
  some_closure(1)
}

struct Counter {
  count: u32,
}

impl Counter {
  fn new() -> Counter {
    Counter { count: 0 }
  }
}

impl Iterator for Counter {
  type Item = u32;

  fn next(&mut self) -> Option<Self::Item> {
    self.count += 1;

    if self.count < 6 {
      Some(self.count)
    } else {
      None
    }
  }
}
