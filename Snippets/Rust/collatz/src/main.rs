fn collatz(mut n: u64) {
  while n != 1 {
    if n % 2 == 0 {
      n /= 2
    } else {
      n = (3 * n) + 1
    }
  }
}

fn main() {
  let mut i: u64 = 11360000000000;
  let max: u64 = u64::max_value();

  while i <= max {
    if i % 10000000000 == 0 {
      println!("{}", i);
    }

    collatz(i);
    i += 1;
  }
}
