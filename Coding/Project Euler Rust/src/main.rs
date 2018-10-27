extern crate integer_sqrt;
use integer_sqrt::IntegerSquareRoot;

fn p1(n: u64) -> u64 {
  let mut sum = 0;

  for i in 0 .. n {
    if i % 3 == 0 || i %5 == 0 {
      sum += i;
    }
  }

  sum
}

fn p2() -> u64 {
  let mut sum = 2;
  let mut prev = 2;
  let mut i = 3;

  loop {
    let cur = prev + i;

    if cur > 4000000 {
      break;
    } else if cur % 2 == 0 {
      sum += cur
    }

    prev = i;
    i = cur;
  }

  sum
}

fn p3(n: u64) -> u64 {
  fn is_prime(n: u64) -> bool {
    for i in 2 .. n.integer_sqrt() {
      if n % i == 0 {
        return false;
      }
    }

    true
  }

  let mut factors = vec![];

  for i in 2 .. n.integer_sqrt() {
    if n % i == 0 && is_prime(i) {
      factors.push(i);
    }
  }

  eprintln!("DEBUG: factors({}) = {:?}", n, factors);

  *factors.last().unwrap()
}

fn p4() -> u64 {
  fn is_palindrome(n: u64) -> bool {
    let mut s = format!("{}", n);
    let n_str: &mut Vec<u8> = unsafe { s.as_mut_vec() };
    let mut n_str_rev: Vec<u8> = n_str.clone();
    n_str_rev.reverse();
    n_str == &mut n_str_rev
  }

  let mut last = 0;

  for i in 100 .. 1000 {
    for j in 100 .. 1000 {
      if is_palindrome(i * j) && i * j > last {
        last = i * j;
      }
    }
  }

  last
}

fn main() {
  println!("p1(10) = {}", p1(10));
  println!("p1(1000) = {}", p1(1000));
  println!("p2() = {}", p2());
  println!("p3(13195) = {}", p3(13195));
  println!("p3(600851475143) = {}", p3(600851475143));
  println!("p4() = {}", p4());
}
