fn test(x: &mut u8) {
  *x = 3;
}

/// tests something
///
/// # Examples
/// ```
/// let five = 5;
/// assert_eq!(6, 5 + 1);
/// ```
fn test2(x: &mut Vec<u8>) {
  x.push(32);
}

fn main() {
  let mut x: u8 = 5;
  test(&mut x);
  println!("{}", x);

  let mut y: Vec<u8> = Vec::new();
  y.push(3);
  y.push(4);

  for i in &y {
    print!("{}, ", i);
  }
  println!("");

  test2(&mut y);

  for i in &y {
    print!("{}, ", i);
  }
  println!("");

  let z: [u8; 3] = [1, 2, 3];

  for i in z.iter() {
    print!("{}, ", i);
  }
  println!("");

  let mut v: Vec<u8> = Vec::new();
  let v2: &mut Vec<u8> = &mut v;

  v2.push(3);

  for i in v2 {
    print!("{}, ", i);
  }
  println!("");
}
