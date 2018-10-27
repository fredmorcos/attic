fn main() {
  const FOO: u8 = 255;

  let name = "Fred";
  println!("Hello, {}!", name);

  let mut x = 256;
  let mut y = x;
  println!("x = {}, y = {}", x, y);
  x = 10;
  println!("x = {}, y = {}", x, y);

  let x = 2;
  let y = 3;
  println!("x = {}, y = {}", x, y);

  let x = y;
  let y = 200;
  println!("x = {}, y = {}", x, y);

  let t: (u32, u32) = (5, 6);
  println!("t = {:?}", t);

  let t: (_, _, _) = (5, 6, 'A');
  println!("t = {:?}", t);
  println!("first of t = {}", t.0);
  println!("second of t = {}", t.1);
  println!("third of t = {}", t.2);

  let mut t: (_, _, char) = (5, 6, 'A');
  t.0 = 7;
  println!("t = {:?}", t);
  let x: u32 = 256;
  t.0 = x;
  println!("t = {:?}", t);

  let i: u64 = t.0.into();

  let arr = [1,2,3,4,5];
  println!("arr = {:?} of length = {}", arr, arr.len());
  let mut arr: [u8; 5] = arr;
  arr[0] = 3;
  println!("arr = {:?} of length = {}", arr, arr.len());

  let index_a: usize = 10;
  // let _ = arr[index_a];   // runtime panic

  // hello
  /* commment */

  /// this is that and this and that and hell world and foobar and
  /// foobaz and hello kevin and benny
  let x = 5;

  const INDEX_B: usize = 10;
  // let _ = arr[INDEX_B];   // runtime panic, compile-time error only from linter

  hello();
  hello2(5);
  println!("hello3(5, 6) -> {}", hello3(5, 6));

  let x = 5;
  if x == 5 {
    println!("x is 5");
  } else {
    println!("x is not 5");
  }

  let y = if x == 5 {
    x + 1
  } else {
    x + 2
  };
}

fn hello() {
  println!("hello()");
}

fn hello2(x: u8) {
  println!("hello2({})", x);
}

fn hello3(x: u8, y: u32) -> u32 {
  u32::from(x) + y
}
