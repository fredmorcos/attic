// use std::ops::Add;

fn main() {
  let a = ["Hello", "World", "!"];
  let mut b = a;
  b[1] = "Fred";
  // let a = ["Foo", "Bar", "!"];
  println!("{:?}", a);
  println!("{:?}", b);
  println!("addrof a[0] = {:?}, b[0] = {:?}",
           a[0] as *const _,
           b[0] as *const _);
  println!("addrof a[1] = {:?}, b[1] = {:?}",
           a[1] as *const _,
           b[1] as *const _);
  println!("addrof a = {:?}, b = {:?}", &a as *const _, &b as *const _);

  let s: String = String::from("hello");
  // s.push_str(", world!");
  // let s2: String = s.add("foo");
  let s2: String = s.clone() + "foo";
  println!("{}", s);
  println!("{}", s2);
  let s3: String = s + "foo";
  println!("{}", s3);

  {
    let mut s4: &mut String = &mut s2.clone();
    // println!("{}", s2);
    s4.push_str("baz");
    println!("{}", s4);
  }

  println!("{}", &s2);

  let i1: i32 = 5;
  let i2: &i32 = &i1;

  let i3: i32 = dontmodify(*i2);
  let i4: i32 = domodify(&mut i3.clone());

  println!("{}", i4);

  let s = String::from("Hello world!");
  let w = first_word(&s);
  println!("first word = {}", w);

  let w2 = first_word_str(&s);
  println!("first word = {}", w2);

  let w3 = first_word_slice(&s[..]);
  println!("first word = {}", w3);
}

fn first_word(s: &String) -> usize {
  let bytes = s.as_bytes();

  for (i, &c) in bytes.iter().enumerate() {
    if char::from(c).is_whitespace() {
      return i;
    }
  }

  s.len()
}

fn first_word_str(s: &String) -> &str {
  let bytes = s.as_bytes();

  for (i, &c) in bytes.iter().enumerate() {
    if char::from(c).is_whitespace() {
      return &s[..i];
    }
  }

  &s[..]
}

fn first_word_slice(s: &str) -> &str {
  let bytes = s.as_bytes();

  for (i, &c) in bytes.iter().enumerate() {
    if char::from(c).is_whitespace() {
      return &s[..i];
    }
  }

  &s[..]
}

fn dontmodify(x: i32) -> i32 {
  x
}

fn domodify(x: &mut i32) -> i32 {
  *x = 42;
  *x
}
