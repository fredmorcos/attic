fn main() {
  let mut vec: Vec<u8> = Vec::new();
  vec.push(1);
  vec.push(2);
  let mut vec: Vec<u8> = take(vec);
  vec.push(4);
  println!("{:?}", vec);
}

fn take(mut vec: Vec<u8>) -> Vec<u8> {
  vec.push(3);
  return vec;
}
