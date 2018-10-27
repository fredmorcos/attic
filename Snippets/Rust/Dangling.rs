fn dangling () -> ~int {
  let i = ~1234;
  return i;
}

fn add_one () -> int {
  let num = dangling ();
  return *num + 1;
}

fn main () {
  println!("{}", add_one ());
}
