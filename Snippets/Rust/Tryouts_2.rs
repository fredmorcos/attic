use std::os;

enum List {
  Cons(int, ~List),
  Nil
}

fn main () {
  let mut x : uint = 5u;
  loop {
    // x += x - 3;
    x += x - 6;
    if x % 5 == 0 { break; }
    println!("{}", x);
  }

  println!("{}", foo());

  let mut i = 0;
  let args = os::args();

  // println!("{}", args[-1]);

  while i < args.len() {
    println!("{}", args[i]);
    i += 1;
  }

  let mut l = ~Cons(1, ~Cons(2, ~Cons(3, ~Nil)));
  // let mut c = ~l;

  loop {
    match l {
      ~Cons(_, next) => {
        println!("found one!");
        l = next;
      }
      ~Nil => {
        break;
      }
    }
  }
}

fn foo () -> int {
  5 + 3
}
