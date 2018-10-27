use std::os;
use std::num::sqrt;
use std::task::spawn;

enum List {
  Cons(int, ~List),
  Nil
}

struct Point {
  x: f64,
  y: f64
}

fn distance (p1: &Point, p2: &Point) -> f64 {
  let x_d = p1.x - p2.x;
  let y_d = p1.y - p2.y;
  sqrt(x_d * x_d + y_d * y_d)
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

  let on_stack :  Point =  Point { x: 3.0, y: 4.0 };
  // let managed  : @Point = @Point { x: 5.0, y: 6.0 };
  let owned    : ~Point = ~Point { x: 7.0, y: 9.0 };

  println!("{}", distance(&on_stack, owned));

  let more = [1,2,3];
  let numbers = &more;

  // more[0] = 2;

  // numbers[0] = 2;

  i = 0;

  while i < numbers.len() {
    println!("{}", numbers[i]);
    i += 1;
  }

  let foo = "foobar";
  let bar: &str = foo.slice(1,3);

  println!("{}", bar);

  let mut l1 = [1,2,3];
  let view = l1.mut_slice(0,3);
  view[0]=5;

  i = 0;

  while i < view.len() {
    println!("{}", view[i]);
    i += 1;
  }

  spawn(proc() {
    let mut i = 0;
    while i < 10 {
      println!(".");
      i += 1;
    }
  });

  spawn(proc() {
    let mut i = 0;
    while i < 10 {
      println!("o");
      i += 1;
    }
  });
}

fn foo () -> int {
  5 + 3
}
