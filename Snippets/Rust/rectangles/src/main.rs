#[derive(Debug)]
struct Rect {
  len: u32,
  wid: u32,
}

impl Rect {
  fn area(&self) -> u32 {
    self.len * self.wid
  }

  fn can_hold(&self, other: &Rect) -> bool {
    self.len >= other.len && self.wid >= other.wid
  }

  fn square(size: u32) -> Rect {
    Rect {
      len: size,
      wid: size,
    }
  }
}

impl Rect {
  fn perim(&self) -> u32 {
    self.len * 2 + self.wid * 2
  }
}

#[derive(Debug)]
#[allow(dead_code)]
enum State {
  Alabama,
  Alaska,
  Illinois,
}

#[allow(dead_code)]
enum Coin {
  Penny,
  Nickel,
  Dime,
  Quarter(State),
}

impl Coin {
  fn in_cents(&self) -> i32 {
    match *self {
      Coin::Penny => 1,
      Coin::Nickel => 5,
      Coin::Dime => 10,
      Coin::Quarter(State::Alabama) => {
        println!("Alabama!");
        25
      }
      Coin::Quarter(State::Alaska) => {
        println!("Alaska!");
        25
      }
      Coin::Quarter(ref state) => {
        println!("State: {:?}", state);
        25
      }
    }
  }
}

fn main() {
  let len1 = 50;
  let wid1 = 30;

  println!(
    "The area of the rectangle is {} square pixels",
    area(len1, wid1)
  );

  let rect1 = (50, 30);

  println!(
    "The area of the rectangle is {} square pixels",
    area_tup(rect1)
  );

  let rect2: Rect = Rect { len: 50, wid: 30 };

  println!(
    "The area of the rectangle is {} square pixels",
    area_struct(&rect2)
  );

  println!("Rect is {:?}", &rect2);
  println!("Rect is {:#?}", &rect2);

  println!("Rect area is {}", rect2.area());
  println!("Rect perim is {}", rect2.perim());

  let rect3 = Rect { len: 40, wid: 20 };

  println!("Rect2 can hold Rect3? {}", rect2.can_hold(&rect3));

  let rect4 = Rect { len: 60, wid: 20 };

  println!("Rect2 can hold Rect4? {}", rect2.can_hold(&rect4));

  let sq = Rect::square(60);

  println!("Sq is {:#?}", &sq);

  let c: Coin = Coin::Quarter(State::Alaska);
  println!("Quarter in cents: {}", c.in_cents());
}

fn area(len: u32, wid: u32) -> u32 {
  len * wid
}

fn area_tup(rect: (u32, u32)) -> u32 {
  rect.0 * rect.1
}

fn area_struct(rect: &Rect) -> u32 {
  rect.len * rect.wid
}
