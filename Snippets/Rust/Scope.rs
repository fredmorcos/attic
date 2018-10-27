extern crate serialize;
use serialize::json;

fn main() {
  // let x: u32 = 5;                  //
  // let z: Box<u32> = Box::new(5);   //
  //                                  //
  // {                                //
  //   let y: &u32 = &x;              //
  //   let a: &Box<u32> = &z;         //
  //   let b: Box<u32> = Box::new(x); //
  // }                                //

  // let mut v = vec![];          //
  // v.push("Hello".to_string()); //
  // let mut x = v[0].clone();    //
  // v.push("world".to_string()); //
  // println!("{}", x);           //
  // x = "foo".to_string();       //
  // println!("{}", x);           //

  let x = 5;
  let y = &x;

  println!("x = {} y = {}", x, y);
  println!("x = {} y = {}", x, y);
}
