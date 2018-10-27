use std::io;
use std::io::Write;

use std::cmp::Ordering;

fn main() {
  // let mut x = vec!["one", "two"];
  //
  // {
  //   let y = &x[0];
  // }
  //
  // let z = x.push("three");
  //
  // println!("hello!");

  let num = 42;

  loop {
    print!("Guess a number: ");
    io::Stdout::flush(&mut io::stdout()).ok();

    let mut guess = String::new();

    io::stdin().read_line(&mut guess)
      .ok()
      .expect("Failed to read line");

    println!("You guessed: {}", guess.trim());

    let guess: u32 = match guess.trim().parse() {
      Ok(num) => num,
      Err(e) => {
        println!("Error: {}", e);
        continue;
      }
    };

    match guess.cmp(&num) {
      Ordering::Less    => println!("Too small!"),
      Ordering::Greater => println!("Too big!"),
      Ordering::Equal   => {
        println!("You win!");
        break;
      }
    }
  }
}
