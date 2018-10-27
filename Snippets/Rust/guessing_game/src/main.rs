extern crate rand;

use std::io;
use std::io::Write;
use std::cmp::Ordering;

use rand::Rng;

fn main() {
  let secret_num = rand::thread_rng().gen_range(1, 101);
  // println!("Secret number is {}", secret_num);

  loop {
    print!("Guess a number: ");

    match io::stdout().flush() {
      Ok(_) => (),
      Err(e) => {
        println!("Warning: Could not flush stdout: {}", e);
        ()
      }
    }

    let mut guess: String = String::new();

    match io::stdin().read_line(&mut guess) {
      Ok(_) => (),
      Err(e) => {
        println!("Error: Failed to read line: {}", e);
        std::process::exit(1);
      }
    }

    let guess = guess.trim();

    let guess: u8 = match guess.parse() {
      Ok(n) => n,
      Err(_) => {
        match guess.cmp("quit") {
          Ordering::Equal => {
            println!("You surrender");
            break;
          }
          _ => continue,
        }
      }
    };

    println!("You guessed: {}", guess);

    match guess.cmp(&secret_num) {
      Ordering::Less => println!("Too small"),
      Ordering::Greater => println!("Too big"),
      Ordering::Equal => {
        println!("You win!");
        break;
      }
    }
  }
}
