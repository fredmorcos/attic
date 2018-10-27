use std::io::{self, BufRead, Write};
use std::str::FromStr;
use std::num::ParseIntError;
use std::fmt::{self, Display};
use std::iter::Peekable;

#[derive(Debug)]
pub enum Tok {
  Iden {
    loc: usize,
    name: String,
  },

  OpAssign(usize),

  LitInt {
    loc: usize,
    val_str: String,
    val: u128,
  },
}

fn skip_ws<I>(iter: &mut Peekable<I>)
  where I: Iterator<Item = (usize, char)>
{
  loop {
    let (_, c) = {
      match iter.peek() {
        Some(v) => v,
        None => return,
      }
    };

    if !c.is_whitespace() {
      return;
    }

    iter.next();
  }
}

enum TokErr {
  NumParse(ParseIntError),
}

impl Display for TokErr {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self {
      TokErr::NumParse(e) => write!(f, "Cannot parse number: {}", e),
    }
  }
}

fn tok_num<I>(iter: &mut Peekable<I>, loc: usize) -> Result<Tok, TokErr>
  where I: Iterator<Item = (usize, char)>
{
  let mut val_str: String = String::new();

  for (_, c) in iter.take_while(|(_, c)| c.is_digit(10)) {
    val_str.push(c);
  }

  let val = u128::from_str(&val_str).map_err(TokErr::NumParse)?;

  Ok(Tok::LitInt { loc, val_str, val })
}

fn tok_id<I>(iter: &mut Peekable<I>, loc: usize) -> Result<Tok, TokErr>
  where I: Iterator<Item = (usize, char)>
{
  let mut name: String = String::new();

  fn allowed(c: char) -> bool {
    !c.is_whitespace() && c != '='
  }

  for (_, c) in iter.take_while(|(_, c)| allowed(*c)) {
    name.push(c);
  }

  Ok(Tok::Iden { loc, name })
}

fn tok<I>(iter: &mut Peekable<I>, toks: &mut Vec<Tok>) -> Option<TokErr>
  where I: Iterator<Item = (usize, char)>
{
  skip_ws(iter);

  if let Some(&(loc, c)) = iter.peek() {
    if c == '=' {               // Assign operator
      toks.push(Tok::OpAssign(loc));
      iter.next();
    } else if c.is_digit(10) {  // Number
      let res = match tok_num(iter, loc) {
        Ok(res) => res,
        Err(err) => return Some(err),
      };

      toks.push(res);
    } else {                    // Identifier
      let res = match tok_id(iter, loc) {
        Ok(res) => res,
        Err(err) => return Some(err),
      };

      toks.push(res);
    }

    return tok(iter, toks);
  }

  None
}

fn main() {
  let stdout = io::stdout();
  let mut stdout = stdout.lock();

  let stdin = io::stdin();
  let mut stdin = stdin.lock();

  let mut input: String = String::new();

  loop {
    match stdout.write(b"> ") {
      Ok(_len) => {}
      Err(err) => {
        eprintln!("Error writing to stdout: {}", err);
        break;
      }
    }

    match stdout.flush() {
      Ok(()) => {}
      Err(err) => {
        eprintln!("Error flusing to stdout: {}", err);
        break;
      }
    }

    match stdin.read_line(&mut input) {
      Ok(0) => {
        println!("exit");
        break;
      },
      Ok(_len) => {
        let mut toks: Vec<Tok> = Vec::new();
        let mut iter = input.chars().enumerate().peekable();

        match tok(&mut iter, &mut toks) {
          Some(err) => eprintln!("Tokenization error: {}", err),
          None => {
            println!("tokens = {:#?}", toks);
          },
        }

        input.clear();
      }
      Err(err) => {
        println!("Error reading from stdin: {}", err);
        break;
      }
    }
  }
}
