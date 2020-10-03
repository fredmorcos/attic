use derive_more::{Display, From};
use derive_new::new;
use io::{Bytes, Read};
use std::str::FromStr;
use std::{io, num};

#[derive(Debug, Display, Clone, Copy)]
#[display(fmt = "at line {} column {}", line, col)]
pub struct Pos {
  line: usize,
  col: usize,
}

impl Pos {
  fn inc(&mut self, c: u8) {
    if c == b'\n' {
      self.line += 1;
      self.col = 1;
    } else {
      self.col += 1;
    }
  }
}

impl Default for Pos {
  fn default() -> Self {
    Self { line: 1, col: 1 }
  }
}

#[derive(Debug, Display, Clone, Copy)]
pub enum Jump {
  JGT,
  JEQ,
  JGE,
  JLT,
  JNE,
  JLE,
  JMP,
}

impl FromStr for Jump {
  type Err = ();

  fn from_str(value: &str) -> Result<Self, Self::Err> {
    match value {
      "JGT" => Ok(Jump::JGT),
      "JEQ" => Ok(Jump::JEQ),
      "JGE" => Ok(Jump::JGE),
      "JLT" => Ok(Jump::JLT),
      "JNE" => Ok(Jump::JNE),
      "JLE" => Ok(Jump::JLE),
      "JMP" => Ok(Jump::JMP),
      _ => Err(()),
    }
  }
}

#[derive(Debug, Display, Clone, Copy)]
#[display(fmt = ":Address")]
pub struct KindAddress;

#[derive(Debug, Display, Clone, Copy)]
#[display(fmt = ":Data")]
pub struct KindData;

#[derive(Debug, Display, Clone, Copy)]
#[display(fmt = ":Memory")]
pub struct KindMemory;

#[derive(Debug, Display, Clone, Copy)]
#[display(fmt = ":{{0}}")]
pub struct KindZero;

#[derive(Debug, Display, Clone, Copy)]
#[display(fmt = ":{{1}}")]
pub struct KindOne;

pub trait Compatible<T>: Sized {
  fn is_compatible(self) -> bool {
    true
  }
}

impl Compatible<KindData> for KindAddress {}
impl Compatible<KindData> for KindMemory {}
impl Compatible<KindData> for KindZero {}
impl Compatible<KindData> for KindOne {}

impl Compatible<KindAddress> for KindData {}
impl Compatible<KindAddress> for KindZero {}
impl Compatible<KindAddress> for KindOne {}

impl Compatible<KindZero> for KindAddress {}
impl Compatible<KindZero> for KindData {}
impl Compatible<KindZero> for KindMemory {}

impl Compatible<KindOne> for KindAddress {}
impl Compatible<KindOne> for KindData {}
impl Compatible<KindOne> for KindMemory {}

impl Compatible<KindMemory> for KindData {}
impl Compatible<KindMemory> for KindZero {}
impl Compatible<KindMemory> for KindOne {}

impl Compatible<KindAddress> for KindMemory {
  fn is_compatible(self) -> bool {
    false
  }
}

impl Compatible<KindMemory> for KindAddress {
  fn is_compatible(self) -> bool {
    false
  }
}

#[derive(Debug, Display, From)]
#[display(fmt = "({})", _0)]
pub struct Label(String);

#[derive(Debug, Display, From)]
#[display(fmt = "@{}", _0)]
pub struct NamedAddr(String);

#[derive(Debug, Display, From, Clone, Copy)]
#[display(fmt = "@{}", _0)]
pub struct NumAddr(u16);

#[derive(Debug, Display, From, Clone, Copy)]
pub enum RegAddr {
  #[display(fmt = "R0")]
  R0,
  #[display(fmt = "R1")]
  R1,
  #[display(fmt = "R2")]
  R2,
  #[display(fmt = "R3")]
  R3,
  #[display(fmt = "R4")]
  R4,
  #[display(fmt = "R5")]
  R5,
  #[display(fmt = "R6")]
  R6,
  #[display(fmt = "R7")]
  R7,
  #[display(fmt = "R8")]
  R8,
  #[display(fmt = "R9")]
  R9,
  #[display(fmt = "R10")]
  R10,
  #[display(fmt = "R11")]
  R11,
  #[display(fmt = "R12")]
  R12,
  #[display(fmt = "R13")]
  R13,
  #[display(fmt = "R14")]
  R14,
  #[display(fmt = "R15")]
  R15,
}

impl FromStr for RegAddr {
  type Err = ();

  fn from_str(s: &str) -> Result<Self, Self::Err> {
    match s {
      "R0" => Ok(Self::R0),
      "R1" => Ok(Self::R1),
      "R2" => Ok(Self::R2),
      "R3" => Ok(Self::R3),
      "R4" => Ok(Self::R4),
      "R5" => Ok(Self::R5),
      "R6" => Ok(Self::R6),
      "R7" => Ok(Self::R7),
      "R8" => Ok(Self::R8),
      "R9" => Ok(Self::R9),
      "R10" => Ok(Self::R10),
      "R11" => Ok(Self::R11),
      "R12" => Ok(Self::R12),
      "R13" => Ok(Self::R13),
      "R14" => Ok(Self::R14),
      "R15" => Ok(Self::R15),
      _ => Err(()),
    }
  }
}

#[derive(Debug, Display)]
pub enum Token {
  #[display(fmt = "comment \"{}\" {}", comment, pos)]
  Comment { comment: String, pos: Pos },
  #[display(fmt = "label \"{}\" {})", label, pos)]
  Label { label: Label, pos: Pos },
  #[display(fmt = "named address \"{}\" {}", addr, pos)]
  NamedAddr { addr: NamedAddr, pos: Pos },
  #[display(fmt = "numerical address \"{}\" {}", addr, pos)]
  NumAddr { addr: NumAddr, pos: Pos },
  #[display(fmt = "register address \"{}\" {}", addr, pos)]
  RegAddr { addr: RegAddr, pos: Pos },

  #[display(fmt = "jump \"{}\" {}", jump, pos)]
  Jump { jump: Jump, pos: Pos },

  #[display(fmt = "variable of type {} \"A\" {}", kind, pos)]
  A { kind: KindAddress, pos: Pos },
  #[display(fmt = "variable of type {} \"M\" {}", kind, pos)]
  M { kind: KindMemory, pos: Pos },
  #[display(fmt = "variable of type {} \"D\" {}", kind, pos)]
  D { kind: KindData, pos: Pos },

  #[display(fmt = "one of type {} \"1\" {}", kind, pos)]
  One { kind: KindOne, pos: Pos },
  #[display(fmt = "zero of type {} \"0\" {}", kind, pos)]
  Zero { kind: KindZero, pos: Pos },

  #[display(fmt = "assignment \"=\" {}", _0)]
  Assign(Pos),
  #[display(fmt = "separator \";\" {}", _0)]
  Sep(Pos),

  #[display(fmt = "minus \"-\" {}", _0)]
  Minus(Pos),
  #[display(fmt = "plus \"+\" {}", _0)]
  Plus(Pos),
  #[display(fmt = "not \"!\" {}", _0)]
  Not(Pos),
  #[display(fmt = "and \"&\" {}", _0)]
  And(Pos),
  #[display(fmt = "and \"|\" {}", _0)]
  Or(Pos),
}

#[derive(Debug, Display)]
pub enum TokenizerError {
  #[display(fmt = "IO Error: {}", _0)]
  IO(io::Error),
  #[display(fmt = "Invalid input '{}' {}, expecting {}.", _1, _0, _2)]
  Invalid(Pos, char, &'static str),
  #[display(fmt = "Invalid instruction jump '{}' {}", _1, _0)]
  Jump(Pos, String),
  #[display(fmt = "Number error: {}", _0)]
  Number(num::ParseIntError),
}

#[derive(new)]
pub struct Tokenizer<R: Read> {
  bytes: Bytes<R>,
  #[new(default)]
  pos: Pos,
}

macro_rules! next {
  ($e:expr,$p:expr) => {
    match $e.next() {
      Some(Ok(c)) => {
        $p.inc(c);
        c
      }
      Some(Err(e)) => return Some(Err(TokenizerError::IO(e))),
      None => return None,
    };
  };
}

impl<R: Read> Tokenizer<R> {
  fn read_comment(
    bytes: &mut Bytes<R>, pos: &mut Pos, start_pos: Pos,
  ) -> Option<Result<Token, TokenizerError>> {
    let c2 = next!(bytes, pos);
    if c2 == b'/' {
      // Found a comment.
      let mut tok = String::with_capacity(10);
      tok.push('/');
      tok.push(char::from(c2));

      // Read until EOL.
      loop {
        let c = next!(bytes, pos);
        if c == b'\n' {
          break;
        }
        tok.push(char::from(c));
      }

      Some(Ok(Token::Comment {
        comment: tok,
        pos: start_pos,
      }))
    } else {
      Some(Err(TokenizerError::Invalid(
        *pos,
        char::from(c2),
        "another '/', constituting a comment",
      )))
    }
  }

  fn read_addr(
    bytes: &mut Bytes<R>, pos: &mut Pos, start_pos: Pos,
  ) -> Option<Result<Token, TokenizerError>> {
    let c2 = next!(bytes, pos);
    if c2.is_ascii_digit() {
      // Found a numerical address.
      let mut tok = String::with_capacity(10);
      tok.push(char::from(c2));

      // read until non-digit character.
      let c = loop {
        let c = next!(bytes, pos);
        if !c.is_ascii_digit() {
          break c;
        }
        tok.push(char::from(c));
      };

      if !c.is_ascii_whitespace() && c != b'\n' {
        return Some(Err(TokenizerError::Invalid(
          *pos,
          char::from(c),
          "whitespace or newline",
        )));
      }

      match u16::from_str_radix(&tok, 10) {
        Ok(addr) => Some(Ok(Token::NumAddr {
          addr: NumAddr::from(addr),
          pos: start_pos,
        })),
        Err(e) => Some(Err(TokenizerError::Number(e))),
      }
    } else if c2.is_ascii_alphanumeric() {
      // Found a named address.
      let mut tok = String::with_capacity(10);
      tok.push(char::from(c2));

      // read until newline or whitespace
      loop {
        let c = next!(bytes, pos);
        if c.is_ascii_whitespace() || c == b'\n' {
          break;
        }
        tok.push(char::from(c));
      }

      if let Ok(reg_addr) = RegAddr::from_str(&tok) {
        return Some(Ok(Token::RegAddr {
          addr: reg_addr,
          pos: start_pos,
        }));
      }

      Some(Ok(Token::NamedAddr {
        addr: NamedAddr::from(tok),
        pos: start_pos,
      }))
    } else {
      Some(Err(TokenizerError::Invalid(
        *pos,
        char::from(c2),
        "an alpha-numeric character to constitute a named or unnamed address",
      )))
    }
  }

  fn read_label(
    bytes: &mut Bytes<R>, pos: &mut Pos, start_pos: Pos,
  ) -> Option<Result<Token, TokenizerError>> {
    let c2 = next!(bytes, pos);
    if !c2.is_ascii_alphabetic() {
      return Some(Err(TokenizerError::Invalid(
        *pos,
        char::from(c2),
        "an alphabetic character to constitute a label",
      )));
    }

    // Found a label.
    let mut tok = String::with_capacity(10);
    tok.push(char::from(c2));

    // read until closing paren.
    loop {
      let c = next!(bytes, pos);
      if c == b')' {
        break;
      } else if c.is_ascii_whitespace() || c == b'\n' {
        return Some(Err(TokenizerError::Invalid(
          *pos,
          char::from(c),
          "a closing parenthesis ')' character to constitute a label",
        )));
      }
      tok.push(char::from(c));
    }

    Some(Ok(Token::Label {
      label: Label::from(tok),
      pos: start_pos,
    }))
  }

  // fn read_dest_or_comp(
  //   bytes: &mut Bytes<R>, pos: &mut Pos, start_pos: Pos, c1: u8,
  // ) -> Option<Result<Token, TokenizerError>> {
  //   // Found either a destination or a computation.
  //   let mut tok = String::with_capacity(3);
  //   tok.push(char::from(c1));

  //   // read until end of destination, computation or instruction.
  //   loop {
  //     let c = next!(bytes, pos);
  //     if c == b'=' {
  //       // Found a destination.
  //       if let Ok(dest) = Dest::try_from(tok.as_str()) {
  //         return Some(Ok(Token::Dest {
  //           dest,
  //           pos: start_pos,
  //         }));
  //       } else {
  //         return Some(Err(TokenizerError::Dest(*pos, tok)));
  //       }
  //     } else if c == b';' || c.is_ascii_whitespace() || c == b'\n' {
  //       // Found a computation.
  //       if let Ok(comp) = Comp::try_from(tok.as_str()) {
  //         return Some(Ok(Token::Comp {
  //           comp,
  //           pos: start_pos,
  //         }));
  //       } else {
  //         return Some(Err(TokenizerError::Comp(*pos, tok)));
  //       }
  //     }
  //     tok.push(char::from(c));
  //   }
  // }

  fn read_jump(
    bytes: &mut Bytes<R>, pos: &mut Pos, start_pos: Pos,
  ) -> Option<Result<Token, TokenizerError>> {
    // Found a jump.
    let mut tok = String::with_capacity(3);
    tok.push('J');

    loop {
      let c = next!(bytes, pos);
      if c.is_ascii_whitespace() || c == b'\n' {
        if let Ok(jump) = Jump::from_str(tok.as_str()) {
          return Some(Ok(Token::Jump {
            jump,
            pos: start_pos,
          }));
        } else {
          return Some(Err(TokenizerError::Jump(*pos, tok)));
        }
      }
      tok.push(char::from(c));
    }
  }
}

impl<R: Read> Iterator for Tokenizer<R> {
  type Item = Result<Token, TokenizerError>;

  fn next(&mut self) -> Option<Self::Item> {
    let (c1, pos) = loop {
      let pos = self.pos;
      let c1 = next!(self.bytes, self.pos);
      if !c1.is_ascii_whitespace() {
        break (c1, pos);
      }
    };

    match c1 {
      b'/' => Tokenizer::read_comment(&mut self.bytes, &mut self.pos, pos),
      b'@' => Tokenizer::read_addr(&mut self.bytes, &mut self.pos, pos),
      b'(' => Tokenizer::read_label(&mut self.bytes, &mut self.pos, pos),
      b'J' => Tokenizer::read_jump(&mut self.bytes, &mut self.pos, pos),
      b'A' => Some(Ok(Token::A {
        kind: KindAddress,
        pos,
      })),
      b'M' => Some(Ok(Token::M {
        kind: KindMemory,
        pos,
      })),
      b'D' => Some(Ok(Token::D {
        kind: KindData,
        pos,
      })),
      b'0' => Some(Ok(Token::Zero {
        kind: KindZero,
        pos,
      })),
      b'1' => Some(Ok(Token::One { kind: KindOne, pos })),
      b'-' => Some(Ok(Token::Minus(pos))),
      b'!' => Some(Ok(Token::Not(pos))),
      b'+' => Some(Ok(Token::Plus(pos))),
      b'|' => Some(Ok(Token::Or(pos))),
      b'&' => Some(Ok(Token::And(pos))),
      b'=' => Some(Ok(Token::Assign(pos))),
      b';' => Some(Ok(Token::Sep(pos))),
      _ => Some(Err(TokenizerError::Invalid(
        self.pos,
        char::from(c1),
        "a comment, address, label or instruction",
      ))),
    }
  }
}
