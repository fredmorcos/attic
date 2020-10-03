use std::io;
use std::io::Bytes;
use std::io::Read;
use std::string::FromUtf8Error;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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

#[derive(Debug, PartialEq, Eq)]
pub enum Token {
  Whitespace(Vec<u8>),
  Newline(Vec<u8>),
  LParen(Pos),
  RParen(Pos),
  LBracket(Pos),
  RBracket(Pos),
  Nil(Pos),
  Dot(Pos),
  Symbol(Pos, Vec<u8>),
  Comment(Pos, String),
  String(Pos, String),
}

pub struct Lexer<R: Read> {
  bytes: Bytes<R>,
  pos: Pos,
  la: Option<u8>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Err {
  IO(Pos, io::ErrorKind),
  UTF8(Pos, FromUtf8Error),
  IncompleteString(Vec<u8>, Pos, Pos),
}

impl<R: Read> Lexer<R> {
  pub fn new(bytes: Bytes<R>) -> Self {
    Self {
      bytes,
      pos: Pos::default(),
      la: Option::default(),
    }
  }

  pub fn pos(&self) -> Pos {
    self.pos
  }
}

impl<R: Read> Iterator for Lexer<R> {
  type Item = Result<Token, Err>;

  fn next(&mut self) -> Option<Self::Item> {
    macro_rules! next {
      ($b:block) => {
        match self.bytes.next() {
          Some(Ok(c)) => c,
          Some(Err(e)) => return Some(Err(Err::IO(self.pos, e.kind()))),
          None => $b,
        };
      };
    }

    let c1 = if let Some(current_byte) = self.la {
      let c = current_byte;
      self.la = None;
      c
    } else {
      next!({ return None })
    };

    if c1.is_ascii_whitespace() {
      if c1 == b'\n' {
        // Newlines.
        let mut text = vec![c1];
        self.pos.inc(c1);

        loop {
          let c2 = next!({ return Some(Ok(Token::Newline(text))) });

          if c2 == b'\n' {
            text.push(c2);
            self.pos.inc(c2);
          } else {
            self.la = Some(c2);
            break;
          }
        }

        Some(Ok(Token::Newline(text)))
      } else {
        // Whitespace.
        let mut text = vec![c1];
        self.pos.inc(c1);

        loop {
          let c2 = next!({ return Some(Ok(Token::Whitespace(text))) });

          if c2.is_ascii_whitespace() && c2 != b'\n' {
            text.push(c2);
            self.pos.inc(c2);
          } else {
            self.la = Some(c2);
            break;
          }
        }

        Some(Ok(Token::Whitespace(text)))
      }
    } else if c1 == b';' {
      // Comment.
      let mut text = vec![c1];
      let pos = self.pos;
      self.pos.inc(c1);

      loop {
        let c2 = next!({
          let text = match String::from_utf8(text) {
            Ok(t) => t,
            Err(e) => return Some(Err(Err::UTF8(pos, e))),
          };

          return Some(Ok(Token::Comment(pos, text)));
        });

        if c2 != b'\n' {
          text.push(c2);
          self.pos.inc(c2);
        } else {
          self.la = Some(c2);
          break;
        }
      }

      let text = match String::from_utf8(text) {
        Ok(t) => t,
        Err(e) => return Some(Err(Err::UTF8(pos, e))),
      };

      Some(Ok(Token::Comment(pos, text)))
    } else if c1 == b'(' {
      let pos = self.pos;
      self.pos.inc(c1);
      Some(Ok(Token::LParen(pos)))
    } else if c1 == b'[' {
      let pos = self.pos;
      self.pos.inc(c1);
      Some(Ok(Token::LBracket(pos)))
    } else if c1 == b')' {
      let pos = self.pos;
      self.pos.inc(c1);
      Some(Ok(Token::RParen(pos)))
    } else if c1 == b']' {
      let pos = self.pos;
      self.pos.inc(c1);
      Some(Ok(Token::RBracket(pos)))
    } else if c1 == b'.' {
      let pos = self.pos;
      self.pos.inc(c1);
      Some(Ok(Token::Dot(pos)))
    } else if c1 == b'"' {
      // String.
      let mut text = Vec::new();
      let pos = self.pos;
      self.pos.inc(c1);

      loop {
        let c2 = match self.bytes.next() {
          Some(Ok(c)) => c,
          Some(Err(e)) => return Some(Err(Err::IO(self.pos, e.kind()))),
          None => return Some(Err(Err::IncompleteString(text, pos, self.pos))),
        };

        if c2 == b'\n' {
          return Some(Err(Err::IncompleteString(text, pos, self.pos)));
        } else if c2 == b'"' {
          self.pos.inc(c2);

          let text = match String::from_utf8(text) {
            Ok(t) => t,
            Err(e) => return Some(Err(Err::UTF8(pos, e))),
          };

          return Some(Ok(Token::String(pos, text)));
        } else {
          text.push(c2);
          self.pos.inc(c2);
        }
      }
    } else {
      // Symbols.
      let mut text = vec![c1];
      let pos = self.pos;
      self.pos.inc(c1);

      loop {
        let c2 = next!({
          if text == b"nil" {
            return Some(Ok(Token::Nil(pos)));
          } else {
            return Some(Ok(Token::Symbol(pos, text)));
          }
        });

        if c2.is_ascii_whitespace()
          || c2.is_ascii_control()
          || c2 == b'('
          || c2 == b')'
          || c2 == b'['
          || c2 == b']'
          || c2 == b'"'
          || c2 == b'.'
          || c2 == b';'
        {
          self.la = Some(c2);
          break;
        } else {
          text.push(c2);
          self.pos.inc(c2);
        }
      }

      if text == b"nil" {
        Some(Ok(Token::Nil(pos)))
      } else {
        Some(Ok(Token::Symbol(pos, text)))
      }
    }
  }
}

#[cfg(test)]
mod tests {
  use std::fs::File;
  use std::io::BufReader;
  use std::io::Read;

  use super::Lexer;
  use super::Pos;
  use super::Token;

  macro_rules! lexer {
    ($f:expr) => {
      Lexer::new(BufReader::new(File::open(concat!("tests/data/", $f)).unwrap()).bytes())
    };
  }

  macro_rules! pos {
    ($l:expr,$c:expr) => {
      Pos { line: $l, col: $c }
    };
  }

  macro_rules! next {
    ($l:expr,$t:expr) => {
      assert_eq!($l.next(), Some(Ok($t)))
    };
  }

  macro_rules! str {
    ($l:expr,$c:expr,$s:expr) => {
      Token::String(pos!($l, $c), String::from($s))
    };
  }

  macro_rules! cmt {
    ($l:expr,$c:expr,$s:expr) => {
      Token::Comment(pos!($l, $c), String::from($s))
    };
  }

  macro_rules! ws {
    () => {
      Token::Whitespace(vec![b' ']);
    };
  }

  macro_rules! nl {
    () => {
      Token::Newline(vec![b'\n'])
    };
  }

  #[test]
  fn empty() {
    let mut lexer = lexer!("empty");
    assert_eq!(lexer.pos, pos!(1, 1));
    assert_eq!(lexer.next(), None);
    assert_eq!(lexer.pos, pos!(1, 1));
  }

  #[test]
  fn comments_only() {
    let mut lexer = lexer!("comments_only");

    next!(
      lexer,
      cmt!(1, 1, ";; -*- mode: Emacs-Lisp; coding: utf-8; -*-")
    );

    next!(lexer, nl!());
    assert_eq!(lexer.pos, pos!(2, 1));
    next!(lexer, cmt!(2, 1, ";;; file-format: 9"));
    next!(lexer, nl!());
  }

  #[test]
  fn single_entry() {
    let mut lexer = lexer!("single_entry");

    next!(lexer, Token::LBracket(pos!(1, 1)));
    next!(lexer, str!(1, 2, "Foo"));
    next!(lexer, ws!());
    next!(lexer, str!(1, 8, "Bar"));
    next!(lexer, ws!());
    next!(lexer, Token::Nil(pos!(1, 14)));
    next!(lexer, ws!());
    next!(lexer, Token::Nil(pos!(1, 18)));
    next!(lexer, ws!());
    next!(lexer, Token::LParen(pos!(1, 22)));
    next!(lexer, str!(1, 23, "Foo OrgFoo"));
    next!(lexer, Token::RParen(pos!(1, 35)));
    next!(lexer, ws!());
    next!(lexer, Token::LParen(pos!(1, 37)));
    next!(lexer, Token::LBracket(pos!(1, 38)));
    next!(lexer, str!(1, 39, "+43 234242 342342"));
    next!(lexer, ws!());
    next!(lexer, str!(1, 59, "+23123 123 123 123"));
    next!(lexer, Token::RBracket(pos!(1, 79)));
    next!(lexer, Token::RParen(pos!(1, 80)));
    next!(lexer, ws!());
    next!(lexer, Token::LParen(pos!(1, 82)));
    next!(lexer, Token::LBracket(pos!(1, 83)));
    next!(lexer, str!(1, 84, "home"));
    next!(lexer, ws!());
    next!(lexer, Token::LParen(pos!(1, 91)));
    next!(lexer, str!(1, 92, "Streetname 34"));
    next!(lexer, Token::RParen(pos!(1, 107)));
    next!(lexer, ws!());
    next!(lexer, str!(1, 109, "Los Angeles"));
    next!(lexer, ws!());
    next!(lexer, str!(1, 123, ""));
    next!(lexer, ws!());
    next!(lexer, str!(1, 126, "24404"));
    next!(lexer, ws!());
    next!(lexer, str!(1, 134, "USA"));
    next!(lexer, Token::RBracket(pos!(1, 139)));
    next!(lexer, Token::RParen(pos!(1, 140)));
    next!(lexer, ws!());
    next!(lexer, Token::LParen(pos!(1, 142)));
    next!(lexer, str!(1, 143, "foo@org.com"));
    next!(lexer, Token::RParen(pos!(1, 156)));
    next!(lexer, ws!());
    next!(lexer, Token::LParen(pos!(1, 158)));
    next!(lexer, Token::LParen(pos!(1, 159)));

    next!(
      lexer,
      Token::Symbol(pos!(1, 160), vec![b'n', b'o', b't', b'e', b's'])
    );

    next!(lexer, ws!());
    next!(lexer, Token::Dot(pos!(1, 166)));
    next!(lexer, ws!());
    next!(lexer, str!(1, 168, "This is a note"));
    next!(lexer, Token::RParen(pos!(1, 184)));
    next!(lexer, Token::RParen(pos!(1, 185)));
    next!(lexer, ws!());
    next!(lexer, str!(1, 187, "a1010c22-e918-4c6e-a834-6858c488b320"));
    next!(lexer, ws!());
    next!(lexer, str!(1, 226, "2020-09-23 19:57:53 +0000"));
    next!(lexer, ws!());
    next!(lexer, str!(1, 254, "2020-09-23 19:59:01 +0000"));
    next!(lexer, ws!());
    next!(lexer, Token::Nil(pos!(1, 282)));
    next!(lexer, Token::RBracket(pos!(1, 285)));
    next!(lexer, nl!());

    assert_eq!(lexer.next(), None);
    assert_eq!(lexer.pos, pos!(2, 1));
  }
}
