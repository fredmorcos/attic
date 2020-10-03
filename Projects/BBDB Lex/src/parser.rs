use crate::lexer::Err as LexerErr;
use crate::lexer::Lexer;
use crate::lexer::Pos;
use crate::lexer::Token;
use crate::record::Record;
use std::io::Bytes;
use std::io::Read;

struct Parser<R: Read> {
  lexer: Lexer<R>,
  la: Option<Token>,
  tokens: Vec<Token>,
}

enum Err {
  Lexer(LexerErr),
  Invalid(Token),
  Unexpected { token: Token, record_pos: Pos },
  EOF { pos: Pos, record_pos: Pos },
}

enum Res<T> {
  End,
  Err(Err),
  Nil,
  Something(T),
}

impl<R: Read> Parser<R> {
  fn new(bytes: Bytes<R>) -> Self {
    Self {
      lexer: Lexer::new(bytes),
      la: None,
      tokens: Vec::new(),
    }
  }

  fn next(&mut self) -> Res<Token> {
    if let Some(la) = self.la.take() {
      Res::Something(la)
    } else {
      match self.lexer.next() {
        Some(Ok(token)) => Res::Something(token),
        Some(Err(e)) => Res::Err(Err::Lexer(e)),
        None => Res::End,
      }
    }
  }

  fn parse_string(&mut self) -> Res<usize> {}

  fn parse_string_or_nil(
    &mut self, record_pos: Pos,
  ) -> Option<Result<Option<usize>, Err>> {
    loop {
      match self.lexer.next() {
        Some(Ok(token @ Token::String(_, _))) => {
          self.tokens.push(token);
          return Some(Ok(Some(self.tokens.len() - 1)));
        }
        Some(Ok(token @ Token::Nil(_))) => {
          self.tokens.push(token);
          return Some(Ok(None));
        }
        Some(Ok(token @ Token::Whitespace(_))) => {
          self.tokens.push(token);
          continue;
        }
        Some(Ok(token)) => return Some(Err(Err::Unexpected { token, record_pos })),
        Some(Err(e)) => return Some(Err(Err::Lexer(e))),
        None => {
          let pos = self.lexer.pos();
          return Some(Err(Err::EOF { pos, record_pos }));
        }
      }
    }
  }

  // fn parse_list_of_strings_or_nil(
  //   &mut self, record_pos: Pos, square: bool,
  // ) -> Option<Result<Option<Vec<Option<usize>>>, Err>> {
  //   loop {
  //     match self.lexer.next() {
  //       Some(Ok(token @ Token::LBracket(_))) => {
  //         self.tokens.push(token);
  //         break;
  //       }
  //       Some(Ok(token @ Token::Nil(_))) => {
  //         self.tokens.push(token);
  //         return Some(Ok(None));
  //       }
  //       Some(Ok(token @ Token::Whitespace(_))) => {
  //         self.tokens.push(token);
  //         continue;
  //       }
  //       Some(Ok(token)) => break token,
  //       Some(Err(e)) => return Some(Err(Err::Lexer(e))),
  //       None => {
  //         let pos = self.lexer.pos();
  //         return Some(Err(Err::EOF { pos, record_pos }));
  //       }
  //     }
  //   }

  //   let mut res = Vec::new();

  //   todo!()
  // }

  fn parse_record(
    &mut self, record_pos: Pos, start: usize,
  ) -> Option<Result<Record, Err>> {
    macro_rules! string_or_nil {
      ($s:ident) => {
        match $s.parse_string_or_nil(record_pos)? {
          Ok(res) => res,
          Err(e) => return Some(Err(e)),
        }
      };
    }

    let _firstname = string_or_nil!(self);
    let _lastname = string_or_nil!(self);

    None
  }
}

impl<R: Read> Iterator for Parser<R> {
  type Item = Result<Record, Err>;

  fn next(&mut self) -> Option<Self::Item> {
    for token in &mut self.lexer {
      let token = match token {
        Ok(t) => t,
        Err(e) => return Some(Err(Err::Lexer(e))),
      };

      match token {
        Token::Whitespace(_) | Token::Newline(_) | Token::Comment(_, _) => {
          self.tokens.push(token)
        }
        Token::LParen(pos) | Token::LBracket(pos) => {
          self.tokens.push(token);
          return self.parse_record(pos, self.tokens.len() - 1);
        }
        Token::RParen(_)
        | Token::RBracket(_)
        | Token::Nil(_)
        | Token::Dot(_)
        | Token::Symbol(_, _)
        | Token::String(_, _) => return Some(Err(Err::Invalid(token))),
      }
    }

    None
  }
}
