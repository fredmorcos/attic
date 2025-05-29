use derive_more::Display;
use derive_new::new;

pub type Byte = u8;
pub type Buffer<'b> = &'b [Byte];
pub type Index = usize;

/// Locations in source code.
#[derive(new, Display, Debug, PartialEq, Eq, Clone, Copy)]
#[display("{line}:{col}")]
pub struct Location {
  /// Byte index in buffer.
  index: Index,

  /// Line in buffer.
  line: usize,

  /// Column in line.
  col: usize,
}

/// Create a default location at line `1`, column `1`.
impl Default for Location {
  fn default() -> Self {
    Self { line: 1, col: 1, index: 0 }
  }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Parser<'b> {
  /// The current point in the input buffer.
  buffer: Buffer<'b>,

  /// The original input buffer.
  original: Buffer<'b>,

  /// The current location in the input buffer.
  location: Location,
}

impl<'b> From<Buffer<'b>> for Parser<'b> {
  fn from(buffer: Buffer<'b>) -> Self {
    Self { buffer, original: buffer, location: Default::default() }
  }
}

/// The kind of a [Token].
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenKind<'b> {
  Whitespace(Buffer<'b>),
  SquareLeft,
  SquareRight,
  BraceLeft,
  BraceRight,
  ParenLeft,
  ParenRight,
  Quote,
  Tick,
  Tilda,
  Caret,
  At,
}

/// Units returned by iterating over a [Parser].
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token<'b> {
  /// Index of the token in the original input buffer.
  location: Location,

  /// The type of token.
  kind: TokenKind<'b>,
}

impl<'b> Iterator for Parser<'b> {
  type Item = Token<'b>;

  fn next(&mut self) -> Option<Self::Item> {
    fn read_while<P>(buffer: Buffer, predicate: P) -> (Buffer, Buffer)
    where
      P: Fn(Byte) -> bool,
    {
      let split = buffer
        .iter()
        .enumerate()
        .find_map(|(i, &c)| if predicate(c) { None } else { Some(i) })
        .unwrap_or(buffer.len());

      (&buffer[..split], &buffer[split..])
    }

    pub fn read_whitespace(buffer: Buffer) -> (Buffer, Buffer) {
      read_while(buffer, |b| b.is_ascii_whitespace())
    }

    'MAIN: loop {
      let &b = self.buffer.first()?;

      if b.is_ascii_whitespace() {
        let (ws, rem) = read_whitespace(self.buffer);
        self.index += len;
        self.buf = rem;
        continue 'MAIN;
      } else if b == b'/' {
        match self.buf.first() {
          Some(b'/') => {}
          Some(_) => return Some(Err(Err::expected_comment(self))),
          None => return Some(Err(Err::expected_comment(self))),
        }

        let (com, rem) = parser::read_until_nl(self.buf);
        self.index += com.len();
        self.buf = rem;
        continue 'MAIN;
      } else if b == b'(' {
        let (txt, rem) = parser::read_while(&self.buf[1..], |b| b != b')');
        let label = match Label::try_from(txt) {
          Ok(label) => label,
          Err(e) => return Some(Err(Err::invalid_label(self, e))),
        };

        self.buf = match parser::read_one(rem, |b| b == b')') {
          Some((_, rem)) => rem,
          None => return Some(Err(Err::missing_lparen(self, txt.len()))),
        };

        let tok = Token::label(self.index, label);
        self.index += txt.len() + 2;
        return Some(Ok(tok));
      } else if b == b'@' {
        match Addr::read_from(&self.buf[1..]) {
          Ok((addr, rem, len)) => {
            let tok = Token::addr(self.index, addr);
            self.buf = rem;
            self.index += len + 1;
            return Some(Ok(tok));
          }
          Err(e) => return Some(Err(Err::invalid_addr(self, e))),
        }
      } else {
        match Inst::read_from(self.buf) {
          Ok((inst, rem, len)) => {
            let tok = Token::inst(self.index, inst);
            self.buf = rem;
            self.index += len;
            return Some(Ok(tok));
          }
          Err(e) => return Some(Err(Err::invalid_inst(self, e))),
        }
      }
    }
  }
}

pub fn read(input: &str) -> &str {
  println!("Input: {}", input);
  input
}
