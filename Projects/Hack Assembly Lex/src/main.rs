#![warn(clippy::all)]

mod token;

use derive_more::{Display, From};
use derive_new::new;
use either::Either;
use log::{debug, info, trace};
use std::fmt;
use std::fs::File;
use std::io::{self, BufReader, Read};
use std::{iter::Iterator, path::PathBuf};
use structopt::StructOpt;

#[derive(Debug, Display, From)]
enum Dest {
  #[display(fmt = "A")]
  A(token::KindAddress, token::Pos),
  #[display(fmt = "M")]
  M(token::KindMemory, token::Pos),
  #[display(fmt = "D")]
  D(token::KindData, token::Pos),
}

#[derive(Default, Debug)]
struct DestSet {
  a: Option<(token::KindAddress, token::Pos)>,
  m: Option<(token::KindMemory, token::Pos)>,
  d: Option<(token::KindData, token::Pos)>,
}

impl fmt::Display for DestSet {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    if self.a.is_some() {
      write!(f, "A")?;
    }
    if self.m.is_some() {
      write!(f, "M")?;
    }
    if self.d.is_some() {
      write!(f, "D")?;
    }

    Ok(())
  }
}

impl DestSet {
  fn none(&self) -> bool {
    self.a.is_none() && self.m.is_none() && self.d.is_none()
  }

  fn single(&self) -> bool {
    (self.a.is_some() && self.m.is_none() && self.d.is_none())
      || (self.a.is_none() && self.m.is_some() && self.d.is_none())
      || (self.a.is_none() && self.m.is_none() && self.d.is_some())
  }

  fn put_a(&mut self, kind: token::KindAddress, pos: token::Pos) -> bool {
    if self.a.is_none() {
      self.a = Some((kind, pos));
      true
    } else {
      false
    }
  }

  fn put_m(&mut self, kind: token::KindMemory, pos: token::Pos) -> bool {
    if self.m.is_none() {
      self.m = Some((kind, pos));
      true
    } else {
      false
    }
  }

  fn put_d(&mut self, kind: token::KindData, pos: token::Pos) -> bool {
    if self.d.is_none() {
      self.d = Some((kind, pos));
      true
    } else {
      false
    }
  }
}

#[derive(Debug, Display)]
enum Comp {
  #[display(fmt = "A")]
  A(token::KindAddress, token::Pos),
  #[display(fmt = "M")]
  M(token::KindMemory, token::Pos),
  #[display(fmt = "D")]
  D(token::KindData, token::Pos),
  #[display(fmt = "0")]
  Zero(token::KindZero, token::Pos),
  #[display(fmt = "1")]
  One(token::KindOne, token::Pos),
  #[display(fmt = "{}-{}", _0, _1)]
  Minus(Box<Comp>, Box<Comp>),
  #[display(fmt = "{}+{}", _0, _1)]
  Plus(Box<Comp>, Box<Comp>),
  #[display(fmt = "{}&{}", _0, _1)]
  And(Box<Comp>, Box<Comp>),
  #[display(fmt = "{}|{}", _0, _1)]
  Or(Box<Comp>, Box<Comp>),
  #[display(fmt = "!{}", _0)]
  Not(Box<Comp>),
  #[display(fmt = "-{}", _0)]
  Negative(Box<Comp>),
}

#[derive(Debug, Display)]
#[display(fmt = "{}={}", dests, comp)]
struct Assign {
  dests: DestSet,
  comp: Comp,
}

#[derive(Debug, Display)]
#[display(fmt = "{};{}", comp, tok)]
struct Jump {
  comp: Either<Assign, Comp>,
  tok: token::Jump,
  pos: token::Pos,
}

#[derive(Debug, Display, From)]
enum InstructionKind {
  NamedAddr(token::NamedAddr),
  NumAddr(token::NumAddr),
  RegAddr(token::RegAddr),
  Label(token::Label),
  Assign(Assign),
  Jump(Jump),
}

#[derive(Debug, Display, From)]
#[display(fmt = "{} {}", kind, pos)]
struct Instruction {
  kind: InstructionKind,
  pos: token::Pos,
}

#[derive(Debug, Display, From)]
enum ParserError {
  #[display(fmt = "Tokenization error: {}", _0)]
  Tokenizer(token::TokenizerError),
  #[display(fmt = "Unexpected {} after a {}, expecting {}", _0, _1, _2)]
  Unexpected(token::Token, token::Token, &'static str),
  #[display(fmt = "Unexpected standalone {}, expecting {}", _0, _1)]
  UnexpectedStandalone(token::Token, &'static str),
  #[display(fmt = "Duplicate destination {}", _0)]
  DuplicateDest(token::Token),
  #[display(fmt = "Invalid computation {}", _0)]
  InvalidComputation(DestSet),
}

#[derive(new)]
struct Parser<R: Read> {
  tokenizer: token::Tokenizer<R>,
  #[new(default)]
  lookahead: Option<token::Token>,
}

impl<R: Read> Iterator for Parser<R> {
  type Item = Result<Instruction, ParserError>;

  fn next(&mut self) -> Option<Self::Item> {
    macro_rules! next {
      () => {
        match self.tokenizer.next() {
          Some(Ok(tok)) => {
            debug!("Token: {}", tok);
            tok
          }
          Some(Err(e)) => return Some(Err(e.into())),
          None => return None,
        }
      };
    }

    macro_rules! instruction {
      ($kind:expr, $pos:expr) => {
        return Some(Ok(Instruction {
          kind: $kind,
          pos: $pos,
        }));
      };
    }

    loop {
      let tok1 = if let Some(la) = self.lookahead.take() {
        debug!("Token from previous lookahead: {}", la);
        la
      } else {
        next!()
      };

      enum InstructionState {
        Destinations(DestSet),
        Assignment(Assign),
        Jump(Jump),
      }

      let mut state = InstructionState::Destinations(DestSet::default());

      match tok1 {
        token::Token::Comment { .. } => continue,
        token::Token::Label { label, pos } => {
          instruction!(InstructionKind::Label(label), pos)
        }
        token::Token::NamedAddr { addr, pos } => {
          instruction!(InstructionKind::NamedAddr(addr), pos)
        }
        token::Token::NumAddr { addr, pos } => {
          instruction!(InstructionKind::NumAddr(addr), pos)
        }
        token::Token::RegAddr { addr, pos } => {
          instruction!(InstructionKind::RegAddr(addr), pos)
        }
        token::Token::Jump { jump, pos } => match state {
          InstructionState::Destinations(mut dests) => {}
          InstructionState::Assignment(mut assign) => {}
          InstructionState::Jump(mut jump) => {}
        },
        token::Token::A { kind, pos } => match state {
          InstructionState::Destinations(mut dests) => {
            if !dests.put_a(kind, pos) {
              return Some(Err(ParserError::DuplicateDest(tok1)));
            }
          }
          InstructionState::Assignment(mut assign) => {}
          InstructionState::Jump(mut jump) => {}
        },
        token::Token::M { kind, pos } => match state {
          InstructionState::Destinations(mut dests) => {
            if !dests.put_m(kind, pos) {
              return Some(Err(ParserError::DuplicateDest(tok1)));
            }
          }
          InstructionState::Assignment(mut assign) => {}
          InstructionState::Jump(mut jump) => {}
        },
        token::Token::D { kind, pos } => match state {
          InstructionState::Destinations(mut dests) => {
            if !dests.put_d(kind, pos) {
              return Some(Err(ParserError::DuplicateDest(tok1)));
            }
          }
          InstructionState::Assignment(mut assign) => {}
          InstructionState::Jump(mut jump) => {}
        },
        token::Token::One { kind, pos } => match state {
          InstructionState::Destinations(mut dests) => {}
          InstructionState::Assignment(mut assign) => {}
          InstructionState::Jump(mut jump) => {}
        },
        token::Token::Zero { kind, pos } => match state {
          InstructionState::Destinations(mut dests) => {}
          InstructionState::Assignment(mut assign) => {}
          InstructionState::Jump(mut jump) => {}
        },
        token::Token::Assign(pos) => match state {
          InstructionState::Destinations(mut dests) => {}
          InstructionState::Assignment(mut assign) => {}
          InstructionState::Jump(mut jump) => {}
        },
        token::Token::Sep(pos) => match state {
          InstructionState::Destinations(mut dests) => {
            // The destinations were actually a computation
            if dests.none() || !dests.single() {
              return Some(Err(ParserError::InvalidComputation(dests)));
            }
          }
          InstructionState::Assignment(mut assign) => {}
          InstructionState::Jump(mut jump) => {}
        },
        token::Token::Minus(pos) => match state {
          InstructionState::Destinations(mut dests) => {}
          InstructionState::Assignment(mut assign) => {}
          InstructionState::Jump(mut jump) => {}
        },
        token::Token::Plus(pos) => match state {
          InstructionState::Destinations(mut dests) => {}
          InstructionState::Assignment(mut assign) => {}
          InstructionState::Jump(mut jump) => {}
        },
        token::Token::Not(pos) => match state {
          InstructionState::Destinations(mut dests) => {}
          InstructionState::Assignment(mut assign) => {}
          InstructionState::Jump(mut jump) => {}
        },
        token::Token::And(pos) => match state {
          InstructionState::Destinations(mut dests) => {}
          InstructionState::Assignment(mut assign) => {}
          InstructionState::Jump(mut jump) => {}
        },
        token::Token::Or(pos) => match state {
          InstructionState::Destinations(mut dests) => {}
          InstructionState::Assignment(mut assign) => {}
          InstructionState::Jump(mut jump) => {}
        },
      }

      todo!()
    }
  }
}

#[derive(Debug, StructOpt)]
struct HasOptions {
  /// Verbose output (can be specified multiple times)
  #[structopt(short, long, parse(from_occurrences))]
  verbose: u8,

  /// Hack assembly file(s) to load
  #[structopt(name = "FILE", parse(from_os_str), required = true, min_values = 1)]
  files: Vec<PathBuf>,
}

#[derive(Display, From)]
enum HasError {
  #[display(fmt = "IO: {}", _0)]
  IO(io::Error),
  #[display(fmt = "Logger: {}", _0)]
  Logger(log::SetLoggerError),
  #[display(fmt = "Parser: {}", _0)]
  Parser(ParserError),
}

impl fmt::Debug for HasError {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}", self)
  }
}

fn main() -> Result<(), HasError> {
  let opt = HasOptions::from_args();

  let log_level = match opt.verbose {
    0 => log::LevelFilter::Warn,
    1 => log::LevelFilter::Info,
    2 => log::LevelFilter::Debug,
    _ => log::LevelFilter::Trace,
  };

  env_logger::Builder::new()
    .filter_level(log_level)
    .try_init()?;

  info!("Informational output enabled");
  debug!("Debug output enabled");
  trace!("Tracing output enabled");

  for file in opt.files {
    let f = File::open(&file)?;
    let reader = BufReader::new(f);
    info!("Reading file from {}", file.display());
    let mut tokenizer = token::Tokenizer::new(reader.bytes());
    // for token in &mut tokenizer {
    //   debug!("Token: {}", token.map_err(ParserError::Tokenizer)?);
    // }
    let parser = Parser::new(tokenizer);
    for instruction in parser {
      debug!("Instruction: {}", instruction?);
    }
  }

  Ok(())
}
