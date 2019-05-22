#![warn(clippy::all)]

extern crate structopt;

use structopt::StructOpt;

use std::path::PathBuf;
use std::fs::File;
use std::io::{self, Read, BufReader, Bytes};
use std::fmt::{self, Display, Formatter};
use std::str::{FromStr, Utf8Error};
use std::num;
use std::iter::Peekable;

#[derive(StructOpt)]
struct Opt {
  #[structopt(name = "input_file",
              parse(from_os_str),
              help = "Input file")]
  file: PathBuf,
}

#[derive(Debug)]
enum Error {
  IO(io::Error),
  Parse(ParseError),
}

#[derive(Debug)]
enum ParseError {
  StrToInt(std::num::ParseIntError),
  IntToChar,
  UnrecognizedEscapeSeq(char),
  UnsupportedEscapeSeq(char),
  MissingHexStrClosing,
  PrematureEOF,
  UTF8(Utf8Error),
  StrToFloat(num::ParseFloatError),
  InvalidNum,
}

fn main() -> Result<(), Error> {
  let filename: PathBuf = Opt::from_args().file;
  let file: File = File::open(filename).map_err(Error::IO)?;
  let reader: BufReader<File> = BufReader::new(file);
  let mut iter = reader.bytes();

  // let doc = lex_document(&data).map_err(Error::Parse)?;
  // print!("{}", doc);

  Ok(())
}

type Iter = Peekable<Bytes<BufReader<File>>>;
type IterResult = Result<u8, io::Error>;

// 1. Parsing keywords (true, false, null, obj, endobj, etc...)
// should be refactored.

// 2. The lexer should be implemented as an "iterator generator" (in
// Rust, an interator of iterators).

// 3. Int/Real lexing should be refactored.

// 4. Ideally, implement one type per token-type and use a trait
// Token: this improves type-safety of parsing but is a lot of work
// for a test.

// 5. Do proper error handling.

// 6. Handle EOF everywhere properly.

// 7. Using macros could potentially be beneficial for repetitive
// parts.

// 8. There is a mistake in the sample output where stream data
// contains the EOL markers from stream and endstream. The PDF spec
// says in section 7.3.8.1: `All streams shall be indirect objects
// (see 7.3.10, "Indirect Objects") and the stream dictionary shall be
// a direct object.  The keyword stream that follows the stream
// dictionary shall be followed by an end-of-line marker consisting of
// either a CARRIAGE RETURN and a LINE FEED or just a LINE FEED, and
// not by a CARRIAGE RETURN alone. The sequence of bytes that make up
// a stream lie between the end-of-line marker following the stream
// keyword and the endstream keyword; the stream dictionary specifies
// the exact number of bytes. There should be an end-of-line marker
// after the data and before endstream; this marker shall not be
// included in the stream length. There shall not be any extra bytes,
// other than white space, between endstream and endobj.`

// 9. There is a mistake in the sample output where negative integers
// are interpreted as reals. This is against the PDF spec.

// 10. There is a mistake in the sample output where Name and Literal
// String objects' escapes are not interpreted.

// whitespace characters
const NUL : u8 = 0x00;         // NULL
const HT  : u8 = 0x09;         // HORIZ TAB
const LF  : u8 = 0x0A;         // LINE FEED
const FF  : u8 = 0x0C;         // FORM FEED
const CR  : u8 = 0x0D;         // CARR RETURN
const SP  : u8 = 0x20;         // SPACE

const WS: [u8; 6] = [NUL, HT, LF, FF, CR, SP];

const CRLF: [u8; 2] = [CR, LF];

// delimiter characters
const LPAR : u8 = 0x28;        // (
const RPAR : u8 = 0x29;        // )
const LT   : u8 = 0x3C;        // <
const GT   : u8 = 0x3E;        // >
const LBR  : u8 = 0x5B;        // [
const RBR  : u8 = 0x5D;        // ]
const LCBR : u8 = 0x7B;        // {
const RCBR : u8 = 0x7D;        // }
const SOL  : u8 = 0x2F;        // /
const PERC : u8 = 0x25;        // %

const DELIM: [u8; 10] = [LPAR, RPAR, LT, GT, LBR, RBR, LCBR, RCBR, SOL, PERC];

// An alternative way to do delimiters, but the above is much clearer
// and better when dealing with exceptional cases and is also
// consistent with the whitespace above
// static DELIM_LITS: &[u8] = b"()<>[]{}/%";

// reverse sol (backslash)
const RSOL: u8 = b'\\';

// hexadecimals
const HEXES: &[u8] = b"0123456789ABCDEFabcdef";

// bools
const TRUE  : &[u8] = b"true";
const FALSE : &[u8] = b"false";

// numerics
const PLUS  : u8 = b'+';
const MINUS : u8 = b'-';
const DOT   : u8 = b'.';

const DIGITS: &[u8] = b"0123456789";
const OCT_DIGITS: &[u8] = b"01234567";

// nul object
// This is in fact incorrect according to the spec: the spec states it
// should be "null" and not "n".
const NULL: u8 = b'n';

// objects
const OBJ    : &[u8] = b"obj";
const ENDOBJ : &[u8] = b"endobj";
const REF    : u8 = b'R';

// names
const NUMSIGN: u8 = b'#';

// dictionaries
const DICT    : &[u8] = b"<<";
const ENDDICT : &[u8] = b">>";

// cross-refs
const XREF      : &[u8] = b"xref";
const STARTXREF : &[u8] = b"startxref";

// trailer
const TRAILER: &[u8] = b"trailer";

// streams
const STREAM    : &[u8] = b"stream";
const ENDSTREAM : &[u8] = b"endstream";

const LF_ENDSTREAM   : &[u8] = b"\nendstream";
const CRLF_ENDSTREAM : &[u8] = b"\r\nendstream";

// other
const F: u8 = b'f';

type ParseRes<'doc> = Result<Token<'doc>, ParseError>;

#[derive(Debug)]
enum Token<'doc> {
  Comment(&'doc [u8]),
  Real(f64),
  Int(i128),
  Name(String),
  HexStr(String),
  LitStr(String),
  True,
  False,
  Null,
  Obj,
  EndObj,
  Ref,
  Dict,
  EndDict,
  Arr,
  EndArr,
  XRef,
  StartXRef,
  Trailer,
  Stream,
  EndStream,
  StreamData(&'doc [u8]),
  F,
}

impl<'doc> Display for Token<'doc> {
  fn fmt(&self, f: &mut Formatter) -> fmt::Result {
    match self {
      Token::Comment(comment) => {
        write!(f, "Comment(\"")?;
        for i in comment.iter() {
          if i.is_ascii() {
            write!(f, "{}", char::from(*i))?;
          } else {
            write!(f, "\\\\x{:x}", i)?;
          }
        }
        write!(f, "\")")
      }

      Token::True      => write!(f, "Boolean(true)"),
      Token::False     => write!(f, "Boolean(false)"),
      Token::Null      => write!(f, "N"),
      Token::Obj       => write!(f, "Obj"),
      Token::EndObj    => write!(f, "EndObj"),
      Token::Ref       => write!(f, "R"),
      Token::Dict      => write!(f, "DictStart"),
      Token::EndDict   => write!(f, "DictEnd"),
      Token::Arr       => write!(f, "ArrayStart"),
      Token::EndArr    => write!(f, "ArrayEnd"),
      Token::XRef      => write!(f, "Xref"),
      Token::StartXRef => write!(f, "StartXref"),
      Token::Trailer   => write!(f, "Trailer"),
      Token::Stream    => write!(f, "Stream"),
      Token::EndStream => write!(f, "EndStream"),
      Token::F         => write!(f, "F"),

      Token::StreamData(data) => {
        write!(f, "StreamData([")?;
        for (idx, i) in data.iter().enumerate() {
          if idx == 0 {
            write!(f, "{}", i)?;
          } else {
            write!(f, ", {}", i)?;
          }
        }
        write!(f, "])")
      }

      Token::Real(num)  => write!(f, "Real({})", num),
      Token::Int(num)   => write!(f, "Integer({})", num),
      Token::Name(name) => write!(f, "Name(\"{}\")", name),
      Token::HexStr(s)  => write!(f, "HexString(\"{}\")", s),
      Token::LitStr(s)  => write!(f, "String(\"{}\")", s),
    }
  }
}

#[derive(Debug)]
struct Document<'doc>(Vec<Token<'doc>>);

impl<'doc> Display for Document<'doc> {
  fn fmt(&self, f: &mut Formatter) -> fmt::Result {
    for tok in &self.0 {
      writeln!(f, "{}", tok)?;
    }
    Ok(())
  }
}

fn skip_whitespace(data: &[u8], pos: &mut usize) {
  assert!(WS.contains(&data[*pos]));

  while *pos < data.len() && WS.contains(&data[*pos]) {
    *pos += 1;
  }
}

fn lex_comment<'doc>(data: &'doc [u8], pos: &mut usize) -> Token<'doc> {
  assert_eq!(data[*pos], PERC);

  *pos +=1;                     // skip the %

  let start: usize = *pos;

  while *pos < data.len() && data[*pos] != LF && data[*pos] != CR {
    *pos += 1;
  }

  Token::Comment(&data[start .. *pos])
}

fn lex_litstr<'doc>(data: &'doc [u8], pos: &mut usize) -> ParseRes<'doc> {
  assert_eq!(data[*pos], LPAR);

  *pos +=1;                     // skip the (

  let mut s: String = String::new();
  let mut parens: usize = 1;

  while parens != 0 {
    if data[*pos] == LPAR {
      parens += 1;
      s.push(char::from(LPAR));
    } else if data[*pos] == RPAR {
      parens -= 1;
      if parens != 0 {
        s.push(char::from(RPAR));
      }
    } else if
      data[*pos] == CR ||
      data[*pos] == LF ||
      data[*pos .. *pos + CRLF.len()] == CRLF
    {
      s.push(char::from(LF));
    } else if data[*pos] == RSOL {
      let mut esc: String = String::new();

      // esc.push(RSOL);
      *pos += 1;

      if OCT_DIGITS.contains(&data[*pos]) {
        esc.push(char::from(data[*pos]));
        *pos += 1;

        if OCT_DIGITS.contains(&data[*pos]) {
          esc.push(char::from(data[*pos]));
          *pos += 1;

          if OCT_DIGITS.contains(&data[*pos]) {
            esc.push(char::from(data[*pos]));
            *pos += 1;
          }
        }

        let n = u32::from_str_radix(&esc, 8).map_err(ParseError::StrToInt)?;
        s.push(std::char::from_u32(n).ok_or(ParseError::IntToChar)?);
      } else if data[*pos] == b'n' {
        s.push('\n');
      } else if data[*pos] == b'r' {
        s.push('\r');
      } else if data[*pos] == b't' {
        s.push('\t');
      } else if data[*pos] == b'b' || data[*pos] == b'f' {
        return Err(ParseError::UnsupportedEscapeSeq(char::from(data[*pos])));
      } else if data[*pos] == b'\\' {
        s.push('\\');
      } else if data[*pos] == b'(' {
        s.push('(');
      } else if data[*pos] == b')' {
        s.push(')');
      } else if
        data[*pos] == CR ||
        data[*pos] == LF ||
        data[*pos .. *pos + CRLF.len()] == CRLF
      {
      } else {
        return Err(ParseError::UnrecognizedEscapeSeq(char::from(data[*pos])));
      }
    } else {
      s.push(char::from(data[*pos]));
    }

    *pos += 1;
  }

  Ok(Token::LitStr(s))
}

fn lex_stream_lf<'doc>(data: &'doc [u8], pos: &mut usize) -> Token<'doc> {
  assert_eq!(data[*pos], LF);

  *pos += 1;                    // skip the LF

  let start: usize = *pos;

  while
    *pos < data.len() &&
    data[*pos ..].len() > LF_ENDSTREAM.len() &&
    &data[*pos .. *pos + LF_ENDSTREAM.len()] != LF_ENDSTREAM
  {
    *pos += 1;
  }

  Token::StreamData(&data[start .. *pos])
}

fn lex_stream_crlf<'doc>(data: &'doc [u8], pos: &mut usize) -> Token<'doc> {
  assert_eq!(data[*pos .. *pos + CRLF.len()], CRLF);

  *pos += 2;                    // skip the CR and LF

  let start: usize = *pos;

  while
    *pos < data.len() &&
    data[*pos ..].len() > CRLF_ENDSTREAM.len() &&
    &data[*pos .. *pos + CRLF_ENDSTREAM.len()] != CRLF_ENDSTREAM
  {
    *pos += 1;
  }

  Token::StreamData(&data[start .. *pos])
}

fn lex_hexstr<'doc>(data: &'doc [u8], pos: &mut usize) -> ParseRes<'doc> {
  assert_eq!(data[*pos], LT);

  *pos +=1;                     // skip the <

  let mut val: String = String::new();

  while *pos < data.len() && HEXES.contains(&data[*pos]) {
    val.push(char::from(data[*pos]));
    *pos += 1;
  }

  if *pos >= data.len() {
    return Err(ParseError::PrematureEOF);
  }

  if data[*pos] != GT {
    return Err(ParseError::MissingHexStrClosing);
  }

  *pos += 1;                    // skip the >

  if val.len() % 2 != 0 {
    val.push('0');
  }

  Ok(Token::HexStr(val))
}

fn lex_name<'doc>(data: &'doc [u8], pos: &mut usize) -> ParseRes<'doc> {
  assert_eq!(data[*pos], SOL);

  *pos +=1;                     // skip the /

  let mut name: String = String::new();

  while *pos < data.len() && !WS.contains(&data[*pos]) && !DELIM.contains(&data[*pos]) {
    // if we find a #
    if data[*pos] == NUMSIGN {
      // we skip it
      *pos += 1;

      // and we expect the input to have at least 2 more bytes
      if *pos > data.len() - 2 {
        return Err(ParseError::PrematureEOF);
      }

      let mut num: String = String::new();

      // the first byte
      num.push(char::from(data[*pos]));
      *pos += 1;

      // the second byte
      num.push(char::from(data[*pos]));
      *pos += 1;

      let n = u32::from_str_radix(&num, 16).map_err(ParseError::StrToInt)?;
      name.push(std::char::from_u32(n).ok_or(ParseError::IntToChar)?);
    } else {
      name.push(char::from(data[*pos]));
      *pos += 1;
    }
  }

  Ok(Token::Name(name))
}

fn starts_numeric(data: &[u8], pos: usize) -> bool {
  data[pos] == DOT || data[pos] == PLUS || data[pos] == MINUS ||
    DIGITS.contains(&data[pos])
}

fn lex_numeric<'doc>(data: &'doc [u8], pos: &mut usize) -> ParseRes<'doc> {
  assert!(starts_numeric(data, *pos));

  let start: usize = *pos;

  if data[*pos] == DOT {
    *pos += 1;

    if !DIGITS.contains(&data[*pos]) {
      return Err(ParseError::InvalidNum);
    }

    // parse an int
    while *pos < data.len() && DIGITS.contains(&data[*pos]) {
      *pos += 1;
    }

    let num: &str = std::str::from_utf8(&data[start .. *pos]).map_err(ParseError::UTF8)?;
    Ok(Token::Real(f64::from_str(num).map_err(ParseError::StrToFloat)?))
  } else if data[*pos] == PLUS || data[*pos] == MINUS {
    *pos += 1;

    if !DIGITS.contains(&data[*pos]) {
      return Err(ParseError::InvalidNum);
    }

    // parse an int
    while *pos < data.len() && DIGITS.contains(&data[*pos]) {
      *pos += 1;
    }

    // if we find a dot after the int
    if data[*pos] == DOT {
      *pos += 1;

      while DIGITS.contains(&data[*pos]) {
        *pos += 1;
      }

      let num: &str = std::str::from_utf8(&data[start .. *pos]).map_err(ParseError::UTF8)?;
      Ok(Token::Real(f64::from_str(num).map_err(ParseError::StrToFloat)?))
    } else {
      let num: &str = std::str::from_utf8(&data[start .. *pos]).map_err(ParseError::UTF8)?;
      Ok(Token::Int(i128::from_str(num).map_err(ParseError::StrToInt)?))
    }
  } else if DIGITS.contains(&data[*pos]) {
    *pos += 1;

    while *pos < data.len() && DIGITS.contains(&data[*pos]) {
      *pos += 1;
    }

    // if we find a dot after the int
    if data[*pos] == DOT {
      *pos += 1;

      while *pos < data.len() && DIGITS.contains(&data[*pos]) {
        *pos += 1;
      }

      let num: &str = std::str::from_utf8(&data[start .. *pos]).map_err(ParseError::UTF8)?;
      Ok(Token::Real(f64::from_str(num).map_err(ParseError::StrToFloat)?))
    } else {
      let num: &str = std::str::from_utf8(&data[start .. *pos]).map_err(ParseError::UTF8)?;
      Ok(Token::Int(i128::from_str(num).map_err(ParseError::StrToInt)?))
    }
  } else {
    Err(ParseError::InvalidNum)
  }
}

fn lex_document(data: &[u8]) -> Result<Document, ParseError> {
  let mut tokens: Vec<Token> = Vec::new();
  let mut pos: usize = 0;

  while pos < data.len() {
    if WS.contains(&data[pos]) {
      skip_whitespace(&data, &mut pos);
    } else if data[pos] == PERC {
      tokens.push(lex_comment(data, &mut pos));
    } else if starts_numeric(data, pos) {
      tokens.push(lex_numeric(data, &mut pos)?);
    } else if data[pos] == SOL {
      tokens.push(lex_name(data, &mut pos)?);
    } else if data[pos] == LPAR {
      tokens.push(lex_litstr(data, &mut pos)?);
    } else if data[pos ..].len() >= TRUE.len() &&
      &data[pos .. pos + TRUE.len()] == TRUE
    {
      tokens.push(Token::True);
      pos += TRUE.len();
    }
    else if data[pos ..].len() >= FALSE.len() &&
      &data[pos .. pos + FALSE.len()] == FALSE
    {
      tokens.push(Token::False);
      pos += FALSE.len();
    }
    else if data[pos ..].len() >= OBJ.len() &&
      &data[pos .. pos + OBJ.len()] == OBJ
    {
      tokens.push(Token::Obj);
      pos += OBJ.len();
    }
    else if data[pos ..].len() >= ENDOBJ.len() &&
      &data[pos .. pos + ENDOBJ.len()] == ENDOBJ
    {
      tokens.push(Token::EndObj);
      pos += ENDOBJ.len();
    }
    else if data[pos ..].len() >= DICT.len() &&
      &data[pos .. pos + DICT.len()] == DICT
    {
      tokens.push(Token::Dict);
      pos += DICT.len();
    }
    else if data[pos ..].len() >= ENDDICT.len() &&
      &data[pos .. pos + ENDDICT.len()] == ENDDICT
    {
      tokens.push(Token::EndDict);
      pos += ENDDICT.len();
    }
    else if data[pos ..].len() >= XREF.len() &&
      &data[pos .. pos + XREF.len()] == XREF
    {
      tokens.push(Token::XRef);
      pos += XREF.len();
    }
    else if data[pos ..].len() >= STARTXREF.len() &&
      &data[pos .. pos + STARTXREF.len()] == STARTXREF
    {
      tokens.push(Token::StartXRef);
      pos += STARTXREF.len();
    }
    else if data[pos ..].len() >= TRAILER.len() &&
      &data[pos .. pos + TRAILER.len()] == TRAILER
    {
      tokens.push(Token::Trailer);
      pos += TRAILER.len();
    }
    else if data[pos ..].len() >= STREAM.len() &&
      &data[pos .. pos + STREAM.len()] == STREAM
    {
      tokens.push(Token::Stream);
      pos += STREAM.len();

      assert!(data[pos .. pos + CRLF.len()] == CRLF || data[pos] == LF);

      if
        data[pos ..].len() >= CRLF.len() &&
        data[pos .. pos + CRLF.len()] == CRLF
      {
        tokens.push(lex_stream_crlf(data, &mut pos));
      } else if !data[pos ..].is_empty() && data[pos] == LF {
        tokens.push(lex_stream_lf(data, &mut pos));
      }
    }
    else if data[pos ..].len() >= ENDSTREAM.len() &&
      &data[pos .. pos + ENDSTREAM.len()] == ENDSTREAM
    {
      tokens.push(Token::EndStream);
      pos += ENDSTREAM.len();
    }
    else if !data[pos ..].is_empty() && data[pos] == NULL {
      tokens.push(Token::Null);
      pos += 1;
    } else if !data[pos ..].is_empty() && data[pos] == REF {
      tokens.push(Token::Ref);
      pos += 1;
    } else if !data[pos ..].is_empty() && data[pos] == LBR {
      tokens.push(Token::Arr);
      pos += 1;
    } else if !data[pos ..].is_empty() && data[pos] == RBR {
      tokens.push(Token::EndArr);
      pos += 1;
    } else if !data[pos ..].is_empty() && data[pos] == F {
      tokens.push(Token::F);
      pos += 1;
    } else if data[pos] == LT {
      tokens.push(lex_hexstr(data, &mut pos)?);
    } else {
      eprintln!("Unknown byte {} (0x{:x})", data[pos], data[pos]);
      break;
    }
  }

  Ok(Document(tokens))
}
