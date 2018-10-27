extern crate chrono;
extern crate syntect;

use syntect::easy as Synt;
use chrono::prelude::*;

use std::io;
use std::fs;
use std::borrow::Cow;
use std::str::{self, FromStr};

#[derive(Debug)]
pub enum Error {
  NoTitle(io::Error),
  NoAuthor(io::Error),
  NoCSS(io::Error),
  PostsDir(io::Error),
  PostFileEntry(io::Error),
  PostFile(io::Error),
}

fn main() -> Result<(), Error> {
  let title = fs::read_to_string("title").map_err(Error::NoTitle)?;
  let author = fs::read_to_string("author").map_err(Error::NoAuthor)?;
  let css = fs::read_to_string("css").map_err(Error::NoCSS)?;

  println!("Title:  {}", title);
  println!("Author: {}", author);
  println!("CSS:    {}", css);

  let posts = fs::read_dir("posts").map_err(Error::PostsDir)?;

  for post in posts {
    let post = post.map_err(Error::PostFileEntry)?;
    let post = fs::read_to_string(post.path()).map_err(Error::PostFile)?;
  }

  Ok(())
}

struct Post {
  title: String,
  author: Option<String>,
  date: Option<String>,
  content: String,
}

enum PostParseError {
  NoTitle,
  NoTitleCmd(usize),
}

impl FromStr for Post {
  type Err = PostParseError;

  fn from_str(s: &str) -> Result<Self, Self::Err> {
    fn string(i: impl Iterator<Item = (usize, char)>) -> String {
      i.map(|(_, c)| c).collect::<String>()
    }

    let mut iter: str::CharIndices = s.char_indices();

    let skipws = || iter.skip_while(|(_, c)| char::is_whitespace(*c));
    let untilnl = || iter.take_while(|(_, c)| *c != '\n');

    skipws();

    let mut title: String = if let Some((pos, title_cmd)) = iter.next() {
      if title_cmd != 't' {
        return Err(PostParseError::NoTitleCmd(pos));
      } else {
        skipws();
        string(untilnl())
      }
    } else {
      return Err(PostParseError::NoTitle);
    };

    skipws();

    let mut author: Option<String> = if let Some((pos, title_cmd)) = iter.next() {
      if title_cmd != 'a' {
        None
      } else {
        skipws();
        Some(string(untilnl()))
      }
    } else {
      None
    };

    skipws();

    let mut date: Option<String> = if let Some((pos, title_cmd)) = iter.next() {
      if title_cmd != 'd' {
        None
      } else {
        skipws();
        Some(string(untilnl()))
      }
    } else {
      None
    };

    Ok(Post { title, author, date, content })
  }
}
