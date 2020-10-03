#![warn(clippy::all)]

mod cmdline;
mod err;
mod file;
mod gen;

use cmdline::{Cmd, Pw};
use err::Error;
use file::get_passfile;
use gen::generate;
use std::fmt::Debug;
use std::fs;
use std::path::Path;
use std::path::PathBuf;
use structopt::StructOpt;
use zeroize::Zeroize;

fn fmt_entry(fmt: &str, entry: EntryData) -> String {
    let mut iter = fmt.chars();
    let mut out = String::new();
    while let Some(c) = iter.next() {
        match c {
            '%' => match iter.next() {
                Some('N') => out.push_str(entry.name),
                Some('L') => out.push_str(entry.link),
                Some('U') => out.push_str(entry.username),
                Some('P') => out.push_str(entry.password),
                Some(c2) => {
                    out.push(c);
                    out.push(c2);
                }
                None => {
                    out.push(c);
                    break;
                }
            },
            _ => out.push(c),
        }
    }
    out
}

#[derive(Debug)]
struct EntryData<'a> {
    name: &'a str,
    link: &'a str,
    username: &'a str,
    password: &'a str,
}

impl<'a> EntryData<'a> {
    fn parse(num: usize, mut iter: impl Iterator<Item = &'a str>) -> Result<Self, Error> {
        Ok(EntryData {
            name: iter.next().ok_or_else(|| Error::MissingName(num))?,
            link: iter.next().ok_or_else(|| Error::MissingLink(num))?,
            username: iter.next().ok_or_else(|| Error::MissingUsername(num))?,
            password: iter.next().ok_or_else(|| Error::MissingPassword(num))?,
        })
    }
}

enum Entry<'a> {
    Valid(EntryData<'a>),
    Invalid(EntryData<'a>),
    Change(EntryData<'a>),
}

impl<'a> Entry<'a> {
    fn parse(num: usize, mut iter: impl Iterator<Item = &'a str>) -> Result<Self, Error> {
        let marker = iter.next().ok_or_else(|| Error::MissingMarker(num))?;
        let data = EntryData::parse(num, iter)?;
        match marker {
            "+" => Ok(Entry::Valid(data)),
            "-" => Ok(Entry::Invalid(data)),
            "*" => Ok(Entry::Change(data)),
            _ => Err(Error::InvalidEntryMarker(num, marker.to_string())),
        }
    }
}

fn parse(data: &str) -> impl Iterator<Item = Result<Entry, Error>> {
    data.lines()
        .enumerate()
        .filter(|(_, line)| {
            let line = line.trim();
            !line.is_empty() && !line.starts_with('#')
        })
        .map(|(num, line)| Entry::parse(num + 1, line.split_whitespace()))
}

fn read<P: AsRef<Path>>(file: P) -> Result<String, Error> {
    fs::read_to_string(file).map_err(Error::PassFile)
}

fn check(file: PathBuf) -> Result<(), Error> {
    let mut data = read(file)?;
    let entries = parse(&data);
    let mut valid = 0;
    let mut invalid = 0;
    let mut change = 0;
    for entry in entries {
        let entry = entry?;
        match entry {
            Entry::Valid(_) => valid += 1,
            Entry::Invalid(_) => invalid += 1,
            Entry::Change(_) => change += 1,
        }
    }
    data.zeroize();

    println!(
        "{} current, {} inactive, {} need changing",
        valid, invalid, change
    );

    Ok(())
}

fn get(file: PathBuf, acc: String, format: String) -> Result<(), Error> {
    let mut data = read(file)?;
    let entries = parse(&data);
    let mut matched = None;
    for entry in entries {
        if let Entry::Valid(data) = entry? {
            if data.name == acc {
                if matched.is_some() {
                    return Err(Error::Mismatch(acc));
                }
                matched = Some(data);
            }
        }
    }
    if let Some(entry) = matched {
        println!("{}", fmt_entry(&format, entry));
    } else {
        return Err(Error::NoMatches(acc));
    }
    data.zeroize();
    Ok(())
}

fn list(file: PathBuf, query: String) -> Result<(), Error> {
    let mut data = read(file)?;
    let entries = parse(&data);
    for entry in entries {
        if let Entry::Valid(data) = entry? {
            if data.name.to_lowercase().contains(&query.to_lowercase()) {
                println!("{}", fmt_entry(&String::from("%N (%L) %U %P"), data));
            }
        }
    }
    data.zeroize();
    Ok(())
}

fn main() -> Result<(), Error> {
    let opt = Pw::from_args();

    let log_level = match opt.verbose {
        0 => log::LevelFilter::Warn,
        1 => log::LevelFilter::Info,
        2 => log::LevelFilter::Debug,
        _ => log::LevelFilter::Trace,
    };
    env_logger::Builder::new()
        .filter_level(log_level)
        .try_init()?;

    match opt.command {
        Cmd::Check { file } => check(get_passfile(file)?),
        Cmd::Generate => generate(),
        Cmd::Get { file, acc, format } => get(get_passfile(file)?, acc, format),
        Cmd::List { file, query } => list(get_passfile(file)?, query),
    }
}
