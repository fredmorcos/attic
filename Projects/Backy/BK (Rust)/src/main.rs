use std::env;
use std::process;
use std::io::{self, Write};

fn usage(exit_code: u8) {
  writeln!(
    &mut io::stderr(),
    "usage: bkd [-h46dsk] -r dir [-a host] -p port"
  ).expect("error: cannot write to stderr");

  process::exit(i32::from(exit_code));
}

enum AFInet {
  IPv4,
  IPv6,
  Unspec,
}

struct Config {
  addr_fam: AFInet,
  host: Option<String>,
  port: String,
  dir: String,
  detach: bool,
  safe: bool,
  keep: bool,
}

impl Config {
  fn new(mut args: env::Args) -> Result<Self, String> {
    args.next(); // skip process name

    let mut addr_fam = AFInet::Unspec;
    let mut host = None;
    let mut port = None;
    let mut dir = None;
    let mut detach = false;
    let mut safe = false;
    let mut keep = false;

    loop {
      match args.next() {
        None => break,
        Some(arg) => {
          if arg == "-h" {
            usage(0);
          } else if arg == "-4" {
            addr_fam = AFInet::IPv4;
          } else if arg == "-6" {
            addr_fam = AFInet::IPv6;
          } else if arg == "-d" {
            detach = true;
          } else if arg == "-s" {
            safe = true;
          } else if arg == "-k" {
            keep = true;
          } else if arg == "-r" {
            dir = match args.next() {
              None => return Err(String::from("no argument given for -r, see -h")),
              Some(d) => Some(d),
            };
          } else if arg == "-a" {
            host = match args.next() {
              None => return Err(String::from("no argument given for -a, see -h")),
              Some(h) => Some(h),
            };
          } else if arg == "-p" {
            port = match args.next() {
              None => return Err(String::from("no argument given for -p, see -h")),
              Some(p) => Some(p),
            };
          } else {
            return Err(format!("unrecognized argument `{}`, see -h", arg));
          }
        }
      }
    }

    let port = match port {
      None => return Err(String::from("no port given with -p, see -h")),
      Some(p) => p,
    };

    let dir = match dir {
      None => return Err(String::from("no dir given with -r, see -h")),
      Some(d) => d,
    };

    Ok(Config {
      addr_fam: addr_fam,
      host: host,
      port: port,
      dir: dir,
      detach: detach,
      safe: safe,
      keep: keep,
    })
  }
}

fn main() {
  let mut stderr = io::stderr();

  let config: Config = match Config::new(env::args()) {
    Err(err) => {
      writeln!(&mut stderr, "error: {}", err).expect("error: cannot write to stderr");
      process::exit(1)
    }
    Ok(cfg) => cfg,
  };
}
