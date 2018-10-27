extern crate getopts;

use std::os::{args};
use getopts::{optopt, optflag, getopts, usage};

fn pet_version () -> f32 { 0.3 }

fn print_version (progname: &str) {
  println!("PET ({}) -- Personal Expense Tracker", progname);
  println!("Version {} Copyright 2012-2014 (c)", pet_version());
  println!("Fred Morcos <fred.morcos@gmail.com>");
  println!("https://github.com/fredmorcos/pet.git");
}

fn main () {
  let args = args();
  let progname: ~str = args[0].clone();

  let opts = ~[optopt ("o", "output",  "Output filename", "FILE"),
               optflag("v", "verbose", "Verbose console output"),
               optflag("q", "quiet",   "Disable console output"),
               optflag("d", "debug",   "Debug console output"),
               optflag("h", "help",    "Show help"),
               optflag("V", "version", "Show version info")];

  let opt_match = match getopts(args.tail(), opts) {
    Ok(m)  => { m }
    Err(f) => { fail!(f.to_err_msg()) }
  };

  if opt_match.opt_present("V") || opt_match.opt_present("h") {
    print_version(progname);

    if !opt_match.opt_present("h") {
      return;
    }

    println!("");
  }

  if opt_match.opt_present("h") {
    println!("{}", usage(progname, opts));
    return;
  }
}
