use errors::*;
use std::fs::File;
use std::io::Read;

#[macro_export] macro_rules! pdbg {
  ($($arg:tt)*) => {
    #[cfg(debug_assertions)]
    eprintln!("[DBG ] {}", format!($($arg)*));
  };
}

#[macro_export] macro_rules! pdie {
  ($($arg:tt)*) => {
    perr!($($arg)*);
    ::std::process::exit(1);
  };
}

#[macro_export] macro_rules! perr {
  ($($arg:tt)*) => {
    eprintln!("[ERR ] {}", format!($($arg)*));
  };
}

#[macro_export] macro_rules! pwarn {
  ($($arg:tt)*) => {
    eprintln!("[WARN] {}", format!($($arg)*));
  };
}

#[macro_export] macro_rules! pbktr {
  ($($arg:tt)*) => {
    eprintln!("[BKTR] {}", format!($($arg)*));
  };
}

#[macro_export] macro_rules! pinfo {
  ($($arg:tt)*) => {
    println!("[INFO] {}", format!($($arg)*));
  };
}

pub fn pts(t1: &::std::time::Instant, t2: &::std::time::Instant) {
  let d = t2.duration_since(*t1);
  let d_seconds = d.as_secs() as f64 + f64::from(d.subsec_nanos()) * 1e-9;
  pinfo!("Elapsed {} seconds", d_seconds);
}

pub fn load_file(f: &str) -> Result<String> {
  let mut fd = File::open(f).chain_err(|| format!("Cannot open file {}", f))?;
  let mut cts: String = String::new();
  fd.read_to_string(&mut cts).chain_err(|| format!("Cannot read file {}", f))?;
  Ok(cts)
}

pub fn load_files(files: &[String]) -> Result<Vec<String>> {
  let mut cts: Vec<String> = Vec::with_capacity(files.len());

  for f in files {
    cts.push(load_file(f).chain_err(|| "Cannot load files")?);
  }

  assert_eq!(cts.len(), files.len());
  Ok(cts)
}
