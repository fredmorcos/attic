use std::fmt;
use std::process;
use std::time;

#[cfg(debug_assertions)]
pub fn pdbg(args: fmt::Arguments) {
  eprintln!("[DBG ] {}", args);
}

#[cfg(not(debug_assertions))]
pub fn pdbg(_: fmt::Arguments) {}

pub fn pdie(args: fmt::Arguments) -> ! {
  perr(args);
  process::exit(1)
}

pub fn perr(args: fmt::Arguments) {
  eprintln!("[ERR ] {}", args);
}

pub fn pwarn(args: fmt::Arguments) {
  eprintln!("[WARN] {}", args);
}

pub fn pinfo(args: fmt::Arguments) {
  println!("[INFO] {}", args);
}

pub fn pts(t1: &time::Instant, t2: &time::Instant) {
  let d = t2.duration_since(*t1);
  let d_seconds = d.as_secs() as f64 + d.subsec_nanos() as f64 * 1e-9;
  pinfo(format_args!("Elapsed {} seconds", d_seconds));
}
