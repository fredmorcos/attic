extern crate structopt;
extern crate fastwalkdir;

use std::path::PathBuf;
use structopt::StructOpt;
use fastwalkdir::Dir;

#[derive(StructOpt)]
struct Args {
  #[structopt(parse(from_os_str))]
  /// Path to recursively scan
  dir: PathBuf,
}

fn main() -> Result<(), fastwalkdir::Error> {
  let args = Args::from_args();
  let _info = Dir::new(args.dir, vec![])?;
  Ok(())
}
