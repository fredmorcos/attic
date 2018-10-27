#[macro_use]
extern crate log;
extern crate env_logger;
extern crate hostname;

use hostname::get_hostname;
use std::process;

fn main() {
  env_logger::Builder::from_default_env()
    .filter(None, log::LevelFilter::Info)
    .init();

  let hostname = match get_hostname() {
    None => {
      error!("Could not get local hostname");
      process::exit(1);
    },
    Some(hostname) => {
      info!("Local hostname: {}", hostname);
      hostname
    },
  };
}
