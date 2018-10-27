#[macro_use]
extern crate structopt;

#[macro_use]
extern crate log;
extern crate env_logger;

use structopt::StructOpt;
use std::net::TcpListener;
use std::process;
use std::io;
use std::fs::{self, DirEntry};
use std::path::Path;

#[derive(StructOpt)]
enum Config {
  #[structopt(name = "client")]
  #[structopt(about = "Run the client (data source)")]
  Client {
    #[structopt(short = "a")]
    #[structopt(help = "Server to connect to")]
    address: String,

    #[structopt(short = "d")]
    #[structopt(help = "Directory to synchronize from")]
    dir: String,
  },

  #[structopt(name = "server")]
  #[structopt(about = "Run the server (data destination)")]
  Server {
    #[structopt(short = "a")]
    #[structopt(help = "Address (interface) to listen on")]
    address: String,

    #[structopt(short = "d")]
    #[structopt(help = "Directory to synchronize onto")]
    dir: String,
  },
}

fn dirbuf(dir: &Path, res: &mut Vec<u8>) -> io::Result<()> {
  if dir.is_dir() {
    match fs::read_dir(dir) {
      Err(err) => warn!("Could not read directory {}: {}", dir.display(), err),
      Ok(entries) => for entry in entries {
        match entry {
          Err(err) => warn!("Could not get information: {}", err),
          Ok(entry) => info!("Entry {:?}", entry.path()),
        }
      },
    }

    for entry in fs::read_dir(dir)? {
      let entry = entry?;
      let path = entry.path();
      // if path.is_dir() {
      //   dirbuf(&path, cb)?;
      // } else {
      //   cb(&entry);
      // }
    }
  }

  Ok(())
}

fn server(addr: String, dir: String) -> ! {
  info!("Loading directory {}...", dir);
  let mut dir_info = vec![];
  let dir_info = dirbuf(Path::new(&dir), &mut dir_info);
  info!("Finished loading {}...", dir);

  let sock = TcpListener::bind(addr).unwrap_or_else(
    |err| {
      error!("Cannot start server");
      error!("├─ Cause (bind): {}", err);
      error!("└─ Make sure the address is formatted as ADDRESS:PORT");
      process::exit(1);
    });

  match sock.local_addr() {
    Ok(addr) => info!("Listening on {}...", addr),
    Err(err) => warn!("Could not get socket address: {}", err),
  }

  for stream in sock.incoming() {
    match stream {
      Ok(stream) => match stream.peer_addr() {
        Ok(addr) => info!("Accepted connection from {}", addr),
        Err(err) => warn!("Could not get peer socket address: {}", err),
      },
      Err(err) => error!("Error accepting connection: {}", err),
    }
  }

  process::exit(0);
}

fn client(add: String, dir: String) -> ! {
  process::exit(0);
}

fn main() {
  env_logger::Builder::from_default_env()
    .filter(None, log::LevelFilter::Info)
    .init();

  let opt = Config::from_args();

  match opt {
    Config::Client { address, dir } => client(address, dir),
    Config::Server { address, dir } => server(address, dir),
  }
}
