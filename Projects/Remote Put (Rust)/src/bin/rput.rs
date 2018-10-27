// -*- flycheck-rust-crate-type: "bin"; flycheck-rust-binary-name: "rput"; -*-

use std::{env, process};
use std::io::Write;
use std::net::{SocketAddr, TcpListener, TcpStream};

extern crate rput;

use rput::files;
use rput::system::{pdie, pinfo};

fn usage(exit_code: u8) {
  eprintln!("usage: bkd [-h] -a host -p port -f file");
  process::exit(i32::from(exit_code));
}

fn main() {
  let mut args = env::args();
  let mut addr = None;
  let mut port = None;
  let mut file = None;

  args.next(); // skip process name

  loop {
    match args.next() {
      None => break,
      Some(arg) => {
        if arg == "-h" {
          usage(0);
        } else if arg == "-a" {
          addr = match args.next() {
            Some(a) => Some(a),
            None => pdie(format_args!("No host given for -a, see -h")),
          };
        } else if arg == "-p" {
          port = match args.next() {
            Some(p) => Some(p),
            None => pdie(format_args!("No port given for -p, see -h")),
          };
        } else if arg == "-f" {
          file = match args.next() {
            Some(d) => Some(d),
            None => pdie(format_args!("No file given for -f, see -h")),
          };
        } else {
          pdie(format_args!("Unrecognized argument `{}`, see -h", arg));
        }
      }
    }
  }

  let addr: String = match addr {
    Some(addr) => addr,
    None => pdie(format_args!("No host given using -a, see -h")),
  };

  let port: u16 = match port {
    None => pdie(format_args!("No port given using -p, see -h")),
    Some(port) => {
      match port.parse::<u16>() {
        Ok(port) => port,
        Err(err) => pdie(format_args!("Cannot parse port value {}: {}", port, err)),
      }
    }
  };

  let file: String = match file {
    Some(file) => file,
    None => pdie(format_args!("No file given using -f, see -h")),
  };

  let addr: SocketAddr = match format!("{}:{}", addr, port).parse() {
    Ok(addr) => addr,
    Err(err) => pdie(format_args!("Could not parse address {} with port {}: {}", addr, port, err)),
  };

  let (file_type, len, mtime): (files::FileType, u64, u64) =
    match files::get_file_info(&file) {
      Ok(info) => info,
      Err(err) => pdie(format_args!("{}", err)),
    };

  pinfo(format_args!("{} is a {}", &file, file_type));
  pinfo(format_args!("  Size:  {} bytes", len));
  pinfo(format_args!("  Mtime: {}", mtime));

  let listen: TcpListener = match TcpListener::bind(&addr) {
    Ok(listener) => {
      pinfo(format_args!("Listening on {}", &addr));
      listener
    }
    Err(err) => pdie(format_args!("Cannot listen on socket: {}", err)),
  };

  let mut stream: TcpStream = match listen.incoming().next() {
    None => pdie(format_args!("No incoming connection")),
    Some(stream) => {
      match stream {
        Ok(stream) => stream,
        Err(err) => pdie(format_args!("Could not accept connection: {}", err)),
      }
    }
  };

  let info_line: String =
    format!("{}{}{}{}{}{}{}{}",
            if file_type == files::FileType::RegularFile { 'F' } else { 'L' },
            '\0', len, '\0', mtime, '\0', &file, '\0');

  if let Err(err) = stream.write_all(info_line.as_bytes()) {
    pdie(format_args!("Could not send file info: {}", err));
  }
}
