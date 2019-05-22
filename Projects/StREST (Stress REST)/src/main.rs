extern crate rayon;
extern crate reqwest;
extern crate time;
extern crate structopt;

use std::io::Write;

use reqwest::Response;
use time::Timespec;
use rayon::prelude::*;

use structopt::StructOpt;

static DATA: &'static str = include_str!("../data.json");

#[derive(StructOpt)]
struct Opt {
  #[structopt(short = "u", help = "Url (example: http://localhost:9292/traces)")]
  url: String,

  #[structopt(short = "t", help = "Number of simultaneous requests")]
  threads: usize,

  #[structopt(short = "n", help = "Number of requests to make")]
  requests: usize,
}

fn main() -> Result<(), Box<std::error::Error>> {
  let opt = Opt::from_args();

  let stderr = std::io::stderr();
  let mut stderr = stderr.lock();

  writeln!(stderr, "Initializing...")?;

  rayon::ThreadPoolBuilder::new().num_threads(opt.threads).build_global()?;

  let mut clients = Vec::with_capacity(opt.requests);

  for _ in 0 .. opt.requests {
    clients.push(reqwest::Client::new().post(&opt.url).body(DATA));
  }

  let clients = clients.into_par_iter().enumerate().map(|(i, client)| {
    let start = time::get_time();
    let resp = client.send();
    let end = time::get_time();
    (i, start, end, resp)
  });

  type Res = (usize, Timespec, Timespec, Result<Response, reqwest::Error>);

  writeln!(stderr, "Starting POSTs...")?;

  let start = time::get_time();
  let results: Vec<Res> = clients.collect();
  let post_time = time::get_time() - start;
  let post_n = results.len();

  let mut clients = Vec::with_capacity(opt.requests);

  for (i, start, end, resp) in results {
    match resp {
      Ok(mut resp) => {
        write!(stderr, "  POST {:>3} took {} ", i, end - start)?;

        let status = resp.status();

        if status.is_success() {
          let body = resp.text().unwrap();
          writeln!(stderr, "(ID = {})", body)?;
          clients.push(
            reqwest::Client::new().get(
              &format!("{}/{}", opt.url, body)));
        } else if status.is_server_error() {
          writeln!(stderr, "(Server Error = {})", resp.text().unwrap())?;
        } else {
          writeln!(stderr, "(Error = {:?})", resp.status())?;
        }
      },
      Err(err) => {
        writeln!(stderr, "  POST {:>3} Error = {}", i, err)?;
      },
    }
  }

  let clients = clients.into_par_iter().enumerate().map(|(i, client)| {
    let start = time::get_time();
    let resp = client.send();
    let end = time::get_time();
    (i, start, end, resp)
  });

  writeln!(stderr, "Starting GETs...")?;

  let start = time::get_time();
  let results: Vec<Res> = clients.collect();
  let get_time = time::get_time() - start;
  let get_n = results.len();

  for (i, start, end, resp) in results {
    match resp {
      Ok(mut resp) => {
        write!(stderr, "  GET {:>3} took {} ", i, end - start)?;

        let status = resp.status();

        if status.is_success() {
          let body: String = resp.text().unwrap().chars().take(10).collect();
          writeln!(stderr, "(Data = {}...)", body)?;
        } else if status.is_server_error() {
          writeln!(stderr, "(Server Error = {})", resp.text().unwrap())?;
        } else {
          writeln!(stderr, "(Error = {:?})", resp.status())?;
        }
      },
      Err(err) => {
        writeln!(stderr, "  GET {:>3} Error = {}", i, err)?;
      },
    }
  }

  writeln!(stderr, "Finished {} POST requests in {}", post_n, post_time)?;
  writeln!(stderr, "Finished {} GET requests in {}",  get_n, get_time)?;

  println!("{:>4} {:>10} {:>10}",
           opt.threads,
           post_time.num_milliseconds(),
           get_time.num_milliseconds());

  Ok(())
}
