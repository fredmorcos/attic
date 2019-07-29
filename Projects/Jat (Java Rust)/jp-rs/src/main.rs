#![warn(clippy::all)]

use regex::Regex;
use std::error::Error;
use std::fs;
use std::path::PathBuf;
use structopt::StructOpt;
use walkdir::WalkDir;

#[derive(StructOpt)]
#[structopt(name = "jp-rs")]
struct Opt {
    dir: PathBuf,
}

fn main() -> Result<(), Box<dyn Error>> {
    let re = Regex::new(".+extends Node \\{")?;

    let opt = Opt::from_args();
    for entry in WalkDir::new(opt.dir) {
        let entry = entry?;

        if let Some(ext) = entry.path().extension() {
            if ext == "java" {
                match fs::read_to_string(entry.path()) {
                    Ok(cts) => {
                        if re.is_match(&cts) {
                            println!("> {:?}", entry.path());
                        }
                    }
                    Err(err) => eprintln!("ERR {:?}: {}", entry.path(), err),
                }
            }
        }
    }

    Ok(())
}
