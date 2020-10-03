use crate::err::Error;
use log::info;
use std::path::PathBuf;

fn default_passfile() -> Option<PathBuf> {
    let mut passfile = dirs::home_dir()?;

    passfile.push(".passfile");

    if passfile.is_file() {
        return Some(passfile);
    }

    None
}

pub fn get_passfile(file: Option<PathBuf>) -> Result<PathBuf, Error> {
    let file = file.or_else(default_passfile);

    if let Some(file) = file {
        info!("Found password file at {}", file.display());
        Ok(file)
    } else {
        Err(Error::NoPassFile)
    }
}
