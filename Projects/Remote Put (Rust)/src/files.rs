use std::{fs, time};
use std::fmt::{self, Display, Formatter};

#[derive(PartialEq)]
pub enum FileType {
  RegularFile,
  Symlink,
}

impl Display for FileType {
  fn fmt(&self, f: &mut Formatter) -> Result<(), fmt::Error> {
    match *self {
      FileType::RegularFile => write!(f, "regular file"),
      FileType::Symlink => write!(f, "symlink"),
    }
  }
}

pub fn get_file_metadata(filename: &str) -> Result<fs::Metadata, String> {
  let metadata: fs::Metadata = match fs::symlink_metadata(filename) {
    Ok(md) => md,
    Err(err) => {
      return Err(format!(
        "Cannot get symlink metadata for {}: {}",
        filename,
        err
      ))
    }
  };

  let metadata: fs::Metadata = if metadata.file_type().is_symlink() {
    metadata
  } else {
    match fs::metadata(filename) {
      Ok(md) => md,
      Err(err) => {
        return Err(format!(
          "Cannot get file metadata for {}: {}",
          filename,
          err
        ))
      }
    }
  };

  if metadata.file_type().is_dir() {
    return Err(format!("Directory not supported: {}", filename));
  }

  Ok(metadata)
}

pub fn get_file_info(filename: &str) -> Result<(FileType, u64, u64), String> {
  match get_file_metadata(filename) {
    Ok(md) => {
      let mtime: u64 = match md.modified() {
        Ok(mt) => {
          match mt.duration_since(time::UNIX_EPOCH) {
            Ok(mtime) => mtime.as_secs(),
            Err(err) => {
              return Err(format!("Cannot calculate file mtime: {}", err))
            }
          }
        }
        Err(err) => return Err(format!("Cannot get file mtime: {}", err)),
      };

      let file_type: FileType = if md.file_type().is_file() {
        FileType::RegularFile
      } else {
        FileType::Symlink
      };

      Ok((file_type, md.len(), mtime))
    }
    Err(err) => Err(err),
  }
}
