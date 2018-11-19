use std::io;
use std::fs;
use std::ffi::OsString;
use std::path::PathBuf;
use std::time::{SystemTime, SystemTimeError};

pub enum Prefix {
  Dir,
  Link,
  File,
}

impl From<Prefix> for u8 {
  fn from(prefix: Prefix) -> Self {
    match prefix {
      Prefix::Dir => 0,
      Prefix::Link => 1,
      Prefix::File => 2,
    }
  }
}

impl From<Prefix> for char {
  fn from(prefix: Prefix) -> Self {
    match prefix {
      Prefix::Dir => 'D',
      Prefix::Link => 'L',
      Prefix::File => 'F',
    }
  }
}

pub trait Entry {
  const PREFIX: Prefix;
  fn name(&self) -> &OsString;
}

pub struct Dir {
  name: OsString,
  dirs: Vec<Dir>,
  links: Vec<Link>,
  files: Vec<File>,
}

impl Entry for Dir {
  const PREFIX: Prefix = Prefix::Dir;

  fn name(&self) -> &OsString {
    &self.name
  }
}

pub struct Link {
  name: OsString,
  modt: u64,
}

impl Link {
  pub fn mk(name: OsString, modt: u64) -> Self {
    Self { name, modt }
  }

  pub fn mk_from_pathbuf(name: PathBuf, modt: u64) -> Self {
    Self::mk(name.into_os_string(), modt)
  }
}

impl Entry for Link {
  const PREFIX: Prefix = Prefix::Link;

  fn name(&self) -> &OsString {
    &self.name
  }
}

pub struct File {
  name: OsString,
  size: u64,
  modt: u64,
}

impl File {
  pub fn mk(name: OsString, size: u64, modt: u64) -> Self {
    Self { name, size, modt }
  }

  pub fn mk_from_pathbuf(name: PathBuf,
                         size: u64,
                         modt: u64)
                         -> Self
  {
    Self::mk(name.into_os_string(), size, modt)
  }
}

impl Entry for File {
  const PREFIX: Prefix = Prefix::File;

  fn name(&self) -> &OsString {
    &self.name
  }
}

#[derive(Debug)]
pub enum Error {
  FileMetadata(OsString, io::Error),
  FileModTime(OsString, io::Error),
  FileModTimeConv(OsString, SystemTimeError),
  LinkMetadata(OsString, io::Error),
  LinkModTime(OsString, io::Error),
  LinkModTimeConv(OsString, SystemTimeError),
  ReadDir(OsString, io::Error),
  ReadEnt(OsString, io::Error),
  EntType(OsString, io::Error),
  Unsupported(OsString),
}

pub enum Act {
  RemoveDir(OsString),
  RemoveFile(OsString),
  RemoveLink(OsString),
  CreateDir(OsString),
  CreateFile(OsString, u64),
  CreateLink(OsString),
}

#[derive(Debug)]
pub enum DiffError {
  FileDestNewer(OsString),
  LinkDestNewer(OsString),
}

impl Dir {
  pub fn diff(self, mut dest: Dir) -> Result<Vec<Act>, DiffError> {
    let mut actions: Vec<Act> = Vec::new();
    let mut delete: Option<usize> = None;

    'files: for src in self.files {
      if let Some(index) = delete {
        let dst = dest.files.remove(index);
        actions.push(Act::RemoveFile(dst.name));
        delete = None;
      }

      for (i, dst) in dest.files.iter().enumerate() {
        if src.name == dst.name {
          if src.modt > dst.modt {
            actions.push(Act::CreateFile(src.name, src.size));
          } else if src.modt < dst.modt {
            return Err(DiffError::FileDestNewer(dst.name.clone()));
          }

          delete = Some(i);
          continue 'files;
        }
      }

      actions.push(Act::CreateFile(src.name, src.size));
    }

    // 'links: for src in self.links {
    //   for dst in &mut dest.links {
    //   }
    // }

    // 'dirs: for src in self.dirs {
    //   for dst in &mut dest.dirs {
    //   }
    // }

    Ok(actions)
  }

  pub fn mk(name: OsString,
            dirs: Vec<Dir>,
            links: Vec<Link>,
            files: Vec<File>)
            -> Self
  {
    Self { name, dirs, links, files }
  }

  pub fn mk_from_pathbuf(name: PathBuf,
                         dirs: Vec<Dir>,
                         links: Vec<Link>,
                         files: Vec<File>)
                         -> Self
  {
    Self::mk(name.into_os_string(), dirs, links, files)
  }

  pub fn new_from_str(path: &str,
                      ignores: Vec<&str>)
                      -> Result<Option<Self>, Error>
  {
    Self::new_helper(PathBuf::from(path),
                     &mut ignores
                     .into_iter()
                     .map(PathBuf::from)
                     .collect::<Vec<PathBuf>>())
  }

  pub fn new(path: PathBuf,
             ignores: Vec<&str>)
             -> Result<Option<Self>, Error>
  {
    Self::new_helper(path,
                     &mut ignores
                     .into_iter()
                     .map(PathBuf::from)
                     .collect::<Vec<PathBuf>>())
  }

  fn must_ignore(path: &PathBuf, ignores: &mut Vec<PathBuf>) -> bool {
    let mut remove_idx = None;

    for (idx, ignore) in ignores.iter().enumerate() {
      if path.starts_with(ignore) {
        remove_idx = Some(idx);
        break;
      }
    }

    if let Some(idx) = remove_idx {
      ignores.remove(idx);
      return true;
    }

    false
  }

  fn new_helper(path: PathBuf,
                ignores: &mut Vec<PathBuf>)
                -> Result<Option<Self>, Error>
  {
    if Self::must_ignore(&path, ignores) {
      return Ok(None);
    }

    let mut dirs: Vec<Dir> = Vec::new();
    let mut links: Vec<Link> = Vec::new();
    let mut files: Vec<File> = Vec::new();

    let entries = match fs::read_dir(&path) {
      Ok(entries) => entries,
      Err(e) => return Err(Error::ReadDir(path.into_os_string(), e)),
    };

    for ent in entries {
      let ent = match ent {
        Ok(ent) => ent,
        Err(e) => return Err(Error::ReadEnt(path.into_os_string(), e)),
      };

      let typ = ent.file_type().map_err(
        |e| Error::EntType(ent.path().into_os_string(), e))?;

      if typ.is_dir() {
        if let Some(child) = Dir::new_helper(ent.path(), ignores)? {
          dirs.push(child);
        }
      } else if typ.is_symlink() {
        let md = fs::symlink_metadata(ent.path()).map_err(
          |e| Error::LinkMetadata(ent.path().into_os_string(), e))?;

        let modt = md.modified().map_err(
          |e| Error::LinkModTime(ent.path().into_os_string(), e))?;

        let modt = modt.duration_since(SystemTime::UNIX_EPOCH).map_err(
          |e| Error::LinkModTimeConv(ent.path().into_os_string(), e))?
          .as_secs();

        links.push(Link::mk(ent.file_name(), modt));
      } else if typ.is_file() {
        let md = ent.metadata().map_err(
          |e| Error::FileMetadata(ent.path().into_os_string(), e))?;

        let modt = md.modified().map_err(
          |e| Error::FileModTime(ent.path().into_os_string(), e))?;

        let modt = modt.duration_since(SystemTime::UNIX_EPOCH).map_err(
          |e| Error::FileModTimeConv(ent.path().into_os_string(), e))?
          .as_secs();

        files.push(File::mk(ent.file_name(), md.len(), modt));
      } else {
        return Err(Error::Unsupported(ent.path().into_os_string()));
      }
    }

    Ok(Some(Dir::mk_from_pathbuf(path, dirs, links, files)))
  }
}

#[cfg(test)]
mod test {
  use super::Dir;

  #[test]
  fn root_dir() {
    let _info = Dir::new_from_str(
      "/",
      vec!["/dev", "/root", "/proc", "/tmp",
           "/var", "/lost+found", "/sys", "/run",
           "/etc/ipsec.d/", "/etc/NetworkManager",
           "/etc/swanctl", "/etc/pacman.d/gnupg",
           "/etc/audisp", "/etc/sudoers.d",
           "/etc/libvirt/secrets", "/etc/wireguard",
           "/home/lost+found", "/home/fred/.steam"])
      .expect("Failed to load root dir");
  }

  #[test]
  fn home_dir() {
    let _info = Dir::new_from_str(
      "/home/fred",
      vec!["/home/fred/.steam"])
      .expect("Failed to load home dir");
  }
}
