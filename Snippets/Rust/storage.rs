#![warn(clippy::all)]

use crate::error::Err;
use crate::Res;
use directories::ProjectDirs;
use fnv::FnvHashMap;
use reqwest::blocking::Client;
use reqwest::Url;
use std::fs;
use std::path::{Path, PathBuf};

struct FileProperties {
  url: Url,
  filename: PathBuf,
  is_compressed: bool,
  contents: Option<&'static [u8]>,
}

impl FileProperties {
  fn new(url: Url, filename: PathBuf, is_compressed: bool) -> Res<Self> {
    Ok(Self { url, filename, is_compressed, contents: None })
  }

  /// Get URL.
  fn url(&self) -> &Url {
    &self.url
  }

  /// The filename.
  fn filename(&self) -> &Path {
    &self.filename
  }

  /// Whether the file is compressed.
  fn is_compressed(&self) -> bool {
    self.is_compressed
  }

  /// Get a mutable reference to the file contents array.
  fn contents(&mut self) -> Res<&[u8]> {
    if let Some(contents) = self.contents {
      return Ok(contents);
    }

    todo!()
  }
}

type DownloadInitFunction<T> = fn(&str, Option<u64>) -> T;
type DownloadProgressFunction<T> = fn(&T, u64);
type DownloadFinishFunction<T> = fn(&T);

type ExtractInitFunction<T> = fn(&str) -> T;
type ExtractProgressFunction<T> = fn(&T, u64);
type ExtractFinishFunction<T> = fn(&T);

pub struct FileStorageBackend<T1, T2> {
  base_url: Url,
  cache_directory: PathBuf,
  files: FnvHashMap<String, FileProperties>,

  client: Client,

  download_init: Option<DownloadInitFunction<T1>>,
  download_progress: Option<DownloadProgressFunction<T1>>,
  download_finish: Option<DownloadFinishFunction<T1>>,

  extract_init: Option<ExtractInitFunction<T2>>,
  extract_progress: Option<ExtractProgressFunction<T2>>,
  extract_finish: Option<ExtractFinishFunction<T2>>,
}

impl<T1, T2> FileStorageBackend<T1, T2> {
  pub fn new(project_dirs: &ProjectDirs, base_url: &str, name: &str) -> Res<Self> {
    let cache_directory = project_dirs.cache_dir().join(name);
    fs::create_dir_all(&cache_directory)?;

    Ok(Self {
      base_url: Url::parse(base_url)?,
      cache_directory,
      files: FnvHashMap::default(),

      client: Client::builder().build()?,

      download_init: None,
      download_progress: None,

      download_finish: None,

      extract_init: None,
      extract_progress: None,
      extract_finish: None,
    })
  }

  pub fn add_file(mut self, name: String, filename: &str, is_compressed: bool) -> Res<Self> {
    let url = self.base_url.join(filename)?;
    let filename = self.cache_directory.join(filename);
    let properties = FileProperties::new(url, filename, is_compressed)?;

    if self.files.contains_key(&name) {
      return Err::name_already_registered(name);
    }
    self.files.insert(name, properties);

    Ok(self)
  }

  pub fn with_download_init_function(mut self, func: DownloadInitFunction<T1>) -> Self {
    self.download_init = Some(func);
    self
  }

  pub fn with_download_progress_function(mut self, func: DownloadProgressFunction<T1>) -> Self {
    self.download_progress = Some(func);
    self
  }

  pub fn with_download_finish_function(mut self, func: DownloadFinishFunction<T1>) -> Self {
    self.download_finish = Some(func);
    self
  }

  pub fn with_extract_init_function(mut self, func: ExtractInitFunction<T2>) -> Self {
    self.extract_init = Some(func);
    self
  }

  pub fn with_extract_progress_function(mut self, func: ExtractProgressFunction<T2>) -> Self {
    self.extract_progress = Some(func);
    self
  }

  pub fn with_extract_finish_function(mut self, func: ExtractFinishFunction<T2>) -> Self {
    self.extract_finish = Some(func);
    self
  }
}
