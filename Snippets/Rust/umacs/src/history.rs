use std::fs;
use std::path::PathBuf;
use std::sync::OnceLock;

use directories::ProjectDirs;
use log::{info, warn};
use rustyline::DefaultEditor;

fn get_file() -> &'static PathBuf {
  const PKG_NAME: &str = env!("CARGO_PKG_NAME");
  static HISTORY_FILE: OnceLock<PathBuf> = OnceLock::new();

  HISTORY_FILE.get_or_init(|| {
    let default_history_file = format!(".{PKG_NAME}_history");
    let default_history_file_msg =
      format!("Using the default {default_history_file} as history file");

    if let Some(dirs) = ProjectDirs::from("com", "fredmorcos", "umacs") {
      let data_dir = dirs.data_dir();
      if let Err(err) = fs::create_dir_all(data_dir) {
        warn!("Could not create data dir {}: {err}", data_dir.display());
        warn!("{}", default_history_file_msg);
        default_history_file.into()
      } else {
        data_dir.join("history")
      }
    } else {
      warn!("Could not get location of data dir");
      warn!("{}", default_history_file_msg);
      default_history_file.into()
    }
  })
}

pub fn load_file(line_editor: &mut DefaultEditor) {
  let history_file = get_file();
  if let Err(err) = line_editor.load_history(history_file) {
    warn!("Could not load history file {}: {err}", history_file.display());
  } else {
    info!("Loaded history from {}", history_file.display());
  }
}

pub fn save_file(line_editor: &mut DefaultEditor) {
  let history_file = get_file();
  if let Err(err) = line_editor.save_history(history_file) {
    warn!("Could not save history file {}: {err}", history_file.display(),);
  } else {
    info!("Saved history to {}", history_file.display());
  }
}
