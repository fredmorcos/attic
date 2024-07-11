#![warn(clippy::all)]

use log::{debug, error, info, trace, warn};
use rand::Rng;
use std::io::{Cursor, Read, Write};
use std::path::PathBuf;
use std::sync::atomic::AtomicUsize;
use std::sync::Arc;
use std::time::Duration;
use std::{fs, thread};
use structopt::StructOpt;

const NTHREADS: u64 = 16;

#[derive(Debug, StructOpt)]
#[structopt(about = "Zip file recovery and correction program")]
#[structopt(author = "Fred Morcos <fm@fredmorcos.com>")]
struct Opt {
  /// Verbose output (can be specified multiple times)
  #[structopt(short, long, parse(from_occurrences))]
  verbose: u8,

  /// The corrupt zip archive file.
  #[structopt(short, long, name = "ARCHIVE")]
  archive: PathBuf,

  /// Path to the corrupt file inside the zip archive (e.g. word/document.xml).
  #[structopt(short, long, name = "CORRUPT-FILE")]
  corrupt_file: PathBuf,
}

fn main() {
  let opt = Opt::from_args();

  let log_level = match opt.verbose {
    0 => log::LevelFilter::Off,
    1 => log::LevelFilter::Error,
    2 => log::LevelFilter::Warn,
    3 => log::LevelFilter::Info,
    4 => log::LevelFilter::Debug,
    _ => log::LevelFilter::Trace,
  };

  env_logger::Builder::new().filter_level(log_level).try_init().unwrap();
  error!("Error output enabled.");
  warn!("Warning output enabled.");
  info!("Info output enabled.");
  debug!("Debug output enabled.");
  trace!("Trace output enabled.");

  let mut archive_file = fs::File::open(&opt.archive).unwrap();
  let mut contents = Vec::new();
  archive_file.read_to_end(&mut contents).unwrap();

  let (crc32_start, crc32_end, data_start, data_end) = {
    let mut archive = zip::ZipArchive::new(Cursor::new(contents.clone())).unwrap();
    let corrupt_file = archive.by_name(opt.corrupt_file.to_str().unwrap()).unwrap();
    let header_start = corrupt_file.header_start();
    let crc32_start = header_start + 14;
    let crc32_end = crc32_start + 4;
    let data_start = corrupt_file.data_start();
    let data_end = corrupt_file.data_start() + corrupt_file.compressed_size();
    info!("{}:", corrupt_file.enclosed_name().unwrap().display());
    info!("  size   {} (0x{:x}) bytes", corrupt_file.size(), corrupt_file.size());
    info!("  comp   {} (0x{:x}) compressed", corrupt_file.compressed_size(), corrupt_file.compressed_size());
    info!("  CRC    {} (0x{:x}) = 0x{:x}", crc32_start, crc32_start, corrupt_file.crc32());
    info!("  data   {} (0x{:x}) -> {} (0x{:x})", data_start, data_start, data_end, data_end);
    info!("  header {} (0x{:x})", header_start, header_start);
    (crc32_start, crc32_end, data_start, data_end)
  };

  let completed_crc32 = Arc::new(AtomicUsize::new(0));
  let completed_data = Arc::new(AtomicUsize::new(0));
  let invalid_archives = Arc::new(AtomicUsize::new(0));
  let invalid_files = Arc::new(AtomicUsize::new(0));
  let invalid_file_contents = Arc::new(AtomicUsize::new(0));
  let valid_files = Arc::new(AtomicUsize::new(0));

  thread::scope(|s| {
    for _ in 0..NTHREADS {
      s.spawn(|| {
        let completed_crc32 = Arc::clone(&completed_crc32);
        let completed_data = Arc::clone(&completed_data);
        let invalid_archives = Arc::clone(&invalid_archives);
        let invalid_files = Arc::clone(&invalid_files);
        let invalid_file_contents = Arc::clone(&invalid_file_contents);
        let valid_files = Arc::clone(&valid_files);
        let contents = contents.clone();
        let mut rng = rand::thread_rng();

        loop {
          let mut contents = contents.clone();
          let mutate_crc32: bool = rng.gen_bool(0.1);

          let (mutate_nbytes, mutate_location) = if mutate_crc32 {
            let mutate_nbytes: u64 = rng.gen_range(1..=4);
            let mutate_location: u64 = rng.gen_range(crc32_start..=(crc32_end - mutate_nbytes));
            (mutate_nbytes, mutate_location)
          } else {
            let mutate_nbytes: u64 = rng.gen_range(1..=8);
            let mutate_location: u64 = rng.gen_range(data_start..=(data_end - mutate_nbytes));
            (mutate_nbytes, mutate_location)
          };

          for i in 0..mutate_nbytes {
            let byte_value: u8 = rng.gen();
            unsafe { *contents.get_unchecked_mut((mutate_location + i) as usize) = byte_value };
          }

          let archive_contents = Cursor::new(Vec::from(contents.as_slice()));
          let mut archive = match zip::ZipArchive::new(archive_contents) {
            Ok(archive) => archive,
            Err(_) => {
              invalid_archives.fetch_add(1, std::sync::atomic::Ordering::Release);
              continue;
            }
          };
          let mut file = match archive.by_name(opt.corrupt_file.to_str().unwrap()) {
            Ok(file) => file,
            Err(_) => {
              invalid_files.fetch_add(1, std::sync::atomic::Ordering::Release);
              continue;
            }
          };
          let mut file_contents = Vec::new();
          if file.read_to_end(&mut file_contents).is_ok() {
            let unique_marker = format!("{mutate_location}_{mutate_nbytes}_{}", rng.gen::<u64>());
            let archive_filename = opt.archive.with_extension(&unique_marker);
            let file_filename = format!("document_{unique_marker}.xml");
            println!("Found valid file, writing to {} (and {})!", archive_filename.display(), file_filename);
            fs::File::create(archive_filename).unwrap().write_all(&contents).unwrap();
            fs::File::create(file_filename).unwrap().write_all(&file_contents).unwrap();
            valid_files.fetch_add(1, std::sync::atomic::Ordering::Release);
          } else {
            invalid_file_contents.fetch_add(1, std::sync::atomic::Ordering::Release);
          }

          if mutate_crc32 {
            completed_crc32.fetch_add(1, std::sync::atomic::Ordering::Release);
          } else {
            completed_data.fetch_add(1, std::sync::atomic::Ordering::Release);
          }
        }
      });
    }

    let completed_crc32 = Arc::clone(&completed_crc32);
    let completed_data = Arc::clone(&completed_data);
    let invalid_archives = Arc::clone(&invalid_archives);
    let invalid_files = Arc::clone(&invalid_files);
    let invalid_file_contents = Arc::clone(&invalid_file_contents);
    let valid_files = Arc::clone(&valid_files);
    s.spawn(move || loop {
      thread::sleep(Duration::from_secs(10));
      let completed_crc32 = completed_crc32.load(std::sync::atomic::Ordering::SeqCst);
      let completed_data = completed_data.load(std::sync::atomic::Ordering::SeqCst);
      let invalid_archives = invalid_archives.load(std::sync::atomic::Ordering::SeqCst);
      let invalid_files = invalid_files.load(std::sync::atomic::Ordering::SeqCst);
      let invalid_file_contents = invalid_file_contents.load(std::sync::atomic::Ordering::SeqCst);
      let valid_files = valid_files.load(std::sync::atomic::Ordering::SeqCst);
      info!("Progress: Completed {} mutations", completed_crc32 + completed_data);
      info!("  {} crc32 mutations", completed_crc32);
      info!("  {} data mutations", completed_data);
      info!("  {} invalid archives", invalid_archives);
      info!("  {} invalid files", invalid_files);
      info!("  {} invalid file contents", invalid_file_contents);
      info!("  {} valid files", valid_files);
    });
  });
}
