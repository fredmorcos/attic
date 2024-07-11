#![warn(clippy::all)]

use flate2::bufread::DeflateDecoder as Flate2DeflateDecoder;
use libflate::deflate::Decoder as LibflateDecoder;
use log::{debug, error, info, trace, warn};
use std::fs::{self, create_dir, File};
use std::io::{BufReader, Cursor, Read, Write};
use std::path::{Path, PathBuf};
use structopt::StructOpt;
use zune_inflate::DeflateDecoder as ZuneDeflateDecoder;

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

  let mut archive = zip::ZipArchive::new(Cursor::new(contents.clone())).unwrap();
  let mut corrupt_file = archive.by_name(opt.corrupt_file.to_str().unwrap()).unwrap();
  let header_start = corrupt_file.header_start();
  let crc32_start = header_start + 14;
  let data_start = corrupt_file.data_start();
  let data_end = corrupt_file.data_start() + corrupt_file.compressed_size();
  info!("{}:", corrupt_file.enclosed_name().unwrap().display());
  info!("  size   {} (0x{:x}) bytes", corrupt_file.size(), corrupt_file.size());
  info!("  comp   {} (0x{:x}) compressed", corrupt_file.compressed_size(), corrupt_file.compressed_size());
  info!("  CRC    {} (0x{:x}) = 0x{:x}", crc32_start, crc32_start, corrupt_file.crc32());
  info!("  data   {} (0x{:x}) -> {} (0x{:x})", data_start, data_start, data_end, data_end);
  info!("  header {} (0x{:x})", header_start, header_start);

  let outdir = opt.archive.with_extension("outputs");
  let _ = create_dir(&outdir);

  fn create_outfile(outdir: &Path, filename: &str) -> File {
    let mut outfile = outdir.to_path_buf();
    outfile.push(filename);
    File::create(outfile).unwrap()
  }

  let mut outfile = create_outfile(&outdir, "document-zip-rs-all.xml");
  let mut file_contents = Vec::new();
  match corrupt_file.read_to_end(&mut file_contents) {
    Ok(len) => info!("Zip-rs: decompressed all {len} bytes"),
    Err(err) => error!("Zip-rs: error decompressing: {err}"),
  }
  outfile.write_all(&file_contents).unwrap();

  let mut outfile = create_outfile(&outdir, "document-zip-rs-byte-by-byte.xml");
  let buf = &mut [0];
  let mut bytes = 0;
  loop {
    match corrupt_file.read(buf) {
      Ok(len) => {
        let _ = outfile.write(buf).unwrap();
        bytes += len;
      }
      Err(err) => {
        error!("Zip-rs: error extracting at byte {bytes}: {err}");
        break;
      }
    }
  }

  info!("Zip-rs: extracted {bytes} bytes");

  // -------------------------------------------------------------------------------------

  let deflate_contents = &contents[data_start as usize..data_end as usize];
  let deflate_filename = opt.archive.with_extension("deflate");
  File::create(&deflate_filename).unwrap().write_all(deflate_contents).unwrap();
  info!("Wrote deflate file to {}", deflate_filename.display());

  // -------------------------------------------------------------------------------------

  let mut decoder = ZuneDeflateDecoder::new(deflate_contents);
  match decoder.decode_deflate() {
    Ok(decompressed_data) => {
      match create_outfile(&outdir, "document-zune-all.xml").write_all(&decompressed_data) {
        Ok(()) => info!("Zune: deflated {} bytes", decompressed_data.len()),
        Err(err) => error!("Zune: error deflating (data size {} bytes): {err}", decompressed_data.len()),
      }
    }
    Err(err) => error!("Zune: error deflating: {err}"),
  }

  // -------------------------------------------------------------------------------------

  // let mut deflate_file = BufReader::new(File::open(&deflate_filename).unwrap());
  let mut deflate_file = BufReader::new(File::open("/home/fred/TMP/word-document-only-2.deflate").unwrap());
  let mut decoder = Flate2DeflateDecoder::new(&mut deflate_file);
  let mut outfile = create_outfile(&outdir, "document-flate2.xml");
  let buf = &mut [0];
  let mut bytes = 0;

  loop {
    match decoder.read(buf) {
      Ok(len) => {
        let _ = outfile.write(buf).unwrap();
        bytes += len;
      }
      Err(err) => {
        error!("Flate2: error decompressing at byte {bytes}: {err}");
        break;
      }
    }
  }

  info!("Flate2: decompressed {bytes} bytes");
  info!("Flate2: decompressor read {} compressed bytes", decoder.total_in());
  info!("Flate2: decompressor produced {} decompressed bytes", decoder.total_out());

  // -------------------------------------------------------------------------------------

  // let mut deflate_file = BufReader::new(File::open(&deflate_filename).unwrap());
  let mut deflate_file = BufReader::new(File::open("/home/fred/TMP/word-document-only-2.deflate").unwrap());
  let mut decoder = LibflateDecoder::new(&mut deflate_file);
  let mut outfile = create_outfile(&outdir, "document-libflate.xml");
  let buf = &mut [0];
  let mut decompressed_bytes = 0;

  loop {
    match decoder.read(buf) {
      Ok(len) => {
        let _ = outfile.write(buf).unwrap();
        decompressed_bytes += len;
      }
      Err(err) => {
        error!("Libflate: error decompressing at byte {decompressed_bytes}: {err}");
        break;
      }
    }
  }

  info!("Libflate: decompressed {decompressed_bytes} bytes");
}
