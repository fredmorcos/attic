pub fn get_level(verbose: u8) -> log::LevelFilter {
  match verbose {
    0 => log::LevelFilter::Off,
    1 => log::LevelFilter::Error,
    2 => log::LevelFilter::Warn,
    3 => log::LevelFilter::Info,
    4 => log::LevelFilter::Debug,
    _ => log::LevelFilter::Trace,
  }
}

pub fn init(log_level: log::LevelFilter) {
  if let Err(err) = env_logger::Builder::new()
    .filter_level(log_level)
    .filter_module("rustyline", log::LevelFilter::Info)
    .try_init()
  {
    eprintln!("Error initializing logger: {err}");
  }
}
