#[derive(Debug, clap::Parser)]
#[clap(author, version, about, long_about = None)]
pub struct Opts {
  /// Verbose output (can be specified multiple times)
  #[clap(short, long, action = clap::ArgAction::Count)]
  pub verbose: u8,
}
