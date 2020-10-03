use std::path::PathBuf;
use structopt::StructOpt;

#[derive(Debug, StructOpt)]
pub enum Cmd {
    #[structopt(about = "Check and print password stats")]
    Check {
        #[structopt(help = "Password file")]
        file: Option<PathBuf>,
    },
    #[structopt(name = "gen", about = "Generate a password")]
    Generate,
    #[structopt(about = "Retrieve a password")]
    Get {
        #[structopt(name = "account name", help = "Exact match for an account name")]
        acc: String,
        #[structopt(help = "Format: %N = Name, %L = Link, %U = Username, %P = Password")]
        format: String,
        #[structopt(help = "Password file")]
        file: Option<PathBuf>,
    },
    #[structopt(name = "ls", about = "Search for passwords")]
    List {
        #[structopt(help = "Query for an account name")]
        query: String,
        #[structopt(help = "Password file")]
        file: Option<PathBuf>,
    },
}

#[derive(Debug, StructOpt)]
#[structopt(about = "Dumb Password Manager")]
pub struct Pw {
    #[structopt(short, long, parse(from_occurrences))]
    pub verbose: u8,
    #[structopt(subcommand)]
    pub command: Cmd,
}
