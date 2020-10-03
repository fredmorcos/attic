use std::fmt::{self, Debug};
use std::io;
use thiserror::Error;

#[derive(Error)]
pub enum Error {
    #[error("Could not initialize logger, {0}")]
    LogInit(#[from] log::SetLoggerError),
    #[error("Could not read password file: {0}")]
    PassFile(io::Error),
    #[error("Invalid entry at line {0}, missing marker")]
    MissingMarker(usize),
    #[error("Invalid entry at line {0}, missing name")]
    MissingName(usize),
    #[error("Invalid entry at line {0}, missing link")]
    MissingLink(usize),
    #[error("Invalid entry at line {0}, missing username")]
    MissingUsername(usize),
    #[error("Invalid entry at line {0}, missing password")]
    MissingPassword(usize),
    #[error("Invalid entry at line {0}, invalid marker {0}")]
    InvalidEntryMarker(usize, String),
    #[error("Could not run pwgen: {0}")]
    PwGenSpawn(io::Error),
    #[error("Could not wait on pwgen process: {0}")]
    PwGenWait(io::Error),
    #[error("Pwgen failed with exit code {0}")]
    PwGenErr(i32),
    #[error("Pwgen failed (exit code {0}): {1}")]
    PwGenErrMsg(i32, String),
    #[error("Pwgen failed (exit code {0}) but could not read its error message: {1}")]
    PwGenStderrErr(i32, io::Error),
    #[error("Pwgen succeeded but did not generate anything")]
    PwGenNoStdout,
    #[error("Pwgen succeeded but could not read its output: {0}")]
    PwGenStdoutErr(io::Error),
    #[error("Pwgen died from a signal")]
    PwGenDied,
    #[error("Found more than 1 match for {0}")]
    Mismatch(String),
    #[error("No matches found for {0}")]
    NoMatches(String),
    #[error("No default password file found in HOME/.passfile")]
    NoPassFile,
}

impl Debug for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self)
    }
}
