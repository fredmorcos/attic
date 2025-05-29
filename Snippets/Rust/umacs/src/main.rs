mod arguments;
mod evaluator;
mod history;
mod logging;
mod parser;
mod printer;

use arguments::Opts;

use clap::Parser;
use rustyline::error::ReadlineError;
use rustyline::{DefaultEditor, Result};

fn main() -> Result<()> {
  let opts = Opts::parse();
  let log_level = logging::get_level(opts.verbose);
  logging::init(log_level);

  let mut line_editor = DefaultEditor::new()?;
  history::load_file(&mut line_editor);

  loop {
    let readline = line_editor.readline("USER> ");
    match readline {
      Ok(line) => {
        let _ = line_editor.add_history_entry(&line);
        let parsed = parser::read(&line);
        let result = evaluator::eval(parsed);
        printer::print(result);
      }
      Err(ReadlineError::Interrupted) => {
        eprintln!("CTRL-C");
        break;
      }
      Err(ReadlineError::Eof) => {
        eprintln!("CTRL-D");
        break;
      }
      Err(err) => {
        eprintln!("Error: {}", err);
        break;
      }
    }
  }

  history::save_file(&mut line_editor);
  Ok(())
}
