extern crate term;

fn main () {
  let mut t = match ::term::Terminal::new(std::io::stdio::stdout()) {
    Ok(term) => term,
    Err(err) => return println!("error!: {}", err)
  };

  t.fg(::term::color::YELLOW);

  t.write_str("hello\n");

  t.flush();

  t.reset();

  println!("hello!");
}
