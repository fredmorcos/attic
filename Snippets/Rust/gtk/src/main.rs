extern crate gtk;

use gtk::prelude::*;
use gtk::{Window, WindowType};
use std::process;

fn main() {
  if let Err(err) = gtk::init() {
    eprintln!("Failed to initialize Gtk: {}", err);
    process::exit(1);
  }

  let window = Window::new(WindowType::Toplevel);

  window.set_title("ffm");
  window.set_default_size(250, 250);

  window.connect_delete_event(|_, _| {
    gtk::main_quit();
    Inhibit(false)
  });

  window.show_all();

  gtk::main();
}
