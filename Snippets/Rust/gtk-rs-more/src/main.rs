#![feature(tool_lints)]
#![warn(clippy::all)]

extern crate glib;
extern crate gdk;
extern crate gdk_pixbuf;
extern crate gtk;

use gdk_pixbuf::PixbufExt;
use gtk::prelude::*;

use std::error::Error;
use std::fmt::Debug;
use std::fmt::Display;

use std::rc::Rc;
use std::cell::RefCell;

enum AppError {
  GtkInit(&'static str),
  IconTheme,
  FolderIcon(gtk::Error),
  NoFolderIcon(&'static str),
}

impl Error for AppError {
  fn cause(&self) -> Option<&Error> {
    if let AppError::FolderIcon(err) = self {
      Some(err)
    } else {
      None
    }
  }
}

impl Debug for AppError {
  fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
    match self {
      AppError::GtkInit(msg) => f.write_str(msg),
      AppError::IconTheme => f.write_str("Cannot get Gtk+ icon theme"),
      AppError::FolderIcon(err) => f.write_str(&err.to_string()),
      AppError::NoFolderIcon(msg) => f.write_str(msg),
    }
  }
}

impl Display for AppError {
  fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
    write!(f, "{:?}", self)
  }
}

struct Listing {
  pub model: gtk::ListStore,
  pub view: gtk::TreeView,
  pub scroll: gtk::ScrolledWindow,
}

impl Listing {
  fn new(icon_width: i32) -> Listing {
    let model = gtk::ListStore::new(
      &[gdk_pixbuf::Pixbuf::static_type(),
        String::static_type()]);

    let type_col = gtk::TreeViewColumn::new();
    let type_col_rend = gtk::CellRendererPixbuf::new();

    type_col.set_title("");
    type_col.set_resizable(false);
    type_col.pack_start(&type_col_rend, true);
    type_col.add_attribute(&type_col_rend, "pixbuf", 0);
    type_col.set_fixed_width(icon_width);

    let name_col = gtk::TreeViewColumn::new();
    let name_col_rend = gtk::CellRendererText::new();

    name_col.set_title("Name");
    name_col.set_resizable(true);
    name_col.pack_start(&name_col_rend, true);
    name_col.add_attribute(&name_col_rend, "text", 1);

    let view = gtk::TreeView::new_with_model(&model);

    view.set_headers_visible(true);
    view.append_column(&type_col);
    view.append_column(&name_col);

    let scroll = gtk::ScrolledWindow::new(None, None);

    scroll.add(&view);

    Listing { model, view, scroll }
  }

  fn insert(&mut self, icon: &gdk_pixbuf::Pixbuf, text: &str) {
    self.model.insert_with_values(
      None,
      &[0, 1],
      &[icon, &String::from(text)]);
  }
}

fn main() -> Result<(), AppError> {
  gtk::init().map_err(|glib::BoolError(msg)| AppError::GtkInit(msg))?;

  let window = gtk::Window::new(gtk::WindowType::Toplevel);

  window.set_title("Files");
  window.set_default_size(800, 600);

  let icon_theme = gtk::IconTheme::get_default()
    .ok_or_else(|| AppError::IconTheme)?;

  let dir_icon = icon_theme.load_icon_for_scale(
    "folder",
    16,
    window.get_scale_factor(),
    gtk::IconLookupFlags::empty()
  ).map_err(AppError::FolderIcon)?
   .ok_or_else(|| AppError::NoFolderIcon("Cannot find folder icon"))?;

  let dir_icon_width = dir_icon.get_width();

  let listing1 = Rc::new(RefCell::new(Listing::new(dir_icon_width)));
  let listing2 = Rc::new(RefCell::new(Listing::new(dir_icon_width)));

  let stack = gtk::Stack::new();

  stack.add_named(&listing1.borrow().scroll, "One");
  stack.add_named(&listing2.borrow().scroll, "Two");

  window.connect_delete_event(|_, _| {
    gtk::main_quit();
    gtk::Inhibit(false)
  });

  let dir_icon_clone = dir_icon.clone();
  let stack_clone = stack.clone();

  window.connect_key_press_event(move |_, key| {
    match key.get_keyval() {
      gdk::enums::key::Escape => {
        gtk::main_quit();
        gtk::Inhibit(false)
      },
      gdk::enums::key::Left => {
        stack_clone.set_transition_type(gtk::StackTransitionType::SlideRight);
        stack_clone.set_visible_child(&listing1.borrow().scroll);
        gtk::Inhibit(true)
      },
      gdk::enums::key::Right => {
        stack_clone.set_transition_type(gtk::StackTransitionType::SlideLeft);
        stack_clone.set_visible_child(&listing2.borrow().scroll);
        gtk::Inhibit(true)
      },
      gdk::enums::key::plus => {
        let visible_child = stack_clone.get_visible_child();

        match visible_child {
          None => panic!("Empty visible child"),
          Some(c) => if c == listing1.borrow().scroll {
            listing1.borrow_mut()
          } else if c == listing2.borrow().scroll {
            listing2.borrow_mut()
          } else {
            panic!("Unrecognized visible child")
          },
        }.insert(&dir_icon_clone, "Hello, World!");

        gtk::Inhibit(true)
      },
      _ => gtk::Inhibit(false)
    }
  });

  window.add(&stack);
  window.show_all();

  gtk::main();

  Ok(())
}
