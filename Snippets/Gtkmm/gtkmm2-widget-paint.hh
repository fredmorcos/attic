#pragma once

#include <gtkmm.h>

/**
 *  \brief Displays a green circle when in saved state, and a red circle when in modified state.
 *
 *  Used to represent a document's saved state.
 */
class Modified: public Gtk::Widget {
 public:
  Modified();

  void set_modified() noexcept;
  bool is_modified() const noexcept;

  void set_saved() noexcept;

 private:
  bool modified;

 protected:
  virtual void on_size_request(Gtk::Requisition *requisition);
  virtual void on_size_allocate(Gtk::Allocation &allocation);
  virtual void on_realize();
  virtual void on_unrealize();
  virtual bool on_expose_event(GdkEventExpose *event);

  Glib::RefPtr<Gdk::Window> gdkWindow;
};
