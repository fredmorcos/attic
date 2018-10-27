#ifndef UI_H_
#define UI_H_

#include <libglademm.h>
#include <gtkmm/window.h>
#include <gtkmm/imagemenuitem.h>
#include <gtkmm/notebook.h>
#include <gtkmm/main.h>
#include <gtkmm/label.h>
#include <gtkmm/aboutdialog.h>
#include <gtkmm/filechooserdialog.h>
#include <gtkmm/stock.h>

class UI
{
	private:
		Glib::RefPtr<Gnome::Glade::Xml> refXml;
		Gtk::Window *mainWindow;
		Gtk::ImageMenuItem *newDocMenuItem, *openDocMenuItem, 
							*saveDocMenuItem, *saveAsDocMenuItem, 
							*closeDocMenuItem, *quitDocMenuItem, 
							*aboutMenuItem;
		Gtk::Notebook *mainNotebook;
		Gtk::AboutDialog *aboutDialog;
		bool docQuit(GdkEventButton* event);
		bool docNew(GdkEventButton* event);
		bool docClose(GdkEventButton* event);
		bool aboutShow(GdkEventButton* event);
		bool openFileDialog(GdkEventButton* event);
		bool saveAsFileDialog(GdkEventButton* event);
		//bool on_AboutDialog_response(GdkEventButton* event);
		//void andrew();
		int createRefXml(Glib::RefPtr<Gnome::Glade::Xml> refXml);
		void initiateWidgets(Glib::RefPtr<Gnome::Glade::Xml> refXml);
		
	public:
		UI();
		~UI();
		Gtk::Window* getMainWindow();
		void run();
};

#endif /* UI_H_ */
