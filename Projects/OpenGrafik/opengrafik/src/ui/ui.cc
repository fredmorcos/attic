#include "ui.h"
#include <iostream>
#include <sstream>

using namespace std;

UI::UI()
{
	createRefXml(refXml);
	getMainWindow()->maximize();
}
		
UI::~UI()
{
	delete newDocMenuItem, openDocMenuItem, saveDocMenuItem, 
			saveAsDocMenuItem, closeDocMenuItem, quitDocMenuItem, 
			mainNotebook, mainWindow;
}
		
Gtk::Window* UI::getMainWindow()
{
	return mainWindow;
}

int UI::createRefXml(Glib::RefPtr<Gnome::Glade::Xml> refXml)
{
	#ifdef GLIBMM_EXCEPTIONS_ENABLED
	try
	{
		refXml = Gnome::Glade::Xml::create("main-ui.glade");
	}
	catch(const Gnome::Glade::XmlError& ex)
	{
		std::cerr << ex.what() << std::endl;
		return 1;
	}
	#else
	std::auto_ptr<Gnome::Glade::XmlError> error;
	refXml = Gnome::Glade::Xml::create("main-ui.glade", "", "", error);
	if(error.get())
	{
		std::cerr << error->what() << std::endl;
		return 1;
	}
	#endif	/*GLIBMM_EXCEPTIONS_ENABLED*/
	
	initiateWidgets(refXml);
}

/*void on_AboutDialog_response(GtkDialog *dialog, gpointer data)
{
	//gtk_widget_hide(GTK_WIDGET(dialog));
	//return false;
	cout << "slfdj" << endl;
}

void andrew()
{
	cout << "andrew" << endl;
}*/

bool UI::docQuit(GdkEventButton* event)
{
	Gtk::Main::quit();
	return false;
}

bool UI::docNew(GdkEventButton* event)
{
	std::ostringstream s;
	/* needs to be fixed with the number of documents open, not the number of tabs, because, as an 
	 * example, the "Welcome" tab doesn't count as a document, maybe we can also use the help to 
	 * come out in a new tab...
	 */
	s << "New Document " << mainNotebook->get_n_pages() + 1;
	Glib::ustring us = s.str();
	mainNotebook->append_page(*(manage(new Gtk::Label(""))), us);//need
										// to replace with document_UI
	mainNotebook->show_all_children();
	return false;
}

bool UI::docClose(GdkEventButton* event)
{
	mainNotebook->remove_page(mainNotebook->get_current_page());
	return false;
}

bool UI::aboutShow(GdkEventButton* event)
{
	aboutDialog->show();
	return false;
}

bool UI::openFileDialog(GdkEventButton* event)
{
	Gtk::FileChooserDialog dialog("Please choose a file",
									Gtk::FILE_CHOOSER_ACTION_OPEN);
	dialog.set_transient_for(*mainWindow);

	dialog.add_button(Gtk::Stock::CANCEL, Gtk::RESPONSE_CANCEL);
	dialog.add_button(Gtk::Stock::OPEN, Gtk::RESPONSE_OK);

	Gtk::FileFilter filter_all;
	filter_all.set_name("All Files");
	filter_all.add_pattern("*");
	dialog.add_filter(filter_all);

	Gtk::FileFilter filter_og;
	filter_og.set_name("*.og");
	filter_og.add_pattern("*.og");
	dialog.add_filter(filter_og);

	Gtk::FileFilter filter_svg;
	filter_svg.set_name("*.svg");
	filter_svg.add_pattern("*.svg");
	dialog.add_filter(filter_svg);

	Gtk::FileFilter filter_png;
	filter_png.set_name("*.png");
	filter_png.add_pattern("*.png");
	dialog.add_filter(filter_png);

	int result = dialog.run();

	switch(result)
	{
		case(Gtk::RESPONSE_OK):
		{
			std::string filename = dialog.get_filename();
			std::cout << "File Path to b opened: " <<  filename 
														<< std::endl;
			break;
		}
		case(Gtk::RESPONSE_CANCEL):
		{
			break;
		}
		default:
		{
			break;
		}
	}
  
	return false;
}

bool UI::saveAsFileDialog(GdkEventButton* event)
{
	Gtk::FileChooserDialog dialog("Save File",
									Gtk::FILE_CHOOSER_ACTION_OPEN);
	dialog.set_transient_for(*mainWindow);

	dialog.add_button(Gtk::Stock::CANCEL, Gtk::RESPONSE_CANCEL);
	dialog.add_button(Gtk::Stock::SAVE_AS, Gtk::RESPONSE_OK);

	int result = dialog.run();

	switch(result)
	{
		case(Gtk::RESPONSE_OK):
		{
			std::string filename = dialog.get_filename();
			std::cout << "File Path to b saved: " <<  filename 
														<< std::endl;
			break;
		}
		case(Gtk::RESPONSE_CANCEL):
		{
			break;
		}
		default:
		{
			break;
		}
	}
  
	return false;
}

void UI::initiateWidgets(Glib::RefPtr<Gnome::Glade::Xml> refXml)
{
	refXml->get_widget("MainWindow", mainWindow);
	
	refXml->get_widget("QuitMenuItem", quitDocMenuItem);
	quitDocMenuItem->signal_button_release_event().connect(
									sigc::mem_fun(this, &UI::docQuit));
	
	refXml->get_widget("MainNotebook", mainNotebook);
	
	refXml->get_widget("NewMenuItem", newDocMenuItem);
	newDocMenuItem->signal_button_release_event().connect(
									sigc::mem_fun(this, &UI::docNew));
	
	refXml->get_widget("CloseMenuItem", closeDocMenuItem);
	closeDocMenuItem->signal_button_release_event().connect(
								sigc::mem_fun(this, &UI::docClose));
	
	refXml->get_widget("AboutDialog", aboutDialog);
	/*aboutDialog->signal_button_release_event().connect(
								sigc::mem_fun(this, &UI::aboutClose));*/
	
	refXml->get_widget("AboutMenuItem", aboutMenuItem);
	aboutMenuItem->signal_button_release_event().connect(
								sigc::mem_fun(this, &UI::aboutShow));
		
	refXml->get_widget("OpenMenuItem", openDocMenuItem);
	openDocMenuItem->signal_button_release_event().connect(
							sigc::mem_fun(this, &UI::openFileDialog));
	
	refXml->get_widget("SaveAsMenuItem", saveAsDocMenuItem);
	saveAsDocMenuItem->signal_button_release_event().connect(
							sigc::mem_fun(this, &UI::saveAsFileDialog));
}

void UI::run()
{
	mainWindow->show();
	mainWindow->show_all_children();
}
