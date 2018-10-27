#!/usr/bin/python

import sys
import gtk
import DocumentManager
from Document import Document

class Window:
	def __init__(self):
		self.builder = gtk.Builder()
		self.builder.add_from_file("ui.glade")
		self.documentManager = DocumentManager.getDocumentManager()

		signals = { "on_MainWindow_destroy"				: gtk.main_quit,
					"on_NewDiagramMenuBar_activate"		: self.newDiagram,
					"on_QuitMenuItem_activate"			: gtk.main_quit,
					"on_PreferencesMenuItem_activate"	: self.showPreferences,
					"on_OpenMenuItem_activate"			: self.openDiagram,
					"on_SaveMenuItem_activate"			: self.saveDiagram,
					"on_SaveAsMenuItem_activate"		: self.saveDiagramAs
				   }
		self.builder.connect_signals(signals)

		self.window = self.builder.get_object("mainWindow")
		self.tabsPanel = self.builder.get_object("tabsPanel")

		self.window.show_all()

	def newDiagram(self, widget, data = None):
		newDocument = Document(self.tabsPanel.get_current_page() + 1)
		scrollArea = gtk.ScrolledWindow()
		scrollArea.set_policy(gtk.POLICY_ALWAYS, gtk.POLICY_ALWAYS)
		scrollArea.add_with_viewport(newDocument)

		n = self.tabsPanel.append_page(scrollArea,
				gtk.Label("Diagram %d" % (self.tabsPanel.get_n_pages() + 1)))
		scrollArea.show_all()
		self.tabsPanel.set_current_page(n)

		self.documentManager.documents.append(newDocument)

	def openDiagram(self, widget, data = None):
		dialog = gtk.FileChooserDialog("Open Diagram..",
									   self.window,
        		                       gtk.FILE_CHOOSER_ACTION_OPEN,
        		                       (gtk.STOCK_CANCEL, gtk.RESPONSE_CANCEL,
        		                        gtk.STOCK_OPEN, gtk.RESPONSE_OK))
		dialog.set_default_response(gtk.RESPONSE_OK)
		filter = gtk.FileFilter()
		filter.set_name("All files")
		filter.add_pattern("*")
		dialog.add_filter(filter)

		if dialog.run() == gtk.RESPONSE_OK:
			newDocument = Document(self.tabsPanel.get_current_page() + 1, 
								   dialog.get_filename())
			self.documentManager.documents.append(newDocument)
			# TODO
			# Load the document data onto the grid
		dialog.destroy()

	def showPreferences(self, widget, data = None):
		self.preferencesWindow = self.builder.get_object("preferencesWindow")
		self.preferencesWindow.show()

	def saveDiagram(self, widget, data = None):
		currentPage = self.tabsPanel.get_current_page()
		if self.documentManager.saveFile(currentPage) == False:
			self.saveDiagramAs(self)
				
	def saveDiagramAs(self, widget, data = None):
		dialog = gtk.FileChooserDialog("Save Diagram As..",
									   self.window,
									   gtk.FILE_CHOOSER_ACTION_SAVE,
									   (gtk.STOCK_CANCEL, gtk.RESPONSE_CANCEL,
									   gtk.STOCK_SAVE, gtk.RESPONSE_OK))
		dialog.set_default_response(gtk.RESPONSE_OK)
		filter = gtk.FileFilter()
		filter.set_name("All files")
		filter.add_pattern("*")
		dialog.add_filter(filter)

		if dialog.run() == gtk.RESPONSE_OK:
			self.documentManager.saveFileAs(self.tabsPanel.get_current_page(),
											dialog.get_filename())
		dialog.destroy()

win = Window()
gtk.main()

