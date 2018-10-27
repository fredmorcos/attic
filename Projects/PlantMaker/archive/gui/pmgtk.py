import gtk
from xml.dom import minidom

def strToBool(s):
	b_dict = {'true': True, 'false': False}
	return b_dict[s.lower()]

class AddOrderDialog(gtk.Dialog):
	def __init__(self, parent, machines):
		gtk.Dialog.__init__(self, "Add Order", parent, 
			gtk.DIALOG_MODAL | gtk.DIALOG_DESTROY_WITH_PARENT,
			(gtk.STOCK_CANCEL, gtk.RESPONSE_REJECT,
			 gtk.STOCK_ADD, gtk.RESPONSE_ACCEPT))

		self.machines = machines

 		id_label = gtk.Label("ID")
 		self.id_spin = gtk.SpinButton(gtk.Adjustment(1, 1, 999, 5))
 
		name_label = gtk.Label("Name")
		self.name_entry = gtk.Entry()

		deadline_label = gtk.Label("Deadline")
		self.deadline_spin = gtk.SpinButton(gtk.Adjustment(1, 1, 999, 5))

		recipe_label = gtk.Label("<b>Recipe</b>")
		recipe_label.set_use_markup(True)
		recipe_table = gtk.Table(len(machines), 2)
		recipe_table.set_row_spacings(5)
		recipe_table.set_col_spacings(5)

		recipe_labels = []
		self.recipe_spins = []
		for m in machines:
			recipe_labels.append(gtk.Label(m))
			self.recipe_spins.append(gtk.SpinButton(gtk.Adjustment(1, 1, 999, 5)))

		for i, l in enumerate(recipe_labels):
			recipe_table.attach(l, 0, 1, i, i + 1, gtk.SHRINK, gtk.SHRINK)
			recipe_table.attach(self.recipe_spins[i], 1, 2, i, i + 1,
				yoptions = gtk.SHRINK)

		table = gtk.Table(5, 2)
		table.set_row_spacings(5)
		table.set_col_spacings(5)
		table.attach(id_label, 0, 1, 0, 1, gtk.SHRINK, gtk.SHRINK)
		table.attach(self.id_spin, 1, 2, 0, 1, yoptions = gtk.SHRINK)
		table.attach(name_label, 0, 1, 1, 2, gtk.SHRINK, gtk.SHRINK)
		table.attach(self.name_entry, 1, 2, 1, 2, yoptions = gtk.SHRINK)
		table.attach(deadline_label, 0, 1, 2, 3, gtk.SHRINK, gtk.SHRINK)
		table.attach(self.deadline_spin, 1, 2, 2, 3, yoptions = gtk.SHRINK)
		table.attach(recipe_label, 0, 2, 3, 4, gtk.SHRINK, gtk.SHRINK)
		table.attach(recipe_table, 0, 2, 4, 5)

		self.get_content_area().pack_start(table)
		self.set_resizable(False)
		self.set_has_separator(False)
		self.set_border_width(5)
 		self.show_all()

class AddPlantItemDialog(gtk.Dialog):
	def __init__(self, parent):
		gtk.Dialog.__init__(self, "Add Plant Item", parent, 
			gtk.DIALOG_MODAL | gtk.DIALOG_DESTROY_WITH_PARENT,
			(gtk.STOCK_CANCEL, gtk.RESPONSE_REJECT,
			 gtk.STOCK_ADD, gtk.RESPONSE_ACCEPT))

 		type_label = gtk.Label("Type")
 		self.type_combo = gtk.combo_box_new_text()
 		self.type_combo.append_text("Machine")
 		self.type_combo.append_text("Buffer")
 		self.type_combo.append_text("Delay")
 		self.type_combo.set_active(0)
 
 		name_label = gtk.Label("Name")
 		self.name_entry = gtk.Entry()
 
 		quantity_label = gtk.Label("Quantity")
 		self.quantity_spin = gtk.SpinButton(gtk.Adjustment(1, 1, 999, 5))
 
 		delay_label = gtk.Label("Delay")
 		self.delay_spin = gtk.SpinButton(gtk.Adjustment(1, 1, 999, 5))

 		unhook_label = gtk.Label("Unhook")
		self.unhook_check = gtk.CheckButton()

		table = gtk.Table(5, 2)
		table.set_row_spacings(5)
		table.set_col_spacings(5)
		table.attach(type_label, 0, 1, 0, 1, gtk.SHRINK, gtk.SHRINK)
		table.attach(self.type_combo, 1, 2, 0, 1, yoptions = gtk.SHRINK)
		table.attach(name_label, 0, 1, 1, 2, gtk.SHRINK, gtk.SHRINK)
		table.attach(self.name_entry, 1, 2, 1, 2, yoptions = gtk.SHRINK)
		table.attach(quantity_label, 0, 1, 2, 3, gtk.SHRINK, gtk.SHRINK)
		table.attach(self.quantity_spin, 1, 2, 2, 3, yoptions = gtk.SHRINK)
		table.attach(delay_label, 0, 1, 3, 4, gtk.SHRINK, gtk.SHRINK)
		table.attach(self.delay_spin, 1, 2, 3, 4, yoptions = gtk.SHRINK)
		table.attach(unhook_label, 0, 1, 4, 5, gtk.SHRINK, gtk.SHRINK)
		table.attach(self.unhook_check, 1, 2, 4, 5, yoptions = gtk.SHRINK)
	 	
		self.get_content_area().pack_start(table)
		self.set_resizable(False)
 		self.set_has_separator(False)
		self.set_border_width(5)
 		self.show_all()

class MachineList(gtk.TreeView):
	def __init__(self):
		gtk.TreeView.__init__(self)
		self.list = gtk.ListStore(str, str, int, int, bool)

		type_col = gtk.TreeViewColumn("Type")
		name_col = gtk.TreeViewColumn("Name")
		name_col.set_expand(True)
		quantity_col = gtk.TreeViewColumn("Quantity")
		delay_col = gtk.TreeViewColumn("Delay")
		unhook_col = gtk.TreeViewColumn("Unhook")

		self.append_column(type_col)
		self.append_column(name_col)
		self.append_column(quantity_col)
		self.append_column(delay_col)
		self.append_column(unhook_col)

		type_cell = gtk.CellRendererText()
		type_col.pack_start(type_cell, False)
		type_col.add_attribute(type_cell, "text", 0)

		name_cell = gtk.CellRendererText()
		name_col.pack_start(name_cell, False)
		name_col.add_attribute(name_cell, "text", 1)

		quantity_cell = gtk.CellRendererText()
		quantity_col.pack_start(quantity_cell, False)
		quantity_col.add_attribute(quantity_cell, "text", 2)

		delay_cell = gtk.CellRendererText()
		delay_col.pack_start(delay_cell, False)
		delay_col.add_attribute(delay_cell, "text", 3)

		unhook_cell = gtk.CellRendererToggle()
		unhook_col.pack_start(unhook_cell, False)
		unhook_col.add_attribute(unhook_cell, "active", 4)

		self.set_search_column(1)
		self.set_reorderable(True)
		self.set_model(self.list)

		self.filename = None
		self.changed = False

	def get_items_of_type(self, name):
		res = []

		for r in self.list:
			if r[0] == "Machine":
				res.append(r[1])

		return res

	def save_to_file(self, filename):
		dom_imp = minidom.getDOMImplementation()
		xml_doc = dom_imp.createDocument(None, "plant", None)
		machine_node = None

		print self.list

		for m in self.list:
			machine_node = xml_doc.createElement("machine")
			machine_node.setAttribute("type", m[0])
			machine_node.setAttribute("name", m[1])
			machine_node.setAttribute("quantity", str(m[2]))
			machine_node.setAttribute("delay", str(m[3]))
			machine_node.setAttribute("unhook", str(m[4]))
			xml_doc.documentElement.appendChild(machine_node)

		file_data = xml_doc.documentElement.toprettyxml()
		file = open(filename, "w")
		file.write(file_data)
		file.close()

		self.changed = False

	def load_from_file(self, filename):
		self.clear()
		file = open(filename, "r")

		dom = minidom.parse(file)
		for c in dom.getElementsByTagName("machine"):
			self.list.append([
				c.getAttribute("type"),
				c.getAttribute("name"),
				int(c.getAttribute("quantity")),
				int(c.getAttribute("delay")),
				strToBool(c.getAttribute("unhook"))])

		file.close()

	def clear(self):
		self.filename = None
		self.changed = False
		self.list.clear()

class OrderList(gtk.TreeView):
	def __init__(self):
		gtk.TreeView.__init__(self)
		self.list = gtk.ListStore(int, str, int)
		self.recipe_list = []
		
		id_col = gtk.TreeViewColumn("ID")
		name_col = gtk.TreeViewColumn("Name")
		name_col.set_expand(True)
		deadline_col = gtk.TreeViewColumn("Deadline")

		self.append_column(id_col)
		self.append_column(name_col)
		self.append_column(deadline_col)
	
		id_cell = gtk.CellRendererText()
		id_col.pack_start(id_cell, False)
		id_col.add_attribute(id_cell, "text", 0)

		name_cell = gtk.CellRendererText()
		name_col.pack_start(name_cell, False)
		name_col.add_attribute(name_cell, "text", 1)

		deadline_cell = gtk.CellRendererText()
		deadline_col.pack_start(deadline_cell, False)
		deadline_col.add_attribute(deadline_cell, "text", 2)

		self.set_search_column(1)
		self.set_reorderable(True)
		self.set_model(self.list)

		self.filename = None
		self.changed = False

	def save_to_file(self, filename):
		print filename

	def load_from_file(self, filename):
		self.clear()
		print filename

	def clear(self):
		self.filename = None
		self.changed = False
		self.list.clear()
		for i in self.recipe_list:
			del i

class Callbacks():
	def check_save_order_list(self):
		if orderList.changed == True:
			dialog = gtk.MessageDialog(mainWindow,
					gtk.DIALOG_MODAL | gtk.DIALOG_DESTROY_WITH_PARENT,
					gtk.MESSAGE_QUESTION, gtk.BUTTONS_YES_NO,
					"Would you like to save this order list?")

			if dialog.run() == gtk.RESPONSE_YES:
				self.on_oSaveButton_clicked(button)
			dialog.destroy()

	def check_save_plant(self):
		if machineList.changed == True:
			dialog = gtk.MessageDialog(mainWindow,
					gtk.DIALOG_MODAL | gtk.DIALOG_DESTROY_WITH_PARENT,
					gtk.MESSAGE_QUESTION, gtk.BUTTONS_YES_NO,
					"Would you like to save this plant?")

			if dialog.run() == gtk.RESPONSE_YES:
				self.on_saveButton_clicked(button)
			dialog.destroy()

	def on_newButton_clicked(self, button):
		self.check_save_plant()
		machineList.clear()

	def on_openButton_clicked(self, button):
		self.check_save_plant()

		dialog = gtk.FileChooserDialog("Save Plant", mainWindow,
			gtk.FILE_CHOOSER_ACTION_OPEN,
			(gtk.STOCK_CANCEL, gtk.RESPONSE_REJECT,
			 gtk.STOCK_OPEN, gtk.RESPONSE_ACCEPT))
			
		if dialog.run() == gtk.RESPONSE_ACCEPT:
			machineList.load_from_file(dialog.get_filename())
		dialog.destroy()

	def on_saveButton_clicked(self, button):
		if machineList.filename == None:
			dialog = gtk.FileChooserDialog("Save Plant", mainWindow,
				gtk.FILE_CHOOSER_ACTION_SAVE,
				(gtk.STOCK_CANCEL, gtk.RESPONSE_REJECT,
				 gtk.STOCK_SAVE, gtk.RESPONSE_ACCEPT))
			
			if dialog.run() == gtk.RESPONSE_ACCEPT:
				machineList.save_to_file(dialog.get_filename())
			dialog.destroy()
		else:
			if machineList.changed == True:
				machineList.save_to_file(machineList.filename)

	def on_addButton_clicked(self, button):
		dialog = AddPlantItemDialog(mainWindow)
 		if dialog.run() == gtk.RESPONSE_ACCEPT:
 			machineList.list.append([
				dialog.type_combo.get_active_text(),
 				dialog.name_entry.get_text(),
				dialog.quantity_spin.get_value_as_int(),
 				dialog.delay_spin.get_value_as_int(),
				dialog.unhook_check.get_active()])
			machineList.changed = True
 		dialog.destroy()

	def on_removeButton_clicked(self, button):
 		(tm, ti) = machineList.get_selection().get_selected()
 		if ti != None:
 			machineList.list.remove(ti)
			machineList.changed = True

	def on_oNewButton_clicked(self, button):
		self.check_save_order_list()
		orderList.clear()

	def on_oOpenButton_clicked(self, button):
		pass

	def on_oSaveButton_clicked(self, button):
		pass

	def on_oAddButton_clicked(self, button):
		dialog = AddOrderDialog(mainWindow,
			machineList.get_items_of_type("Machine"))
		if dialog.run() == gtk.RESPONSE_ACCEPT:
			orderList.list.append([
				dialog.id_spin.get_value_as_int(),
				dialog.name_entry.get_text(),
				dialog.deadline_spin.get_value_as_int()])
			dict = {}
			for i, s in enumerate(dialog.recipe_spins):
				dict[dialog.machines[i]] = s.get_value_as_int()
			orderList.recipe_list.append(dict)
			orderList.changed = True
		dialog.destroy()

	def on_oRemoveButton_clicked(self, button):
 		(tm, ti) = orderList.get_selection().get_selected()
 		if ti != None:
 			orderList.list.remove(ti)
			orderList.changed = True

	def on_mainWindow_delete_event(self, widget, event):
		self.check_save_plant()
		gtk.main_quit()

gladeFile = "gtkui.glade"
callbacks = Callbacks()
builder = gtk.Builder()
builder.add_from_file(gladeFile)
builder.connect_signals(callbacks)

plantBox = builder.get_object("plantBox")
machineList = MachineList()
plantBox.pack_start(machineList)

orderBox = builder.get_object("orderBox")
orderList = OrderList()
orderBox.pack_start(orderList)

mainWindow = builder.get_object("mainWindow")
mainWindow.show_all()

