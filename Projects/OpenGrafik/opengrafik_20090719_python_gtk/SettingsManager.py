# API:
# getSettingsManager()
# SettingsManager.setValue(name, value)
# SettingsManager.getValue(name)
# SettingsManager.registerListener(callbackFunction)

# FIXME:
# 1. Use dictionary or hash table instead of list
# DONE 2. Get rid of pyinotify?

import os

settingsManager = None

class SettingsManager:
	def __init__(self):
		self.settingsFilename = os.path.join(
								os.path.expanduser('~'), '.opengrafik')
		
		self.checkSettingsFile()

		self.listeners = []
		self.config = self.loadSettings()

	def settingsFileModified(self):
		# self.config = self.loadSettings()
		for l in self.listeners:
			l()

	def checkSettingsFile(self):
		if os.path.exists(self.settingsFilename) != True:
			f = open(self.settingsFilename, "w")
			f.close()

	def registerListener(self, callback):
		self.listeners.append(callback)

	def setValue(self, name, value):
		assert(name != None)
		assert(value != None)

		found = False

		for c in self.config:
			if c[0] == name:
				c[1] = value
				found = True
				break

		if found != True:
			self.config.append([name, value])

		self.saveSettings()
		self.settingsFileModified()
	
	def getValue(self, name):
		assert(name != None)

		for c in self.config:
			if c[0] == name:
				return c[1]
		raise Exception()

	def saveSettings(self):
		data = ""

		for c in self.config:
			if type(c) == type([]):
				data = data + c[0] + "=" + c[1] + "\n"

		f = open(self.settingsFilename, "w")
		f.writelines(data)
		f.close()

	def loadSettings(self):
		f = open(self.settingsFilename, "r")
		data = f.readlines()
		f.close()

		config = []

		for d in data:
			if d != "":
				config.append(d.split("=", 1))
		return config

def getSettingsManager():
	global settingsManager
	if settingsManager == None:
		settingsManager = SettingsManager()
	return settingsManager
