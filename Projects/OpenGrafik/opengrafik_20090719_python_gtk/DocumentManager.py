#!/usr/bin/python

from Document import Document

documentManager = None

class DocumentManager :
	def __init__(self):
		self.documents = []
	
	def closeFile(self, docID):
		pass
	
#	def loadFile(self):
#		pass
	
	def saveFile(self, docID):
		for i in self.documents:
			if i.docID == docID:
				if i.data == None:
					return False
				else:
					i.writeData()
	
	def saveFileAs(self, docID, data):
		for i in self.documents:
			if i.docID == docID:
				i.data = data
				i.writeData()
				break

def getDocumentManager():
	global documentManager
	if documentManager == None:
		documentManager = DocumentManager()
	return documentManager
