import os, sys

def get_path():
	return os.path.dirname(os.path.abspath(root))

def start():
	print "start()"
	print "current path =", get_path()

