#!/usr/bin/python

from arg_parser import ArgParser
from loader import Loader

if __name__ == '__main__':
	arg_parser = ArgParser()
	options = arg_parser.get_options()

	loader = Loader(options)
	if loader.error:
		print loader.error_message
		exit(1)
