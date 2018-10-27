from optparse import OptionParser

class ArgParser(OptionParser):
	def __init__(self):
		OptionParser.__init__(self)

		self.add_option('-o', type = 'string', dest = 'output_filename',
					metavar = 'FILE', help = 'output c file')
		self.add_option('-i', type = 'string', dest = 'input_filename',
					metavar = 'FILE', help = 'input object-c file')
		self.add_option('-v', action = 'store_true', default = False,
					dest = 'verbose', help = 'verbose output')

	def get_options(self):
		return self.parse_args()[0]
