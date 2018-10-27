class Loader:
	def __init__(self, options):
		self.options = options
		self.error = False
		self.error_message = ''
		self.input_file = None
		self.output_file = None

		if options.input_filename == '' or options.input_filename == None:
			self.error = True
			self.error_message = 'Please provide an input file'
			return

		if options.output_filename == '' or options.output_filename == None:
			self.error = True
			self.error_message = 'Please provide an output file'
			return

		if options.input_filename == options.output_filename:
			self.error = True
			self.error_message = 'Do not provide same input and output files'
			return

	def open_input(self):
		self.input_file = file(self.options.input_filename, 'r')
		return self.input_file

	def open_output(self):
		self.output_file = file(self.options.output_filename, 'w')
		return self.output_file

	def close_input(self):
		self.input_file.close()

	def close_output(self):
		self.output_file.close()
