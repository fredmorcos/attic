from distutils.core import setup

files = ["img/*"]

setup(
		  name = "Grafeo",
		  version = "0.1",
			description = "Graph and diagram editor",
			author = "Fred Morcos",
			author_email = "fred.morcos@gmail.com",
			url = "http://grafeo.googlecode.com/",
			packages = ["src"],
			package_data = {"src" : files},
			scripts = ["grafeo"],
			long_description = "Graph and diagram editor"
		 )

