solution "reminder"
	configurations {
		"debug", 
		"release"
	}
	
	project "reminder"
		language "C"
		kind "ConsoleApp"
		files {
			"*.h", 
			"*.c"
		}
		buildoptions {"-std=c99"}
		flags {
			"ExtraWarnings",
			"FatalWarnings"
		}

		configuration "debug"
			defines {"DEBUG"}
			flags {"Symbols"}

		configuration "release"
			flags {
				"OptimizeSize",
				"NoFramePointer",
				"FloatFast"
			}

