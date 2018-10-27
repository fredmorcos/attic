solution "mand"
	configurations {
		"debug", 
		"release"
	}
	
	project "mand"
		language "C"
		kind "ConsoleApp"
		files {
			"*.h", 
			"*.c"
		}
		linkoptions {"-lpthread"}
		buildoptions {"-std=c99"}
		flags {
			"ExtraWarnings",
			"FatalWarnings"
		}

		newoption {
			trigger = "with-max-z",
			description = "Maximum Z value [default = 4]"
		}

		newoption {
			trigger = "with-max-iteration",
			description = "Maximum iterations [default = 1000]"
		}

		if not _OPTIONS["with-max-z"] then
			max_z = 4
		else
			max_z = _OPTIONS["with-max-z"]
		end

		if not _OPTIONS["with-max-iterations"] then
			max_iterations = 1000
		else
			max_iterations = _OPTIONS["with-max-iterations"]
		end

		defines {
			"MAX_Z=" .. max_z,
			"MAX_ITER=" .. max_iterations
		}

		configuration "debug"
			defines {"DEBUG"}
			flags {"Symbols"}

		configuration "release"
			defines {"NDEBUG"}
			flags {
				"OptimizeSize",
				"NoFramePointer",
				"FloatFast"
			}

