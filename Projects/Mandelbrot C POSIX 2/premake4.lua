solution "mandelbrot"
	configurations {"debug", "release"}
	
	project "mandelbrot"
		language "C"
		kind "ConsoleApp"
		files {"*.h", "*.c"}
		buildoptions {"-std=c99"}
		flags {
			"ExtraWarnings",
			"FatalWarnings"
		}

		newoption {
			trigger = "with-output-image",
			description = "Enable output to image [default = no]"
		}

		newoption {
			trigger = "with-image-width",
			description = "Output image width [default = 800]"
		}

		newoption {
			trigger = "with-image-height",
			description = "Output image height [default = 600]"
		}

		newoption {
			trigger = "with-max-z",
			description = "Maximum Z value [default = 4]"
		}

		newoption {
			trigger = "with-max-iteration",
			description = "Maximum iterations [default = 1000]"
		}

		newoption {
			trigger = "with-benchmark",
			description = "Run in benchmark mode [default = no]"
		}

		if not _OPTIONS["with-image-width"] then
			image_width = 800
		else
			image_width = _OPTIONS["with-image-width"]
		end

		if not _OPTIONS["with-image-height"] then
			image_height = 600
		else
			image_height = _OPTIONS["with-image-height"]
		end

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
			"IMAGE_WIDTH=" .. image_width,
			"IMAGE_HEIGHT=" .. image_height,
			"MAX_Z=" .. max_z,
			"MAX_ITER=" .. max_iterations
		}

		configuration "with-output-image"
			linkoptions {"`pkg-config --libs cairo-png`"}
			buildoptions {"`pkg-config --cflags cairo-png`"}
			defines {"OUTPUT_IMAGE"}

		configuration "with-benchmark"
			defines {"BENCHMARK"}

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

