solution "grafeo"
	configurations {
		"debug", 
		"release"
	}

	objdir "build/obj"
	buildoptions {"-std=c99"}

	flags {
		"ExtraWarnings",
		"FatalWarnings"
	}

	newoption {
		trigger = "prefix",
		value = "PATH",
		description = "Installation directory [default = /usr/local]"
	}

	if not _OPTIONS["prefix"] then
		install_dir = "/usr/local"
	else
		install_dir = _OPTIONS["prefix"]
	end

	newaction {
		trigger     = "install",
		description = "Install Grafeo",
		execute = function ()
			os.mkdir(install_dir .. "/bin")
			os.mkdir(install_dir .. "/lib")
			os.mkdir(install_dir .. "/include")
			os.mkdir(install_dir .. "/include/graph")
			os.copyfile("graph/*.h", install_dir .. "/include/graph/.")
			os.copyfile("build/lib/*", install_dir .. "/lib/.")
		end
	}

	newaction {
		trigger     = "uninstall",
		description = "Uninstall Grafeo",
		execute = function ()
			os.remove(install_dir .. "/bin/grafeo")
			os.remove(install_dir .. "/lib/libgraph.so")
			os.rmdir(install_dir .. "/include/graph")
		end
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

	project "grafeo"
		targetdir "build/bin"
		language "C"
		kind "WindowedApp"
		defines {"INSTALL_PATH=" .. install_dir .. "/bin"}
		links {"graph"}
		includedirs {"."}
		files {
			"gtk/*.h",
			"gtk/*.c"
		}
	
	project "graph"
		targetdir "build/lib"
		language "C"
		kind "SharedLib"
		defines {"INSTALL_PATH=" .. install_dir .. "/lib"}
		files {
			"graph/*.h", 
			"graph/*.c"
		}

	if _ACTION == "clean" then
		os.rmdir("build/lib")
		os.rmdir("build/bin")
		os.rmdir("build")
	end

