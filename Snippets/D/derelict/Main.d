module Main;

import derelict.opengl.gl;
import tango.io.Stdout;

void main()
{
	try {
		DerelictGL.load();
		Stdout("Successfully loaded the OpenGL shared library.").newline;
	} catch (Exception e) {
		Stdout("Could not load the OpenGL shared library.").newline;
	}
}

