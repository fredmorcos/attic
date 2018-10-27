////////////////////////////////////////////////////////////////////////////////
//3456789 123456789 123456789 123456789 123456789 123456789 123456789 123456789 
//
//  lowfat - an "engine" for natural document viewing for free desktop-systems
//
//  copyright (c) 2007 Mirco Müller
//
//  lowfat is free software; you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation; either version 2 of the License, or
//  (at your option) any later version.
//
//  lowfat is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with Foobar; if not, write to the Free Software
//  Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
//
////////////////////////////////////////////////////////////////////////////////

#include <pthread.h>
#include <stdlib.h>
#include <unistd.h>
#include <gtk/gtk.h>
#include <stdexcept>
#include <fstream>
#include <iostream>
#include "include/lowfat.h"

int
get_screen_width (int argc,
		  char** argv)
{
	GdkDisplay* p_defaultDisplay = NULL;
	GdkScreen* p_defaultScreen = NULL;
	gint i_screenWidth = 0;

	gtk_init (&argc, &argv);
	gdk_threads_enter ();

	p_defaultDisplay = gdk_display_get_default ();

	if (p_defaultDisplay)
		p_defaultScreen = gdk_display_get_default_screen (
					p_defaultDisplay);

	if (p_defaultScreen)
		i_screenWidth = gdk_screen_get_width (p_defaultScreen);

	gdk_threads_leave ();

	return i_screenWidth;
}

int
get_screen_height (int argc,
		   char** argv)
{
	GdkDisplay* p_defaultDisplay = NULL;
	GdkScreen* p_defaultScreen = NULL;
	gint i_screenHeight = 0;

	gtk_init (&argc, &argv);
	gdk_threads_enter ();

	p_defaultDisplay = gdk_display_get_default ();

	if (p_defaultDisplay)
		p_defaultScreen = gdk_display_get_default_screen (
					p_defaultDisplay);

	if (p_defaultScreen)
		i_screenHeight = gdk_screen_get_height (p_defaultScreen);

	gdk_threads_leave ();

	return i_screenHeight;
}

int
main (int argc,
      char** argv)
{
	try
	{
		int width = 800;
		int height = 600;

		if (argc == 3)
		{
			width = (int) atof (argv[1]);
			height = (int) atof (argv[2]);
		}
		else
		{
			printf ("You can pass width and height of the window like this:\n\n");
			printf ("\t%s <width in pixels> <height in pixels>\n\n",
				argv[0]);
			g_thread_init (NULL);
			width = get_screen_width (argc, argv);
			height = get_screen_height (argc, argv);
		}

		Lowfat app ("lowfat", width, height, false, argc, argv);
		app.run ();
	}
	catch (std::runtime_error& e)
	{
		std::cout << "an exception occured: " << e.what() << "\n";
	}

	return 0;
}

