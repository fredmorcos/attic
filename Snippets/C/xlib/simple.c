/*
	Simple Xlib application drawing a box in a window.
*/

#include<X11/Xlib.h>
#include<stdio.h>

int main() {
	Display *d;
	int s;
	Window w;
	XEvent e;

	/* open connection with the server */
	d=XOpenDisplay(NULL);
	if(d==NULL) {
		printf("Cannot open display\n");
		return 1;
	}
	s=DefaultScreen(d);

                        /* create window */
   w=XCreateSimpleWindow(d, RootWindow(d, s), 10, 10, 100, 100, 1,
                         BlackPixel(d, s), WhitePixel(d, s));

                        /* select kind of events we are interested in */
   XSelectInput(d, w, ExposureMask | KeyPressMask);

                        /* map (show) the window */
   XMapWindow(d, w);
                        /* event loop */
   XWindowAttributes window_info;
   int err = 0;
	while(1) {
		XNextEvent(d, &e);
		/* draw or redraw the window */
		if(e.type==Expose) {
			XGetWindowAttributes(d, w, &window_info);
			//printf("w=%d, h=%d\n", window_w, window_h);
			XFillRectangle(d, w, DefaultGC(d, s), 20, 20, window_info.width - 40, window_info.height - 40);
		}
                        /* exit on key press */
     if(e.type==KeyPress)
       break;
   }

                        /* destroy our window */
   XDestroyWindow(d, w);

                        /* close connection to server */
   XCloseDisplay(d);

   return 0;
 }
