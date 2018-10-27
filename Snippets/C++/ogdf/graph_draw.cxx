/*
This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
Version 2 or later as published by the Free Software Foundation
and appearing in the file COPYING included in the packaging
of this file.

In addition, as a special exception, you have permission to link
this software with

  - the libraries of the COIN-OR Osi project (see
    http://www.coin-or.org/projects/Osi.xml);

  - and all LP-solver libraries directly supported by the
    COIN-OR Osi project,

and distribute executables, as long as you follow the requirements
of the GNU General Public License in regard to all of the software
in the executable aside from these third-party libraries.
*/

   
#include <ogdf/basic/Graph.h>
#include <ogdf/basic/GraphAttributes.h>

#include <ogdf/basic/NodeArray.h>

#include <cairo.h>

#ifdef USE_X
#include <X11/Xlib.h>
#include <cairo-xlib.h>
#else
#include <cairo-svg.h>
#endif

#include <string>
#include <stdio.h>
#include <sys/time.h>

#ifndef PI
#define PI 3.14159265358979323846
#endif

typedef struct
{
  FILE *out;
} png_stream_to_file_closure_t;

static cairo_status_t
write_png_stream_to_file(void *in_closure, const unsigned char *data, unsigned int length)
{
  png_stream_to_file_closure_t *closure = (png_stream_to_file_closure_t *)in_closure;

  size_t wsize = fwrite((void *)data, length, 1, closure->out);


#if 0
  if (wsize != length)
    return CAIRO_STATUS_WRITE_ERROR;
#endif

  return CAIRO_STATUS_SUCCESS;
}

int keep_running = 1;

using namespace ogdf;

typedef struct display_device {
#ifdef USE_X
  Display *dpy;
  int screen;
  int depth;
  Visual *vis;
  Window root;
  unsigned long color;
#endif
} display_device_t;

typedef struct drawing_device {
  cairo_surface_t *surf;
  cairo_t *cr;
} drawing_device_t;

typedef struct window_device {
#ifdef USE_X
  Window win;
#endif
  int x, y;
  unsigned int width;
  unsigned int height;
} window_device_t;


int setColor(drawing_device_t *dev, String color);

int ellipseLineIntersect(DPoint p1, DPoint p2,
			 DPoint center,double xRadius,double yRadius,
			 DPoint *intersect1, DPoint *intersect2)
{
  DPoint v;
  double a,b,c,d;
  double t1,t2;
  DPoint q;

  // Scale and transform everything so we're intersecting with a unit circle centered on (0,0).
  p1.m_x=(p1.m_x-center.m_x)/xRadius;
  p1.m_y=(p1.m_y-center.m_y)/yRadius;
  p2.m_x=(p2.m_x-center.m_x)/xRadius;
  p2.m_y=(p2.m_y-center.m_y)/yRadius;

  // Parametrize the line as p1 + t*v.
  v.m_x=p2.m_x-p1.m_x;
  v.m_y=p2.m_y-p1.m_y;

  /*
    Now we want to find the points on the line p1+t*v that satisfy x^2 + y^2 = 1.

    We have (x,y)=p1+t*v, so:
  
    x^2+y^2=1
    => (p1.x+t*v.x)^2 + (p1.y+t*v.y)^2 = 1
    => t^2*(v.x^2 + v.y^2) + 2*t*(p1.x*v.x + p1.y*v.y) + p1.x^2+p1.y^2 = 1

    Set a = v.x^2+v.y^2, b=2*(p1.x*v.x+p1.y*v.y), c=p1.x^2+p1.y^2-1, and we have:

    a*t^2 + b*t + c = 0

    with standard solution:

    t = (-b +/- sqrt(b^2-4*a*c))/2a
  */
  a=v.m_x*v.m_x+v.m_y*v.m_y;
  b=2*(p1.m_x*v.m_x+p1.m_y*v.m_y);
  c=p1.m_x*p1.m_x+p1.m_y*p1.m_y-1;

  // Degenerate line.
  if (!a)
    return 0;

  // Set d=b^2-4*a*c to shorten things.
  d=b*b-4*a*c;

  // Line does not intersect the ellipse.
  if (d<0)
    return 0;

  // First intersection.
  t1=(-b-sqrt(d))/2/a;
  q.m_x=p1.m_x+t1*v.m_x;
  q.m_y=p1.m_y+t1*v.m_y;
  intersect1->m_x = q.m_x;
  intersect1->m_y = q.m_y;

  // If d was 0, the line intersects the ellipse in only one point.
  if (d==0)
    return 1;

  // Otherwise, find the second point, too.
  t2=(-b+sqrt(d))/2/a;
  q.m_x=p1.m_x+t2*v.m_x;
  q.m_y=p1.m_y+t2*v.m_y;
  
  intersect2->m_x = q.m_x;
  intersect2->m_y = q.m_y;

  return 2;
}

void create_display_device(display_device_t *dev)
{
#ifdef USE_X
  dev->dpy = XOpenDisplay(NULL);
  dev->screen = DefaultScreen(dev->dpy);
  dev->depth = DefaultDepth(dev->dpy, dev->screen);
  dev->vis = DefaultVisual(dev->dpy, dev->screen);
  dev->root = RootWindow(dev->dpy, dev->screen);
  dev->color = WhitePixel(dev->dpy, dev->screen);
#endif
}

void create_window_device(display_device_t *dispDev, window_device_t *dev, DRect bbox)
{
  dev->x = dev->y = 0;
  dev->height = (unsigned int)(fabs(bbox.p2().m_y) + fabs(bbox.p1().m_y) + 20);
  dev->width = (unsigned int)(fabs(bbox.p2().m_x) + fabs(bbox.p1().m_x) + 20);
  
  if (!(dev->width && dev->height))
    {
      printf("invalid window size\n");
      exit(1);
    }
#ifdef USE_X
  dev->win = XCreateSimpleWindow(dispDev->dpy, dispDev->root, dev->x, dev->y, dev->width, dev->height, 0, dispDev->color, dispDev->color);
#endif
}

void create_drawing_device(display_device_t *dispDev, window_device_t *winDev, drawing_device_t *draw)
{
#ifdef USE_X
  draw->surf = cairo_xlib_surface_create(dispDev->dpy, winDev->win, dispDev->vis,
					 winDev->width,
					 winDev->height);
#else
  draw->surf = cairo_svg_surface_create("foo.svg", winDev->width, winDev->height);
#endif

  draw->cr = cairo_create(draw->surf);
 
  if (cairo_surface_status(draw->surf) != CAIRO_STATUS_SUCCESS)
    printf("error creating surface: %s\n", cairo_status_to_string(cairo_surface_status(draw->surf)));

}

void window_setup(display_device_t *disp, window_device_t *dev)
{
#ifdef USE_X
  long mask = StructureNotifyMask | ExposureMask | KeyPressMask |
    KeyReleaseMask | ButtonPressMask | ButtonReleaseMask;
  XSelectInput(disp->dpy, dev->win, mask);      
#endif
}

void stop()
{
  keep_running = 0;
}

int shouldRun()
{
  return keep_running;
}

void draw_background(window_device_t *winDev, drawing_device_t *dev)
{
  cairo_set_source_rgb(dev->cr, 1.0, 1.0, 1.0);
  cairo_paint(dev->cr);
}

void draw_arrow(drawing_device_t *draw, DLine line)
{
  DPoint pt;

  double angle = (23 * PI/180);
  double arrow_len = 10.0;
  double dx = line.dx();
  double dy = line.dy();
  double x1, y1, x2, y2;
  
  pt = line.start();

  x1 = pt.m_x + (dx * cos(angle) + dy * sin(angle)) * arrow_len/line.length();
  y1 = pt.m_y + (dy * cos(angle) - dx * sin(angle)) * arrow_len/line.length();

  x2 = pt.m_x + (dx * cos(-angle) + dy * sin(-angle)) * arrow_len/line.length();
  y2 = pt.m_y + (dy * cos(-angle) - dx * sin(-angle)) * arrow_len/line.length();

  cairo_move_to(draw->cr, pt.m_x, pt.m_y);
  cairo_line_to(draw->cr, x1, y1);
  cairo_line_to(draw->cr, x2, y2);
  cairo_close_path(draw->cr);



  cairo_set_dash(draw->cr, NULL, 0, 0.0);
  cairo_stroke_preserve(draw->cr);
  cairo_fill(draw->cr);

#ifdef USE_X
#ifdef DEBUG 
  cairo_surface_flush(draw->surf);
  XFlush(cairo_xlib_surface_get_display(draw->surf));
#endif
#endif
}


void set_line_stipple(drawing_device_t *dev, GraphAttributes &ga, edge edg)
{
  long attribs = ga.attributes();

  /* set up edge styles */
  if (attribs & GraphAttributes::edgeStyle)
    {
      GraphAttributes::EdgeStyle &es = ga.styleEdge(edg);

      switch (es)
	{
	case GraphAttributes::esSolid:
	  cairo_set_dash(dev->cr, NULL, 0, 0.0);
	  break;
	case GraphAttributes::esDash:
	  {
	    const double dashes[] =  { 10.0, 10.0 };
	    int ndashes = sizeof(dashes)/sizeof(dashes[0]);

	    cairo_set_dash(dev->cr, dashes, ndashes, 0.0);
	  }
	  break;
	case GraphAttributes::esDot:
	  {
	    const double dashes[] =  { 2.0, 5.0 };
	    int ndashes = sizeof(dashes)/sizeof(dashes[0]);
		
	    cairo_set_dash(dev->cr, dashes, ndashes, 0.0);
	  }
	  break;
	case GraphAttributes::esDashdot:
	  {
	    const double dashes[] = { 10.0, 5.0, 2.0, 5.0 };
	    int ndashes = sizeof(dashes)/sizeof(dashes[0]);

	    cairo_set_dash(dev->cr, dashes, ndashes, 0.0);
	  }
	  break;
	case GraphAttributes::esDashdotdot:
	  {
	    const double dashes[] = { 10.0, 5.0, 2.0, 5.0, 2.0, 5.0 };
	    int ndashes = sizeof(dashes)/sizeof(dashes[0]);

	    cairo_set_dash(dev->cr, dashes, ndashes, 0.0);
	  }
	  break;
	case GraphAttributes::esNoPen:
	default:
	  break;
	}
    }
}


/* ok ok theres no reason for this function to be so ugly */
void draw_arrows(drawing_device_t *dev, GraphAttributes &ga, edge &edg)
{
  DPolyline &bends = ga.bends(edg);
  DPoint linept1, linept2, n1center, n2center;
  DPoint is1, is2;
  String edgeColor = ga.colorEdge(edg);
  GraphAttributes::EdgeArrow arrow = GraphAttributes::none;
  long attribs = ga.attributes();
  DLine line1, line2, line3, line4, minLine;
  int pc1, pc2;
  DPoint startPoint;
  DPoint startPoint2;
  DPoint endPoint;
  DPoint endPoint2;
 
  int n1shape = ga.shapeNode(edg->source());
  int n2shape = ga.shapeNode(edg->target());
  DRect sourceRect, targetRect;
  
  endPoint2 = DPoint(-1.0, -1.0);
  startPoint2 = DPoint(-1.0, -1.0);
  setColor(dev, ga.colorEdge(edg));
  
  if (attribs & GraphAttributes::edgeArrow)
    {
      arrow = ga.arrowEdge(edg);
    }

  n1center.m_x = ga.x(edg->source());
  n1center.m_y = ga.y(edg->source());
  n2center.m_x = ga.x(edg->target());
  n2center.m_y = ga.y(edg->target());
     
  sourceRect = DRect(DPoint(n1center.m_x - (ga.width(edg->source()) / 2.0),
                            n1center.m_y - (ga.height(edg->source()) / 2.0)),
                     DPoint(n1center.m_x + (ga.width(edg->source()) / 2.0),
                            n1center.m_y + (ga.height(edg->source()) / 2.0)));
  targetRect = DRect(DPoint(n2center.m_x - (ga.width(edg->target()) / 2.0),
                            n2center.m_y - (ga.height(edg->target()) / 2.0)),
                     DPoint(n2center.m_x + (ga.width(edg->target()) / 2.0),
                            n2center.m_y + (ga.height(edg->target()) / 2.0)));


  ListConstIterator<DPoint> it = bends.begin();

  if (it == bends.end())
    {
      /* need 2 elements to make a line, just use
       * the source/targets x and y */
      linept1 = n1center;
      linept2 = n2center;
    }
  else
    {
      linept1 = *it;
      if (linept1 != n1center)
        {
          /* extend the line to connect to the shapes center */
	  linept2 = linept1;
	  linept1 = n1center;
	}
      else
        {
	  /* center to some point.. */
	  it++;
	  linept2 = *it;
	} 
    }
	
  if (arrow == GraphAttributes::first || arrow == GraphAttributes::both)
    {
      switch (n1shape)
	{
	case GraphAttributes::rectangle:
	  {
	    DLine line = DLine(linept1, linept2);
            if (sourceRect.contains(linept2))
              {
                pc1=1;
		startPoint = linept2;
              }
            else if (sourceRect.topLine().intersection(line, is1, true) ||
		     sourceRect.bottomLine().intersection(line, is1, true) ||
		     sourceRect.leftLine().intersection(line, is1, true) ||
		     sourceRect.rightLine().intersection(line, is1, true))
              {
		pc1=1;
	        startPoint = DPoint(is1.m_x, is1.m_y);
              }
            else
              {
		pc1=1;
	        startPoint = DPoint(linept1.m_x, linept1.m_y);
              }
	  }
	  break;
	case GraphAttributes::oval:
	  {

	    pc1 = ellipseLineIntersect(linept1, linept2, linept1, ga.width(edg->source()), ga.height(edg->source()),
				       &is1, &is2);
	    cairo_save(dev->cr);
	    cairo_translate(dev->cr, n1center.m_x, n1center.m_y);
	    cairo_scale (dev->cr, ga.width(edg->source()) / 2.0,  ga.height(edg->source()) / 2.0);

	    if (pc1 == 1)
	      {
		cairo_user_to_device(dev->cr, &is1.m_x, &is1.m_y);
		cairo_restore(dev->cr);
		cairo_device_to_user(dev->cr, &is1.m_x, &is1.m_y);
		startPoint = is1;
	      }
	    else if (pc1 == 2)
	      {	       
		cairo_user_to_device(dev->cr, &is1.m_x, &is1.m_y);
		cairo_user_to_device(dev->cr, &is2.m_x, &is2.m_y);
		cairo_restore(dev->cr);
		cairo_device_to_user(dev->cr, &is1.m_x, &is1.m_y);
		cairo_device_to_user(dev->cr, &is2.m_x, &is2.m_y);
		startPoint = is1;
		startPoint2 = is2;
	      }
	  }
	  break;
	}

      if (linept2 == n2center)
	{
	  switch (n2shape)
	    {
	    case GraphAttributes::rectangle:
	      {
                DLine line = DLine(linept1, linept2);
                if (targetRect.topLine().intersection(line, is1, true) ||
		    targetRect.bottomLine().intersection(line, is1, true) ||
		    targetRect.leftLine().intersection(line, is1, true) ||
		    targetRect.rightLine().intersection(line, is1, true))
		  {
		    pc2=1;
		    endPoint = DPoint(is1.m_x, is1.m_y);
		  }
		else
		  {
		    pc2=1;
		    endPoint = DPoint(linept2.m_x, linept2.m_y);
		  }
	      }
	      break;
	    case GraphAttributes::oval:
	      {
		pc2 = ellipseLineIntersect(linept1, linept2, linept1, ga.width(edg->target()), ga.height(edg->target()),
					   &is1, &is2);
		cairo_save(dev->cr);
		cairo_translate(dev->cr, n2center.m_x, n2center.m_y);
		cairo_scale (dev->cr, ga.width(edg->target()) / 2.0,  ga.height(edg->target()) / 2.0);

		if (pc2 == 1)
		  {
		    cairo_user_to_device(dev->cr, &is1.m_x, &is1.m_y);
		    cairo_restore(dev->cr);
		    cairo_device_to_user(dev->cr, &is1.m_x, &is1.m_y);
		    endPoint = is1;
		  }
		else if (pc2 == 2)
		  {
		    cairo_user_to_device(dev->cr, &is2.m_x, &is2.m_y);
		    cairo_user_to_device(dev->cr, &is1.m_x, &is1.m_y);
		    cairo_restore(dev->cr);
		    cairo_device_to_user(dev->cr, &is1.m_x, &is1.m_y);
		    cairo_device_to_user(dev->cr, &is2.m_x, &is2.m_y);
		    endPoint = is1;
		    endPoint2 = is2;
		  }
	      }
	      break;
	    }
        }
      else
        {
	  pc2 = 1;
	  endPoint = linept2;
        }

       
      line1 = DLine(startPoint, endPoint);
      minLine = line1;
      if (pc1 > 1)
        {
	  line2 = DLine(startPoint2, endPoint);
          minLine = line2.length() > minLine.length() ? minLine : line2;
        }
      if (pc2 > 1)
        {
          line3 = DLine(startPoint, endPoint2);
          minLine = line3.length() > minLine.length() ? minLine : line3;
        }
      if (pc2 > 1 && pc1 > 1)
        {
          line4 = DLine(startPoint2, endPoint2);
          minLine = line4.length() > minLine.length() ? minLine : line4;
	}

      startPoint = minLine.start();
      endPoint = minLine.end();
      if (startPoint == endPoint)
	{
	  /* self loops maybe */
	  if (it != bends.end())
       	    {
	      pc2 = 1;
	      it++;
              endPoint = (*it);
	    }
	  else
	    {
		/* self loop */
		endPoint = DPoint(n1center.m_x - 1.0, n1center.m_x - 1.0);	
	    }
	}

/* make sure we have something like:
 * start---end----center 
 * if we have end----start---center
 * swap start and end 
 */
      line1 = DLine(startPoint, n1center);
      line2 = DLine(endPoint, n1center);

      if (line2.length() < line1.length())
	{
	  DPoint tmp = endPoint;
	  endPoint = startPoint;
	  startPoint = tmp;
	}

      draw_arrow(dev, DLine(startPoint, endPoint));
    }

  it = bends.rbegin();
  if (it == bends.rend())
    {
      /* need 2 elements to make a line, just use
       * the source/targets x and y */
      linept1 = n2center;
      linept2 = n1center;
    }
  else
    {
      linept1 = *it;
      if (linept1 != n2center)
        {
	  linept2 = linept1;
          linept1 = n2center;
        }
      else
        {
          it--;
          linept2 = *it;
        }
    }


  if (arrow == GraphAttributes::last || arrow == GraphAttributes::both)
    {
      switch (ga.shapeNode(edg->target()))
	{
	case GraphAttributes::rectangle:
          {
            DLine line = DLine(linept1, linept2);
            if (targetRect.contains(linept2))
              {
		pc1=1;
                endPoint = DPoint(linept2.m_x, linept2.m_y);
              }
            else if (targetRect.topLine().intersection(line, is1, true) ||
		     targetRect.bottomLine().intersection(line, is1, true) ||
		     targetRect.leftLine().intersection(line, is1, true) ||
		     targetRect.rightLine().intersection(line, is1, true))
              {
                pc1=1;
                endPoint = DPoint(is1.m_x, is1.m_y);
              }
            else
              {
                pc1=1;
                endPoint = DPoint(linept1.m_x, linept1.m_y);
              }
          }
	  break;
	case GraphAttributes::oval:
	  {
	    pc1 = ellipseLineIntersect(linept1, linept2, linept1, ga.width(edg->target()), ga.height(edg->target()),
				       &is1, &is2);
	    cairo_save(dev->cr);
	    cairo_translate(dev->cr, n2center.m_x, n2center.m_y);
	    cairo_scale (dev->cr, ga.width(edg->target()) / 2.0,  ga.height(edg->target()) / 2.0);

	    if (pc1 == 1)
	      {
		cairo_user_to_device(dev->cr, &is1.m_x, &is1.m_y);
		cairo_restore(dev->cr);
		cairo_device_to_user(dev->cr, &is1.m_x, &is1.m_y);
		endPoint = is1;
	      }
	    else if (pc1 == 2)
	      {
		cairo_user_to_device(dev->cr, &is1.m_x, &is1.m_y);
		cairo_user_to_device(dev->cr, &is2.m_x, &is2.m_y);
		cairo_restore(dev->cr);
		cairo_device_to_user(dev->cr, &is1.m_x, &is1.m_y);
		cairo_device_to_user(dev->cr, &is2.m_x, &is2.m_y);
		endPoint = is1;
		endPoint2 = is2;
	      }
	  }
	  break;
	}
      if (linept2 == n1center)
	{
	  switch (ga.shapeNode(edg->source()))
	    {
	    case GraphAttributes::rectangle:
	      {
		DLine line = DLine(linept1, linept2);
		if (sourceRect.topLine().intersection(line, is1, true) ||
		    sourceRect.bottomLine().intersection(line, is1, true) ||
		    sourceRect.leftLine().intersection(line, is1, true) ||
		    sourceRect.rightLine().intersection(line, is1, true))
		  {
		    pc2=1;
		    startPoint = DPoint(is1.m_x, is1.m_y);
		  }
		else
		  {
		    pc2=1;
		    startPoint = DPoint(linept2.m_x, linept2.m_y);
		  }
	      }
	      break;
	    case GraphAttributes::oval:
	      {
		pc2 = ellipseLineIntersect(linept1, linept2, linept1, ga.width(edg->source()), ga.height(edg->source()),
					   &is1, &is2);
		cairo_save(dev->cr);
		cairo_translate(dev->cr, n1center.m_x, n1center.m_y);
		cairo_scale (dev->cr, ga.width(edg->source()) / 2.0,  ga.height(edg->source()) / 2.0);

		if (pc2 == 1)
		  {
		    cairo_user_to_device(dev->cr, &is1.m_x, &is1.m_y);
		    cairo_restore(dev->cr);
		    cairo_device_to_user(dev->cr, &is1.m_x, &is1.m_y);
		    startPoint = is1;
		  }
		else if (pc2 == 2)
		  {
		    cairo_user_to_device(dev->cr, &is2.m_x, &is2.m_y);
		    cairo_user_to_device(dev->cr, &is1.m_x, &is1.m_y);
		    cairo_restore(dev->cr);
		    cairo_device_to_user(dev->cr, &is1.m_x, &is1.m_y);
		    cairo_device_to_user(dev->cr, &is2.m_x, &is2.m_y);
		    startPoint = is1;
		    startPoint2 = is2;
		  }
	      }
	      break;
	    }
	}
      else
	{
	  pc2 = 1;
	  startPoint = DPoint(linept2.m_x, linept2.m_y);
	}


      line1 = DLine(startPoint, endPoint);
      minLine = line1;
      if (pc1 > 1)
        {
          line2 = DLine(startPoint, endPoint2);
          minLine = line2.length() > minLine.length() ? minLine : line2;
        }
      if (pc2 > 1)
        {
          line3 = DLine(startPoint2, endPoint);
          minLine = line3.length() > minLine.length() ? minLine : line3;
        }
      if (pc2 > 1 && pc1 > 1)
        {
          line4 = DLine(startPoint2, endPoint2);
          minLine = line4.length() > minLine.length() ? minLine : line4;
        }



      startPoint = minLine.start();
      endPoint = minLine.end();
      if (startPoint == endPoint)
        {
	  /* self loops maybe */
	  if (it != bends.rend())
            {
              pc2 = 1;
              it--;
              startPoint = (*it);
            }
	  else
	    {
	      /* self loop */
	      endPoint = DPoint(n1center.m_x - 1.0, n1center.m_x - 1.0);	
	    }
	}

/* make sure we have something like:
 * start---end----center 
 * if we have end----start---center
 * swap start and end 
 */
      line1 = DLine(startPoint, n2center);
      line2 = DLine(endPoint, n2center);

      if (line2.length() > line1.length())
	{
	  DPoint tmp = endPoint;
	  endPoint = startPoint;
	  startPoint = tmp;
	}
      draw_arrow(dev, DLine(endPoint, startPoint));
    }
}

void draw_line(drawing_device_t *dev, GraphAttributes &ga, edge &edg, int orth)
{
  DPolyline &bends = ga.bends(edg);
  ListConstIterator<DPoint> it = bends.begin();
  DPoint linept1, linept2, n1center, n2center;
  DPoint is1, is2;
  String edgeColor = ga.colorEdge(edg);
  DRect sourceRect;
  DRect targetRect;
   
  n1center.m_x = ga.x(edg->source());
  n1center.m_y = ga.y(edg->source());
  n2center.m_x = ga.x(edg->target());
  n2center.m_y = ga.y(edg->target());
  
  sourceRect = DRect(DPoint(n1center.m_x - (ga.width(edg->source()) / 2.0),
		            n1center.m_y - (ga.height(edg->source()) / 2.0)),
		     DPoint(n1center.m_x + (ga.width(edg->source()) / 2.0),
			    n1center.m_y + (ga.height(edg->source()) / 2.0)));
  targetRect = DRect(DPoint(n2center.m_x - (ga.width(edg->target()) / 2.0),
			    n2center.m_y - (ga.height(edg->target()) / 2.0)),
		     DPoint(n2center.m_x + (ga.width(edg->target()) / 2.0),
			    n2center.m_y + (ga.height(edg->target()) / 2.0)));
   
  cairo_set_line_width(dev->cr, 1.0);

  if (it == bends.end())
    {
      /* need 2 elements to make a line, just use
       * the source/targets x and y
       */
      linept1 = n1center;
      linept2 = n2center;
    }
  else
    {
      linept1 = *it;
      if (linept1 != n1center)
        {
	  linept2 = linept1;
	  linept1 = n1center;
	}
      else
        {
	  it++;
	  linept2 = *it;
	}
    }

  switch (ga.shapeNode(edg->source()))
    {
    case GraphAttributes::rectangle:
      {
  	DLine line = DLine(linept1, linept2);
	if (sourceRect.contains(linept2))
          {
	    cairo_move_to(dev->cr, linept2.m_x, linept2.m_y);
	  }
	else if (sourceRect.topLine().intersection(line, is1, true) ||
		 sourceRect.bottomLine().intersection(line, is1, true) ||
		 sourceRect.leftLine().intersection(line, is1, true) ||
		 sourceRect.rightLine().intersection(line, is1, true))
	  {
	    cairo_move_to(dev->cr, is1.m_x, is1.m_y);
	  }	
        else
          {
	    cairo_move_to(dev->cr, linept1.m_x, linept1.m_y);
          }
	break;
      }
    case GraphAttributes::oval:
      {
	int pc; // pt count
	DPoint is1, is2; /* intersection points */

	pc = ellipseLineIntersect(linept1, linept2, linept1, ga.width(edg->source()), ga.height(edg->source()),
				  &is1, &is2);
	cairo_save(dev->cr);
	cairo_translate(dev->cr, n1center.m_x, n1center.m_y);
	cairo_scale(dev->cr, ga.width(edg->source()) / 2.0,
		    ga.height(edg->source()) / 2.0);
	if (pc == 1)
	  {
	    cairo_move_to(dev->cr, is1.m_x, is1.m_y);
	  }
	else if (pc == 2)
	  {
	    cairo_move_to(dev->cr, is2.m_x, is2.m_y);
	  }
	cairo_restore(dev->cr);
      }
      break;
    }
  
  if (linept2 == n2center)
    {
      switch (ga.shapeNode(edg->target()))
        {
	case GraphAttributes::rectangle:
	  {
	    DLine line = DLine(linept1, linept2);
	    if (targetRect.topLine().intersection(line, is1, true) ||
		targetRect.bottomLine().intersection(line, is1, true) ||
		targetRect.leftLine().intersection(line, is1, true) ||
		targetRect.rightLine().intersection(line, is1, true))
	      {
		cairo_line_to(dev->cr, is1.m_x, is1.m_y);
	      }          
	  }	
	  break;
	case GraphAttributes::oval:
	  {
	    int pc; // pt count
	    DPoint is1, is2; /* intersection points */

	    pc = ellipseLineIntersect(linept1, linept2, linept1, ga.width(edg->target()), ga.height(edg->target()),
				      &is1, &is2);
	    cairo_save(dev->cr);
	    cairo_translate(dev->cr, n2center.m_x, n2center.m_y);
	    cairo_scale (dev->cr, ga.width(edg->target()) / 2.0,  ga.height(edg->target()) / 2.0);

	    if (pc >= 1)
	      {
		cairo_line_to(dev->cr, is1.m_x, is1.m_y);
	      }

	    cairo_restore(dev->cr);
	  }
	  break;
        }
    }
  else
    {
      cairo_line_to(dev->cr, linept2.m_x, linept2.m_y);
    }
  
  for (; it.valid(); ++it)
    {
      linept1 = linept2;
      linept2 = *it;
      if (linept2 == n2center)
	{
	  switch (ga.shapeNode(edg->target()))
	    {
	    case GraphAttributes::rectangle:
	      {
		DLine line = DLine(linept1, linept2);
		if (targetRect.topLine().intersection(line, is1, true) ||
		    targetRect.bottomLine().intersection(line, is1, true) ||
		    targetRect.leftLine().intersection(line, is1, true) ||
		    targetRect.rightLine().intersection(line, is1, true))
		  {
		    cairo_line_to(dev->cr, is1.m_x, is1.m_y);
		  }	
		else
		  {
		    cairo_line_to(dev->cr, linept2.m_x, linept2.m_y);
	          }
	      }
	      break;
	    case GraphAttributes::oval:
	      {
		int pc; // point count
		pc = ellipseLineIntersect(linept1, linept2, n2center,
					  ga.width(edg->target()),
					  ga.height(edg->target()),
					  &is1, &is2);
		cairo_save(dev->cr);
		cairo_translate(dev->cr, n2center.m_x, n2center.m_y);
		cairo_scale (dev->cr, ga.width(edg->target()) / 2.0,  ga.height(edg->target()) / 2.0);
		if (pc >= 1)
		  {
		    cairo_line_to(dev->cr, is1.m_x, is1.m_y);
		  }
		cairo_restore(dev->cr);
	      }
	      break;
	    }
	}
      else
	{
          cairo_line_to(dev->cr, linept2.m_x, linept2.m_y);
	}
    }

  if (linept2 != n2center)
    {
      switch (ga.shapeNode(edg->target()))
        {
	case GraphAttributes::rectangle:
	  {
	    DLine line = DLine(linept2, n2center);
	    if (targetRect.topLine().intersection(line, is1, true) ||
		targetRect.bottomLine().intersection(line, is1, true) ||
		targetRect.leftLine().intersection(line, is1, true) ||
		targetRect.rightLine().intersection(line, is1, true))
	      {
		cairo_line_to(dev->cr, is1.m_x, is1.m_y);
	      }	
          }
          break;
	case GraphAttributes::oval:
	  {
	    int pc; // point count
	    pc = ellipseLineIntersect(linept2, n2center, n2center,
				      ga.width(edg->target()),
				      ga.height(edg->target()),
				      &is1, &is2);
	    cairo_save(dev->cr);
	    cairo_translate(dev->cr, n2center.m_x, n2center.m_y);
	    cairo_scale (dev->cr, ga.width(edg->target()) / 2.0,  ga.height(edg->target()) / 2.0);
	    if (pc >= 1)
	      {
		cairo_line_to(dev->cr, is1.m_x, is1.m_y);
	      }
	    cairo_restore(dev->cr);
	  }
	  break;
        }
    }

  setColor(dev, ga.colorEdge(edg));
  cairo_stroke(dev->cr);
}

int setColor(drawing_device_t *dev, String color)
{
  if (color.length() >=7)
    {
      char r[5];
      char g[5];
      char b[5];
      char a[5];
      double d1,d2,d3;
      
      r[2] = color[1];
      r[3] = color[2];
      g[2] = color[3];
      g[3] = color[4];
      b[2] = color[5];
      b[3] = color[6];
      r[0] = g[0] = b[0] = '0'; 
      r[1] = g[1] = b[1] = 'x'; 
      r[4] = g[4] = b[4] = '\0'; 
       
      d1 = strtod(r, NULL) / 255.0;
      d2 = strtod(g, NULL) / 255.0;
      d3 = strtod(b, NULL) / 255.0;
    
      if (color.length() >= 9)
	{
	  double d4;
           
	  d4 = 1.0 / strtod(b, NULL);
	  a[0] = color[7];
	  a[8] = color[7];
	  cairo_set_source_rgba(dev->cr, d1, d2, d3, d4);
	  return 0;
	}     
      else
	{
	  cairo_set_source_rgb(dev->cr, d1, d2, d3);
	  return 0;
	}
    }
  return 1;
}


void draw_node(drawing_device_t *dev, node &v, GraphAttributes &ga)
{
  double x, y, width, height;
  int fill = 1;

  x = ga.x(v) - ga.width(v) / 2;
  y = ga.y(v) - ga.height(v) / 2;
  width = ga.width(v);
  height = ga.height(v);

  switch (ga.shapeNode(v))
    {
    case GraphAttributes::rectangle:
      cairo_rectangle(dev->cr, x, y, width, height);
      break;
    case GraphAttributes::oval:
      {
	cairo_save(dev->cr);
	cairo_translate(dev->cr, x + width / 2.0, y + height / 2.0);
	cairo_scale (dev->cr, width / 2.0, height / 2.0);
	cairo_new_sub_path(dev->cr);
	cairo_arc(dev->cr, 0.0, 0.0, 1.0, 0.0, 2 * PI);
	cairo_close_path(dev->cr);
	cairo_restore(dev->cr);
        break;
      }
    }
  if (fill)
    {
      setColor(dev, ga.nodeLine(v));
      cairo_stroke_preserve(dev->cr);
      setColor(dev, ga.colorNode(v));
      cairo_fill(dev->cr);
    }
  else
    {
      setColor(dev, ga.nodeLine(v));
      cairo_stroke(dev->cr);
    }

    String aLabel = ga.labelNode(v);
    if (aLabel.length()) {
      cairo_text_extents_t extents;

      cairo_text_extents(dev->cr, aLabel.cstr(), &extents);
      cairo_set_source_rgb(dev->cr, 0, 0, 0);
      cairo_move_to(dev->cr, (x + (width / 2.0)) - (extents.width / 2.0 + extents.x_bearing),
			     (y + (height / 2.0)) - (extents.height / 2.0 + extents.y_bearing));
      cairo_show_text(dev->cr, aLabel.cstr());
    }
}

int main(int argc, char **argv)
{
  char *filename = NULL;
  int orth = 1;
  Graph g; 
  GraphAttributes ga(g,
		     GraphAttributes::nodeGraphics | GraphAttributes::edgeGraphics |
		     GraphAttributes::nodeLabel | GraphAttributes::nodeColor |
		     GraphAttributes::edgeColor | GraphAttributes::edgeStyle |
		     GraphAttributes::nodeStyle | GraphAttributes::nodeTemplate |
		     GraphAttributes::edgeArrow);
  display_device_t dispDev;
  window_device_t winDev;
  drawing_device_t drawDev;

  if (argc > 1)
    {
      filename = argv[1];
      ga.readGML(g, filename);
    }
  else
    {
      ga.readGML(g, cin);
    }

  create_display_device(&dispDev);
  create_window_device(&dispDev, &winDev, ga.boundingBox());
  create_drawing_device(&dispDev, &winDev, &drawDev);
  cairo_set_fill_rule(drawDev.cr, CAIRO_FILL_RULE_EVEN_ODD);
  cairo_translate(drawDev.cr, fabs(ga.boundingBox().p1().m_x) + 10, fabs(ga.boundingBox().p1().m_y) + 10);

  window_setup(&dispDev, &winDev);
#ifdef USE_X
  XMapWindow(dispDev.dpy, winDev.win);

  while (shouldRun())
    {
      XEvent ev;
      XNextEvent(dispDev.dpy, &ev);

      if (ev.type == Expose && ev.xexpose.count == 0)
	{
	  XEvent ev2;
	  Bool check;
      	  node v;
	  edge edg;
	  while ((check = XCheckWindowEvent(ev.xany.display, ev.xany.window, ExposureMask, &ev2)))
	    {
	      /* as silly as it sounds just skip over any more expose events
 		 for now even though ev.xexpose.count == 0, reduce flicker. */
	    }

	  cairo_set_line_width(drawDev.cr, 1.0);
	  
	  draw_background(&winDev, &drawDev);

	  forall_nodes(v, g)
            {
	      cairo_set_line_width(drawDev.cr, 2.0);
	      draw_node(&drawDev, v, ga);
	    }

	  forall_edges(edg, g)
	    {
	      cairo_set_source_rgb(drawDev.cr, 0.0, 0.0, 0.0);
	      cairo_set_line_width(drawDev.cr, 1.0);
	      set_line_stipple(&drawDev, ga, edg);
	      draw_line(&drawDev, ga, edg, orth);
	      draw_arrows(&drawDev, ga, edg);
	    }
	}
      if (ev.type == KeyPress)
  	{
          KeySym sym = XKeycodeToKeysym(dispDev.dpy, ev.xkey.keycode, 0);
	  if (strcmp(XKeysymToString(sym), "q")  == 0)
   	    stop();
	  else if (strcmp(XKeysymToString(sym), "s")  == 0)
	    {
	         png_stream_to_file_closure_t foo_closure;
        	 foo_closure.out = fopen("foo.png", "wb");

	         cairo_status_t status;
	         status = cairo_surface_write_to_png_stream(drawDev.surf,
				 write_png_stream_to_file, &foo_closure);
	         if (status != CAIRO_STATUS_SUCCESS)  {
	             printf("error writing png: %s\n",
				 cairo_status_to_string(status));
	           }

	      //cairo_surface_write_to_png(drawDev.surf, "foo.png");
	    }
	} 
    }
#else

      	  node v;
	  edge edg;
	  cairo_set_line_width(drawDev.cr, 1.0);
	  
	  draw_background(&winDev, &drawDev);

	  forall_nodes(v, g)
            {
	      cairo_set_line_width(drawDev.cr, 2.0);
	      draw_node(&drawDev, v, ga);
	    }

	  forall_edges(edg, g)
	    {
	      cairo_set_source_rgb(drawDev.cr, 0.0, 0.0, 0.0);
	      cairo_set_line_width(drawDev.cr, 1.0);
	      set_line_stipple(&drawDev, ga, edg);
	      draw_line(&drawDev, ga, edg, orth);
	      draw_arrows(&drawDev, ga, edg);
	    }

	 png_stream_to_file_closure_t foo_closure;
	 foo_closure.out = fopen("foo.png", "wb");
		 
	 cairo_status_t status; 
	 status = cairo_surface_write_to_png_stream(drawDev.surf, write_png_stream_to_file, &foo_closure);
	 if (status != CAIRO_STATUS_SUCCESS)  {
	     printf("error writing png: %s\n", cairo_status_to_string(status));
	   }

	cairo_show_page(drawDev.cr);
	cairo_destroy(drawDev.cr);
	cairo_surface_destroy(drawDev.surf);
#endif
  return 0;
}
