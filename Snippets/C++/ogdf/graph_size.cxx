#include <cairo.h>
#include <cairo-svg.h>
#include <ogdf/basic/Graph.h>
#include <ogdf/basic/GraphAttributes.h>

using namespace ogdf;

int main(int argc, const char *argv[])
{
  cairo_surface_t *surf;
  cairo_t *cr;
  cairo_text_extents_t extents;
  double defaultWidth = 5.0;
  double defaultHeight = 5.0;
  double vspace = 2.0;
  double hspace = 2.0;
  Graph g; 
  GraphAttributes ga(g,
	     GraphAttributes::nodeGraphics | GraphAttributes::edgeGraphics |
	     GraphAttributes::nodeLabel | GraphAttributes::nodeColor |
	     GraphAttributes::edgeColor | GraphAttributes::edgeStyle |
	     GraphAttributes::nodeStyle | GraphAttributes::nodeTemplate |
	     GraphAttributes::edgeArrow);
  node n;
  if (argc < 1)
    ga.readGML(g, argv[1]);
  else
    ga.readGML(g, cin);
  
  surf = cairo_svg_surface_create("foo.svg", 10000, 1000);
  cr = cairo_create(surf);

  ga.setAllWidth(defaultWidth);
  ga.setAllHeight(defaultHeight);
 
  forall_nodes(n, g) {
    cairo_text_extents(cr, ga.labelNode(n).cstr(), &extents);
    if (ga.shapeNode(n) == GraphAttributes::rectangle)
      {
        if (extents.width)
          ga.width(n) = extents.width + hspace * 2;
        if (extents.height)
          ga.height(n) = extents.height + vspace * 2;
      }
   else if (ga.shapeNode(n) == GraphAttributes::oval)
      {
	double f = extents.width + hspace * 2;
	double b = (extents.height + (vspace * 2)) / 2;
	double a = sqrt(pow(f, 2) + pow(b, 2));
	double minorAxis;
	double majorAxis;

        if (a < b) {
	  double tmp = a;

	  a = b;
	  b = tmp;
	}

        minorAxis = sqrt(pow(a + b, 2) - pow(f, 2));
        majorAxis = a + b;

	ga.width(n) = extents.width > extents.height
			? majorAxis
			: minorAxis;
	ga.height(n) = extents.width > extents.height
		        ? minorAxis
			: majorAxis;
      }
  }

  ga.writeGML(cout);
  return 0;
}
