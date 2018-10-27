#include "canvas.h"
#include "shape.h"

Canvas::Canvas() {}

Canvas::~Canvas()
{
	while (!ShapeList.empty())
	{
		delete ShapeList.front();
		ShapeList.pop_front();
	}
}

void Canvas::addShape(Shape *s)
{
	ShapeList.push_back(s);
}

void Canvas::removeShape(Shape *s)
{
	ShapeList.remove(s);
}
