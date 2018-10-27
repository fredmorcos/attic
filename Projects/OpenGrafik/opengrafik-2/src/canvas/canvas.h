#ifndef CANVAS_H_
#define CANVAS_H_

#include "shape.h"
#include <list>

class Canvas
{
	private:
		std::list<Shape *> ShapeList;

	public:
		Canvas();
		~Canvas();
		
		void addShape(Shape *s);
		void removeShape(Shape *s);
};

#endif /* CANVAS_H_ */

