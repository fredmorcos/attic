#ifndef SHAPE_H_
#define SHAPE_H_

#include "color.h"

class Shape
{
	private:
		int X, Y, Width, Height;
		Color *FillColor, *BorderColor;

	public:
		Shape(int x = 0, int y = 0,
				int w = 0, int h = 0,
				Color *fc = 0, Color *bc = 0);
		~Shape();
		
		void setProperties(int x = 0, int y = 0,
							int w = 0, int h = 0,
							Color *fc = 0, Color *bc = 0);

		void setX(int x = 0);
		void setY(int y = 0);
		void setWidth(int w = 0);
		void setHeight(int h = 0);
		void setFillColor(Color *fc = 0);
		void setBorderColor(Color *bc = 0);
		void setPosition(int x = 0, int y = 0);
		void setSize(int w = 0, int h = 0);
		
		int getX();
		int getY();
		int getWidth();
		int getHeight();
		Color *getFillColor();
		Color *getBorderColor();

		virtual void draw();
};

#endif /* SHAPE_H_ */
