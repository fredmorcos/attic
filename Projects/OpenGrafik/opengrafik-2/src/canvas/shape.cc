#include "shape.h"
#include "color.h"

Shape::Shape(int x, int y, int w, int h, Color *fc, Color *bc)
{
	setProperties(x, y, w, h, fc, bc);
}

Shape::~Shape()
{
	delete FillColor;
	delete BorderColor;
}

void Shape::setProperties(int x, int y, int w, int h, Color *fc, Color *bc)
{
	setPosition(x, y);
	setSize(w, h);
	setFillColor(fc);
	setBorderColor(bc);
}

void Shape::setPosition(int x, int y)
{
	setX(x);
	setY(y);
}

void Shape::setSize(int w, int h)
{
	setWidth(w);
	setHeight(h);
}

void Shape::setX(int x)
{
	X = x;
}

void Shape::setY(int y)
{
	Y = y;
}

void Shape::setWidth(int w)
{
	Width = w;
}

void Shape::setHeight(int h)
{
	Height = h;
}

void Shape::setFillColor(Color *fc)
{
	FillColor = fc;
	
	if (FillColor == 0)
		FillColor = new Color();
}

void Shape::setBorderColor(Color *bc)
{
	BorderColor = bc;

	if (BorderColor == 0)
		BorderColor = new Color();
}

int Shape::getX()
{
	return X;
}

int Shape::getY()
{
	return Y;
}

int Shape::getWidth()
{
	return Width;
}

int Shape::getHeight()
{
	return Height;
}

Color *Shape::getFillColor()
{
	return FillColor;
}

Color *Shape::getBorderColor()
{
	return BorderColor;
}

void Shape::draw()
{
}
