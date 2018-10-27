#include "color.h"

Color::Color(double r, double g, double b, double a)
{
	setColor(r, g, b, a);
}

void Color::setColor(double r, double g, double b, double a)
{
	setR(r);
	setG(g);
	setB(b);
	setA(a);
}

void Color::setColor(Color& c)
{
	setColor (c.getR(), c.getG(), c.getB(), c.getA());
}

void Color::setR(double r)
{
	roundColorValue(r);
	R = r;
}

void Color::setG(double g)
{
	roundColorValue(g);
	G = g;
}

void Color::setB(double b)
{
	roundColorValue(b);
	B = b;
}

void Color::setA(double a)
{
	roundColorValue(a);
	A = a;
}

double Color::getR()
{
	return R;
}

double Color::getG()
{
	return G;
}

double Color::getB()
{
	return B;
}

double Color::getA()
{
	return A;
}

inline void Color::roundColorValue(double& x)
{
	x < 0.0 ? x = 0.0 : (x > 1.0 ? x = 1.0 : x);
}
