/*
 * Copyright (C) 2008	Frederic-Gerald Morcos
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#include "shape.h"

using namespace Ag;

Shape::Shape(
	int x, int y, int w, int h, unsigned int bw, Color *fc, Color *bc)
{
	setProperties(x, y, w, h, bw, fc, bc);
}

Shape::~Shape()
{
//	delete FillColor;
//	delete BorderColor;
}

/* FIXME:
 * will emit several useless signals here, need to find a way
 * to solve that without too much cluttered code.
 */
void Shape::setProperties(
	int x, int y, int w, int h, unsigned int bw, Color *fc, Color *bc)
{
	setPosition(x, y);
	setSize(w, h);
	setBorderWidth(bw);
	setFillColor(fc);
	setBorderColor(bc);
}

/* FIXME:
 * will emit several useless signals here, need to find a way
 * to solve that without too much cluttered code.
 */
void Shape::setPosition(int x, int y)
{
	setX(x);
	setY(y);
}

/* FIXME:
 * will emit several useless signals here, need to find a way
 * to solve that without too much cluttered code.
 */
void Shape::setSize(int w, int h)
{
	setWidth(w);
	setHeight(h);
}

void Shape::setX(int x)
{
	setWithSignalEmition(X, x);
}

void Shape::setY(int y)
{
	setWithSignalEmition(Y, y);
}

void Shape::setWidth(int w)
{
	setWithSignalEmition(Width, w);
}

void Shape::setHeight(int h)
{
	setWithSignalEmition(Height, h);
}

void Shape::setBorderWidth(unsigned int bw)
{
	setWithSignalEmition(BorderWidth, bw);
}

/* FIXME:
 * use setWithSignalEmition() but make sure first pointers
 * will work fine with references
 */
void Shape::setFillColor(Color *fc)
{
	if (FillColor != fc)
	{
		FillColor = fc;
		getSignalOnChange().emit();
	}
}

/* FIXME:
 * use setWithSignalEmition() but make sure first pointers
 * will work fine with references
 */
void Shape::setBorderColor(Color *bc)
{
	if (BorderColor != bc)
	{
		BorderColor = bc;
		getSignalOnChange().emit();
	}
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

unsigned int Shape::getBorderWidth()
{
	return BorderWidth;
}

Color *Shape::getFillColor()
{
	return FillColor;
}

Color *Shape::getBorderColor()
{
	return BorderColor;
}

/*
 * to avoid creating an abstract class
 * any other way to solve this?
 */
void Shape::draw(Cairo::RefPtr<Cairo::Context> context)
{
}
