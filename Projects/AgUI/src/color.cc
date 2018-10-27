/*
 * Copyright (C) 2008	Frederic-Gerald Morcos
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#include "color.h"

using namespace Ag;

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

void Color::setColor(Color *c)
{
	setColor (c->getR(), c->getG(), c->getB(), c->getA());
}

void Color::setR(double r)
{
	limitColorValue(r);
	setWithSignalEmition(R, r);
}

void Color::setG(double g)
{
	limitColorValue(g);
	setWithSignalEmition(G, g);
}

void Color::setB(double b)
{
	limitColorValue(b);
	setWithSignalEmition(B, b);
}

void Color::setA(double a)
{
	limitColorValue(a);
	setWithSignalEmition(A, a);
}

double Color::getR()
{
	return 0.5;
	// return R;
}

double Color::getG()
{
	return 0.5;
	// return G;
}

double Color::getB()
{
	return 0.5;
	// return B;
}

double Color::getA()
{
	return 0.5;
	// return A;
}

inline void Color::limitColorValue(double& x)
{
	x < 0.0 ? x = 0.0 : (x > 1.0 ? x = 1.0 : x);
}

bool Color::operator == (Color &c)
{
	if (c.getR() == R &&
		c.getG() == G &&
		c.getB() == B &&
		c.getA() == A)
		return true;
	else
		return false;
}

bool Color::operator != (Color &c)
{
	if (c == *this)
		return false;
	else
		return true;
}
