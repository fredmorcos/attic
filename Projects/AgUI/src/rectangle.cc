/*
 * Copyright (C) 2008	Frederic-Gerald Morcos
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#include <iostream>
using namespace std;

#include "rectangle.h"

using namespace Ag;

Rectangle::Rectangle(
	int x, int y, int w, int h, unsigned int bw, Color *fc, Color *bc)
{
	Shape(x, y, w, h ,bw, fc, bc);
}

void Rectangle::draw(Cairo::RefPtr<Cairo::Context> context)
{
	context->move_to(0, 0);
	context->line_to(50, 50);
	context->set_source_rgba(0.5, 0.5, 0.5, 0.5);
	context->set_line_width(getBorderWidth());
	context->rectangle(getX(), getY(), getWidth(), getHeight());
	context->stroke_preserve();

	cout << "rectangle::draw" << endl;
}
