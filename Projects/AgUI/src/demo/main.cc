/*
 * Copyright (C) 2008	Frederic-Gerald Morcos
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#include <gtkmm/main.h>
#include <gtkmm/window.h>
#include "canvas.h"

int main (int argc, char *argv[])
{
	Gtk::Main AgilithDemo(argc, argv);

	Gtk::Window window(Gtk::WINDOW_TOPLEVEL);
	DemoCanvas canvas;

	Ag::Color b (0.5, 0.5, 0.5, 0.5);
	Ag::Rectangle *r = new Ag::Rectangle (10, 10, 50, 50, 5);
	canvas.getCanvas()->addShape(r);

	window.add(canvas);
	window.maximize();
	window.show_all();

	Gtk::Main::run(window);

	return 0;
}
