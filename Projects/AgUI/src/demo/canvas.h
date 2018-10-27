/*
 * Copyright (C) 2008	Frederic-Gerald Morcos
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#ifndef DEMO_CANVAS_H
#define DEMO_CANVAS_H

#include <gtkmm/table.h>
#include <gtkmm/ruler.h>
#include <gtk.h>

class DemoCanvas : public Gtk::Table
{
	private:
		Gtk::VRuler vruler;
		Gtk::HRuler hruler;
		Ag::GtkCanvas canvas;
		int CursorXPos, CursorYPos;

	protected:
		bool on_motion(GdkEventMotion* event);
		void onHRulerResize(Gtk::Allocation &allocation);
		void onVRulerResize(Gtk::Allocation &allocation);
	
	public:
		DemoCanvas();
		Ag::GtkCanvas *getCanvas();
};

#endif /* DEMO_CANVAS_H */
