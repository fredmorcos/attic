/*
 * Copyright (C) 2008	Frederic-Gerald Morcos
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#ifndef _GTK_H_
#define _GTK_H_

#include <cairomm/context.h>
#include <gtkmm/drawingarea.h>
#include "engine.h"
#include "shape.h"
#include "object.h"
#include "rectangle.h"
#include "color.h"

namespace Ag
{
class GtkCanvas :
	public Gtk::DrawingArea,
	public Ag::Engine
{
	public:
		GtkCanvas(Gdk::Color bc = Gdk::Color("White"));
		void setBackColor(Gdk::Color bc);

	protected:
		bool on_expose_event(GdkEventExpose *event);

	private:
		Cairo::RefPtr<Cairo::Context> context;

		void engineChanged();
};
}

#endif /* _GTK_H_ */
