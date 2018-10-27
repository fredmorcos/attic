/*
 * Copyright (C) 2008	Frederic-Gerald Morcos
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#include "gtk.h"

using namespace Ag;

GtkCanvas::GtkCanvas(Gdk::Color bc)
{
	modify_bg(Gtk::STATE_NORMAL, bc);
	getSignalOnChange().connect(
		sigc::mem_fun(this, &GtkCanvas::engineChanged));
}

bool GtkCanvas::on_expose_event(GdkEventExpose *event)
{
	context = get_window()->create_cairo_context();
	draw(context);
	context->stroke();
	return true;
}

/* FIXME:
 * make it use region redrawing instead of redrawing the entire canvas
 * for efficiency. will need to change the signals to emitting the changed
 * regions.
 */
void GtkCanvas::engineChanged()
{
	queue_draw();
}
