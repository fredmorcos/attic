/*
 * Copyright (C) 2008	Frederic-Gerald Morcos
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#ifndef _ENGINE_H_
#define _ENGINE_H_

#include <cairomm/cairomm.h>
#include <list>
#include "object.h"
#include "shape.h"

namespace Ag
{	
class Engine :
	public Object
{
	private:
		std::list<Shape *> ShapeList;

		void shapeChanged();

	public:
		Engine();
		~Engine();
		
		void addShape(Shape *s);
		void removeShape(Shape *s);
		void draw(Cairo::RefPtr<Cairo::Context> context);
};
}

#endif /* _ENGINE_H_ */

