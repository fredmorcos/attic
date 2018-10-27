/*
 * Copyright (C) 2008	Frederic-Gerald Morcos
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#include <iostream>
using namespace std;

#include "engine.h"

using namespace Ag;

Engine::Engine()
{
}

Engine::~Engine()
{
/*	while (!ShapeList.empty())
	{
		delete ShapeList.front();
		ShapeList.pop_front();
	}
*/
}

void Engine::addShape(Shape *s)
{
	ShapeList.push_back(s);
	s->getSignalOnChange().connect(sigc::mem_fun(this, &Engine::shapeChanged));
	getSignalOnChange().emit();
}

void Engine::removeShape(Shape *s)
{
	ShapeList.remove(s);
	getSignalOnChange().emit();
}

void Engine::shapeChanged()
{
	getSignalOnChange().emit();
}

void Engine::draw(Cairo::RefPtr<Cairo::Context> context)
{
	for (std::list<Shape *>::iterator it = ShapeList.begin(); it != ShapeList.end(); ++it)
		(*it)->draw(context);

	cout << "engine::draw" << endl;
}
