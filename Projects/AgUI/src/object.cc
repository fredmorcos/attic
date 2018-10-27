/*
 * Copyright (C) 2008	Frederic-Gerald Morcos
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#include "object.h"

using namespace Ag;

Object::Object()
{
}

Object::TypeSignalChange Object::getSignalOnChange()
{
	return SignalOnChange;
}
