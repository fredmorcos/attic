/*
 * Copyright (C) 2008	Frederic-Gerald Morcos
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#ifndef _OBJECT_H_
#define _OBJECT_H_

#include <sigc++/sigc++.h>

namespace Ag
{
  class Object
  {
  public:
    typedef sigc::signal<void> TypeSignalChange;

    Object();
    TypeSignalChange getSignalOnChange();

  protected:
    TypeSignalChange SignalOnChange;

    template<class T> void setWithSignalEmition(T &a, T &b) {
      if (a != b) {
        a = b;
        getSignalOnChange().emit();
      }
    }
  };
}

#endif /* _OBJECT_H_ */
