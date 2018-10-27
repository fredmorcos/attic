/*
 * Copyright (C) 2008	Frederic-Gerald Morcos
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#ifndef _RECTANGLE_H_
#define _RECTANGLE_H_

#include "shape.h"

namespace Ag
{
  class Rectangle :
    public Shape
  {
  public:
		Rectangle(int x = 0, int y = 0,
              int w = 0, int h = 0,
              unsigned int bw = 0,
              Color *fc = 0, Color *bc = 0);

    virtual void draw(Cairo::RefPtr<Cairo::Context> context);
  };
}

#endif /* _RECTANGLE_H_ */
