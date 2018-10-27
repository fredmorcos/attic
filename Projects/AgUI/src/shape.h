/*
 * Copyright (C) 2008	Frederic-Gerald Morcos
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#ifndef _SHAPE_H_
#define _SHAPE_H_

#include <cairomm/cairomm.h>
#include "object.h"
#include "color.h"

namespace Ag
{
  class Shape :
    public Object
  {
  protected:
    int X, Y, Width, Height;
    unsigned int BorderWidth;
    Color *FillColor, *BorderColor;

  public:
    Shape(int x = 0, int y = 0,
          int w = 0, int h = 0,
          unsigned int bw = 0,
          Color *fc = 0, Color *bc = 0);
    ~Shape();

    void setProperties(int x = 0, int y = 0,
                       int w = 0, int h = 0,
                       unsigned int bw = 0,
                       Color *fc = 0, Color *bc = 0);

    void setX(int x = 0);
    void setY(int y = 0);
    void setWidth(int w = 0);
    void setHeight(int h = 0);
    void setBorderWidth(unsigned int bw = 0);
    void setFillColor(Color *fc = 0);
    void setBorderColor(Color *bc = 0);
    void setPosition(int x = 0, int y = 0);
    void setSize(int w = 0, int h = 0);

    int getX();
    int getY();
    int getWidth();
    int getHeight();
    unsigned int getBorderWidth();
    Color *getFillColor();
    Color *getBorderColor();

    virtual void draw(Cairo::RefPtr<Cairo::Context> context);
  };
}

#endif /* _SHAPE_H_ */
