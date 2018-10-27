/*
 * Copyright (C) 2008	Frederic-Gerald Morcos
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#ifndef _COLOR_H_
#define _COLOR_H_

#include "object.h"

namespace Ag
{
class Color :
	public Object
{
	private:
		double R, G, B, A;
		inline void limitColorValue(double& x);

	public:
		Color
			(double r = 0.0, double g = 0.0, double b = 0.0, double a = 0.0);
			
		void setColor
			(double r = 0.0, double g = 0.0, double b = 0.0, double a = 0.0);

		void setColor(Color& c);
		void setColor(Color *c);
		
		void setR(double r = 0.0);
		void setG(double g = 0.0);
		void setB(double b = 0.0);
		void setA(double a = 0.0);
		double getR();
		double getG();
		double getB();
		double getA();

		bool operator == (Color &c);
		bool operator != (Color &c);
};
}

#endif /* _COLOR_H_ */
