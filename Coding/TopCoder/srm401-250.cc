#include <math.h>
#include <stdlib.h>

class DreamingAboutCarrots {
public:
  int carrotsBetweenCarrots(int x1, int y1, int x2, int y2)
  {
    int res = 0;

    for (int i = x1; i < x2; i++)
      for (int j = (i > y1 ? i : y1); j < y2; j++)
	if (i == j) { ++res; break; }

    return res;
    // if (x1 > x2 || y1 > y2) return 1;
    // return (int) (sqrt(pow(abs(x2 - x1), 2) + pow(abs(y2 - y1), 2)) - 2);
  }
};
