#ifndef __POINT_H__
#define __POINT_H__

typedef struct _Point
{
	int iX, iY;
} Point;

Point *point_new(int, int);
void point_unload(Point *);

#endif /* __POINT_H__ */
