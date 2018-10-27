#ifndef POINT_H
#define POINT_H

typedef struct {
	double x, y;
} Point;

typedef struct {
	double width, height;
} Size;

typedef struct {
	Point position;
	Size  size;
} Geometry;

#endif

