#define CB 0	/* channel blue */
#define CG 1	/* channel green */
#define CR 2	/* channel red */

#undef MAX
#undef MIN
#define MAX(x,y) (x >= y ? x : y)
#define MIN(x,y) (x >= y ? y : x)
/*
inline double DMAX (double x, double y) {
	return (x >= y ? x : y);
}

inline double DMIN (double x, double y) {
	return (x >= y ? y : x);
}
*/
typedef struct _RGBColor {
	double R, G, B;
} RGBColor;

typedef struct _HSVColor {
	double H, S, V;
} HSVColor;

typedef struct _Point {
	int	X, Y;
} Point;

RGBColor *RGB(double, double, double);
HSVColor *HSV(double, double, double);
Point *POINT(int, int);

HSVColor *RGBtoHSV(RGBColor *);
RGBColor *HSVtoRGB(HSVColor *);

