#import "Extras.h"
#import <stdlib.h>
#import <math.h>

/**
 * RGB constructor.
 */
RGBColor *RGB(double r, double g, double b) {
	RGBColor *tmp = malloc(sizeof(RGBColor));
	tmp->R = r;
	tmp->G = g;
	tmp->B = b;
	return tmp;
}

/**
 * HSV constructor.
 */
HSVColor *HSV(double h, double s, double v) {
	HSVColor *tmp = malloc(sizeof(HSVColor));
	tmp->H = h;
	tmp->S = s;
	tmp->V = v;
	return tmp;
}

/**
 * Point constructor.
 */
Point *POINT(int x, int y) {
	Point *tmp = malloc(sizeof(Point));
	tmp->X = x;
	tmp->Y = y;
	return tmp;
}

/**
 * Conversion from RGB to HSV as given in the assignment.
 */
HSVColor *RGBtoHSV(RGBColor *color) {
	HSVColor	*tmp = HSV(0.0, 0.0, 0.0);
	double		max = MAX(color->R, MAX(color->G, color->B)),
				min = MIN(color->R, MIN(color->G, color->B));
	
	if (max == min)
		tmp->H = 0.0;
	else if (max == color->R && color->G >= color->B)
		tmp->H = (60 * (color->G - color->B) / (max - min)) + 0;
	else if (max == color->R && color->G < color->B)
		tmp->H = (60 * (color->G - color->B) / (max - min)) + 360;
	else if (max == color->G)
		tmp->H = (60 * (color->B - color->R) / (max - min)) + 120;
	else if (max == color->B)
		tmp->H = (60 * (color->R - color->G) / (max - min)) + 240;
	
	if (max != 0)
		tmp->S = (max - min) / max;
	
	tmp->V = max;
	
	return tmp;
}

/**
 * Conversion of HSV to RGB as given in the assignment.
 */
RGBColor *HSVtoRGB(HSVColor *color) {
	double 		hi = floorf(color->H / 60.0),
		   		f = (color->H / 60.0) - hi,
		  		p = color->V * (1.0 - color->S),
		   		q = color->V * (1.0 - f * color->S),
		   		t = color->V * (1.0 - ((1.0 - f) * color->S));
	RGBColor	*tmp = RGB(0.0, 0.0, 0.0);

	if (hi == 0.0) {
		tmp->R = color->V;
		tmp->G = t;
		tmp->B = p;
	}
	else if (hi == 1.0) {
		tmp->R = q;
		tmp->G = color->V;
		tmp->B = p;
	}
	else if (hi == 2.0) {
		tmp->R = p;
		tmp->G = color->V;
		tmp->B = t;
	}
	else if (hi == 3.0) {
		tmp->R = p;
		tmp->G = q;
		tmp->B = color->V;
	}
	else if (hi == 4.0) {
		tmp->R = t;
		tmp->G = p;
		tmp->B = color->V;
	}
	else if (hi == 5.0) {
		tmp->R = color->V;
		tmp->G = p;
		tmp->B = q;
	}

	return tmp;
}

