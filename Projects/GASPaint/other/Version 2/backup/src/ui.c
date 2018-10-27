#include "ui.h"

void init_ui ()
{
	/* initialize the toolbox and colorsbox */
	#define GRAY_RGB 0.7;
	pc.red = GRAY_RGB;
	pc.green = GRAY_RGB;
	pc.blue = GRAY_RGB;
	
	/* where the tools will be */
	tbox.x1 = -TBOX_W + 2;
	tbox.y1 = 4;
	tbox.x2 = 0;
	tbox.y2 = TBOX_H;
	tbox.bg = pc;
	
	/* where the colors will be */
	cbox.x1 = tbox.x1;
	cbox.y1 = tbox.y2 + 4;
	cbox.x2 = tbox.x2;
	cbox.y2 = cbox.y1 + CBOX_H;
	cbox.bg = pc;
	
	/* where the x and y coords will be */
	sbox.container.x1 = cbox.x1;
	sbox.container.y1 = cbox.y2 + 4;
	sbox.container.x2 = cbox.x2;
	sbox.container.y2 = WIN_H - 2;
	sbox.container.bg = pc;
	sbox.text = "hello!!!";
}
