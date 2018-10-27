#ifndef _PANEL_H_
#define _PANEL_H_

#include "color.h"

// button new_panel (int, int, int, int, color);
// void panel_add_button (panel, button);

typedef struct _panel panel;
struct _panel
{
	int x1, y1, x2, y2;
//	int num_buttons;
	color bg;
//	button *buttons;
};

#endif
