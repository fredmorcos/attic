#include "panel.h"
/*
button new_panel (int x1, int y1, int x2, int y2, color bg)
{
	panel temp;
	temp.num_buttons = 0;
	temp.buttons = NULL;
	temp.x1 = x1;
	temp.y1 = y1;
	temp.x2 = x2;
	temp.y2 = y2;
	temp.bg = bg;
}

void panel_add_button (panel p, button b)
{
	button *old_buttons;
	int i = 0;
	
	if (p.num_buttons != 0)
	{
		old_buttons = malloc (sizeof (button) * p.num_buttons);
		
		while (i < p.num_buttons)
			old_buttons [i] = p.buttons [i++];
	}
		
	p.buttons = malloc (sizeof (button) * (p.num_buttons + 1));
	
	i = 0;
	while (i < p.num_buttons)
		p.buttons [i] = old_buttons [i++];
	p.buttons [i] = b;
	p.num_buttons++;
	
	free (old_buttons);
}
*/
