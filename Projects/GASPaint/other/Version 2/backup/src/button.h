#ifndef _BUTTON_H_
#define _BUTTON_H_

#include "color.h"
#include "panel.h"

typedef struct _button button;
struct _button
{
	panel container;
	char *text;
};

#endif
