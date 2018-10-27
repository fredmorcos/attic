#ifndef _UI_H_
#define _UI_H_

#include "panel.h"
#include "button.h"
#include "color.h"

/* our gl window size */
#define WIN_H 500
#define WIN_W 700

/* our gl window position */
#define WIN_X 20
#define WIN_Y 20

/* our toolbox size */
#define TBOX_W (WIN_W / 5)
#define TBOX_H (WIN_H / 2.2)

/* our colorbox size */
#define CBOX_W TBOX_W
#define CBOX_H TBOX_H

/* please don't kill me for this */
panel tbox, cbox;
button sbox;
color pc;

void init_ui (void);

#endif
