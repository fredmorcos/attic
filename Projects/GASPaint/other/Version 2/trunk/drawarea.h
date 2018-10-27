#ifndef _DRAWAREA_H_
#define _DRAWAREA_H_

/* function prototypes for drawarea.c */

void draw_area_configure (void);
void draw_area_expose (void);
void draw_area_clicked (GtkWidget *widget, GdkEventButton *event);
void draw_area_motion (GtkWidget *widget, GdkEventMotion *event, GtkWidget *label);

void draw_area_set_color (void);

#endif
