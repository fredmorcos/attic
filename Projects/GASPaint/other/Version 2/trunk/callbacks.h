#ifndef _CALLBACKS_H_
#define _CALLBACKS_H_

/* function prototypes for callbacks.c */

void btn_tool_clicked (GtkWidget *widget, GtkLabel *label);
void current_color_set (GtkWidget *widget);
void btn_new_clicked (void);
void btn_open_clicked (void);
void btn_save_clicked (void);
void btn_undo_clicked (void);
void btn_redo_clicked (void);

#endif
