#include <stdlib.h>
#include <vte/vte.h>

int
main (int argc, char **argv)
{
  GtkWindow *main_window;
  GtkWidget *terminal_widget;

  gtk_init(&argc, &argv);
  main_window = GTK_WINDOW(gtk_window_new(GTK_WINDOW_TOPLEVEL));
  gtk_window_set_has_resize_grip(main_window, FALSE);
  /* gtk_widget_set_size_request(GTK_WIDGET(main_window), 600, 600); */
  g_signal_connect(main_window, "destroy", G_CALLBACK(gtk_main_quit), NULL);
  terminal_widget = vte_terminal_new();
  vte_terminal_set_size(VTE_TERMINAL(terminal_widget), 80, 24);
  gtk_container_add(GTK_CONTAINER(main_window), terminal_widget);
  gtk_widget_show_all(GTK_WIDGET(main_window));
  gtk_main();
  return EXIT_SUCCESS;
}
