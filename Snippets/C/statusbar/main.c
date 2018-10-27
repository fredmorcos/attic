#include <stdio.h>
#include <xcb/xcb.h>

int main (int argc, char **argv)
{
  int screen_num, i;
  xcb_connection_t *xcb_conn;
  const xcb_setup_t *xcb_setup;
  xcb_screen_iterator_t xcb_screen_iter;
  xcb_screen_t *xcb_screen;

  xcb_conn = xcb_connect(NULL, &screen_num);
  xcb_setup = xcb_get_setup(xcb_conn);
  xcb_screen_iter = xcb_setup_roots_iterator(xcb_setup);

  for (i = 0; i < screen_num; i++)
    xcb_screen_next(&xcb_screen_iter);

  xcb_screen = xcb_screen_iter.data;

  printf("screen info\n");
  printf("  width........: %d\n", xcb_screen->width_in_pixels);
  printf("  height.......: %d\n", xcb_screen->height_in_pixels);

  xcb_disconnect(xcb_conn);
  return 0;
}
