#ifndef __MC_CONF__
#define __MC_CONF__

#include <SDL.h>
#include <glib.h>

#define CONF_FILE "conf/mediacenter.conf"

typedef struct {
  char *title;
  char *dir;
} Collection;

typedef struct {
  /* General */
  gboolean general_debug;
  int general_fps;

  /* Font */
  char *font_file;
  int font_size;
  SDL_Color font_color;

  /* Interface */
  int gui_padding;
  SDL_Color gui_box_color;
  SDL_Color gui_sel_color;

  /* Wallpaper */
  char *wallpaper_file;
  SDL_Color wallpaper_color;

  /* Player */
  char *player_name;
  char *player_flags;
  char *player_file;

  /* Collections */
  Collection *collections;
  int num_collections;
} Conf;

void conf_load_from_file (const char *filename, Conf *c);
void conf_destroy (Conf *c);
void conf_print (Conf *c);

#endif
