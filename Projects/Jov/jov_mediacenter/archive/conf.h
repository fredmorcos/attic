#ifndef __MC_CONF__
#define __MC_CONF__

#include <SDL.h>

#define CONF_FILE "conf/mediacenter.json"

typedef struct {
  char *title;
  char *dir;
} Collection;

typedef struct {
  /* General */
  SDL_bool debug;
  unsigned int fps;

  /* Font */
  char *font_file;
  unsigned int font_size;
  SDL_Color font_color;

  /* Interface */
  unsigned int interface_padding;
  SDL_Color interface_box_color;
  SDL_Color interface_select_color;

  /* Wallpaper */
  char *wallpaper_file;
  SDL_Color wallpaper_color;

  /* Collections */
  Collection *collections;
  int num_collections;
} Conf;

SDL_bool conf_load (const char *conf_file, Conf *c);
void conf_destroy (Conf *c);
void conf_print (Conf *c);

#endif
