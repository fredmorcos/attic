#define _DEFAULT_SOURCE
#define _GNU_SOURCE

#include "conf.h"

#include <SDL.h>
#include <glib.h>

typedef enum {
  conf_type_boolean,
  conf_type_string,
  conf_type_int,
  conf_type_color
} ConfType;

static gboolean def_general_debug = TRUE;
static int def_general_fps = 30;

static char *def_font_file = "fonts/NotoSans-Regular.ttf";
static int def_font_size = 20;
static SDL_Color def_font_color = {255, 255, 255, 255};

static int def_gui_padding = 40;
static SDL_Color def_gui_box_color = {0, 0, 0, 100};
static SDL_Color def_gui_sel_color = {50, 50, 150, 200};

static char *def_player_name = "vlc";
static char *def_player_flags = "";
static char *def_player_file = "";

static char *def_wallpaper_file = "images/wp_small.jpg";
static SDL_Color def_wallpaper_color = {100, 0, 0, 255};

static char *def_dir = "~";

inline int cap_col (int v);
inline void cpr_col (const char *prefix, SDL_Color *c);
void conf_get_value (GKeyFile *keyfile, const char *filename,
                     const char *group, const char *key,
                     void *res, void *def, ConfType type);

int cap_col (int v) {
  return v < 0 ? 0 : v > 255 ? 255 : v;
}

void cpr_col (const char *prefix, SDL_Color *c) {
  SDL_Log("%s = %d, %d, %d, %d", prefix, c->r, c->g, c->b, c->a);
}

void conf_get_value (GKeyFile *keyfile, const char *filename,
                     const char *group, const char *key,
                     void *res, void *def, ConfType type) {
  const char *msg = "Cannot read `%s:%s` value from config file (%s): %s";
  GError *err = NULL;

  if (g_key_file_has_group(keyfile, group) &&
      g_key_file_has_key(keyfile, group, key, &err) && err == NULL) {
    gboolean bool;
    char *str;
    int i;
    int *col;
    gsize len;

    switch (type) {
    case conf_type_boolean:
      bool = g_key_file_get_boolean(keyfile, group, key, &err);
      break;
    case conf_type_color:
      col = g_key_file_get_integer_list(keyfile, group, key, &len, &err);
      break;
    case conf_type_int:
      i = g_key_file_get_integer(keyfile, group, key, &err);
      break;
    case conf_type_string:
      str = g_key_file_get_string(keyfile, group, key, &err);
      break;
    }

    if (err == NULL) {
      switch (type) {
      case conf_type_boolean:
        *((gboolean *) res) = bool;
        break;
      case conf_type_color:
        if (len == 4) {
          SDL_Color *color = (SDL_Color *) res;

          color->r = cap_col(col[0]);
          color->g = cap_col(col[1]);
          color->b = cap_col(col[2]);
          color->a = cap_col(col[3]);

          g_free(col);
        } else {
          g_warning(msg, group, key, filename,
                    "Length of list of color values != 4");
          goto set_default;
        }
        break;
      case conf_type_int:
        *((int *) res) = i;
        break;
      case conf_type_string:
        *((char **) res) = str;
        break;
      }

      return;
    }
  }

  if (err) {
    g_warning(msg, group, key, filename, err->message);
    g_clear_error(&err);
  }

 set_default:
  switch (type) {
  case conf_type_boolean:
    *((gboolean *) res) = *((gboolean *) def);
    break;
  case conf_type_color:
    *((SDL_Color *) res) = *((SDL_Color *) def);
    break;
  case conf_type_int:
    *((int *) res) = *((int *) def);
    break;
  case conf_type_string:
    *((char **) res) = g_strdup(*((char **) def));
    break;
  }
}

void conf_load_from_file (const char *filename, Conf *c) {
  GKeyFile *keyfile = NULL;
  GError *err = NULL;

  keyfile = g_key_file_new();
  g_key_file_set_list_separator(keyfile, ',');

  if (g_key_file_load_from_file
      (keyfile, filename,
       G_KEY_FILE_KEEP_COMMENTS |
       G_KEY_FILE_KEEP_TRANSLATIONS, &err) == FALSE) {
    g_warning("Error loading configuration file (%s): %s\n",
              filename, err->message);
    g_clear_error(&err);
    return;
  }

  conf_get_value(keyfile, filename, "General", "debug",
                 &c->general_debug, &def_general_debug,
                 conf_type_boolean);
  conf_get_value(keyfile, filename, "General", "fps",
                 &c->general_fps, &def_general_fps,
                 conf_type_int);
  conf_get_value(keyfile, filename, "Font", "file",
                 &c->font_file, &def_font_file,
                 conf_type_string);
  conf_get_value(keyfile, filename, "Font", "size",
                 &c->font_size, &def_font_size,
                 conf_type_int);
  conf_get_value(keyfile, filename, "Font", "color",
                 &c->font_color, &def_font_color,
                 conf_type_color);
  conf_get_value(keyfile, filename, "GUI", "padding",
                 &c->gui_padding, &def_gui_padding,
                 conf_type_int);
  conf_get_value(keyfile, filename, "GUI", "box_color",
                 &c->gui_box_color, &def_gui_box_color,
                 conf_type_color);
  conf_get_value(keyfile, filename, "GUI", "sel_color",
                 &c->gui_sel_color, &def_gui_sel_color,
                 conf_type_color);
  conf_get_value(keyfile, filename, "Wallpaper", "file",
                 &c->wallpaper_file, &def_wallpaper_file,
                 conf_type_string);
  conf_get_value(keyfile, filename, "Wallpaper", "color",
                 &c->wallpaper_color, &def_wallpaper_color,
                 conf_type_color);
  conf_get_value(keyfile, filename, "Player", "name",
                 &c->player_name, &def_player_name,
                 conf_type_string);
  conf_get_value(keyfile, filename, "Player", "flags",
                 &c->player_flags, &def_player_flags,
                 conf_type_string);
  conf_get_value(keyfile, filename, "Player", "file",
                 &c->player_file, &def_player_file,
                 conf_type_string);

  if (g_key_file_has_group(keyfile, "Collections")) {
    gsize len;
    char **keys = g_key_file_get_keys(keyfile, "Collections", &len, &err);

    if (err) {
      g_warning("Cannot get keys under `Collections` from config file (%s): %s",
                filename, err->message);
      g_clear_error(&err);
    } else {
      gsize i;

      if ((c->collections = (Collection *) g_try_malloc0_n
           (len, sizeof(Collection))) == NULL) {
        g_warning("Could not allocate space to store collection information");
        g_strfreev(keys);

        c->num_collections = 0;
      } else {
        for (i = 0; i < len; i++) {
          conf_get_value(keyfile, filename, "Collections", keys[i],
                         &c->collections[i].dir, &def_dir, conf_type_string);
          c->collections[i].title = keys[i];
        }

        c->num_collections = (int) len;
        g_free(keys);
      }
    }
  }

  g_key_file_unref(keyfile);
}

void conf_destroy (Conf *c) {
  int i = 0;

  g_clear_pointer(&c->font_file, g_free);
  g_clear_pointer(&c->wallpaper_file, g_free);

  for (i = 0; i < c->num_collections; i++) {
    g_clear_pointer(&c->collections[i].title, g_free);
    g_clear_pointer(&c->collections[i].dir, g_free);
  }

  c->num_collections = 0;
  g_clear_pointer(&c->collections, g_free);

  g_clear_pointer(&c->player_name, g_free);
  g_clear_pointer(&c->player_flags, g_free);
  g_clear_pointer(&c->player_file, g_free);
}

void conf_print (Conf *c) {
  int i = 0;

  SDL_Log("Configuration");

  SDL_Log("  General");
  SDL_Log("    Debug = %d", c->general_debug);
  SDL_Log("    FPS = %d", c->general_fps);

  SDL_Log("  Font");
  SDL_Log("    File = %s", c->font_file);
  SDL_Log("    Size = %d", c->font_size);
  cpr_col("    Color", &c->font_color);

  SDL_Log("  GUI");
  SDL_Log("    Padding = %d", c->gui_padding);
  cpr_col("    Box Color", &c->gui_box_color);
  cpr_col("    Select Color", &c->gui_sel_color);

  SDL_Log("  Wallpaper");
  SDL_Log("    File = %s", c->wallpaper_file);
  SDL_Log("    Color", &c->wallpaper_color);

  SDL_Log("  Player");
  SDL_Log("    Name = %s", c->player_name);
  SDL_Log("    Flags = %s", c->player_flags);
  SDL_Log("    File = %s", c->player_file);

  SDL_Log("  Collections");
  for (i = 0; i < c->num_collections; i++) {
    SDL_Log("    %s = %s", c->collections[i].title, c->collections[i].dir);
  }
}
