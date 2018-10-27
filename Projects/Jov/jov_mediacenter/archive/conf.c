#define _DEFAULT_SOURCE
#define _GNU_SOURCE

#include "conf.h"

#include <SDL.h>
#include <json.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

#define _BS 4096                /* buffer size */

static SDL_bool conf_default_general_debug = SDL_TRUE;
static unsigned int conf_default_general_fps = 30;

static char *conf_default_font_file = "fonts/NotoSans-Regular.ttf";
static unsigned int conf_default_font_size = 20;
static SDL_Color conf_default_font_color = {255, 255, 255, 255};

static unsigned int conf_default_interface_padding = 40;
static SDL_Color conf_default_interface_boxcolor = {0, 0, 0, 100};
static SDL_Color conf_default_interface_selectcolor = {50, 50, 150, 200};

static char *conf_default_wallpaper_file = "images/wp_small.jpg";
static SDL_Color conf_default_wallpaper_color = {100, 0, 0, 255};

static Collection conf_default_collection[0] = {};

typedef enum {
  ObjSubtype_None,
  ObjSubtype_Color,
  ObjSubtype_Collection
} ObjSubtype;

SDL_bool _get_prop (json_object *root,
                    const char ** const tree, unsigned int tree_len,
                    json_object **obj_p, json_type type,
                    const void *const def, void *ret,
                    ObjSubtype subtype);
SDL_bool _get_prop_helper (json_object *root,
                           const char ** const tree, unsigned int tree_len,
                           json_object **obj_p, json_type type);

SDL_bool _get_prop_helper (json_object *root,
                           const char ** const tree, unsigned int tree_len,
                           json_object **obj_p, json_type type) {
  if (tree_len == 0) {
    return SDL_TRUE;
  }

  if (json_object_object_get_ex(root, *tree, obj_p) == TRUE) {
    if (tree_len == 1 && json_object_get_type(*obj_p) != type) {
      SDL_LogInfo(SDL_LOG_CATEGORY_APPLICATION,
                  "Invalid type for property (%s)", *tree);
      return SDL_FALSE;
    }

    return _get_prop_helper(*obj_p, tree + 1, tree_len - 1, obj_p, type);
  }

  return SDL_FALSE;
}

SDL_bool _get_prop (json_object *root,
                    const char ** const tree, unsigned int tree_len,
                    json_object **obj_p, json_type type,
                    const void *const def, void *ret,
                    ObjSubtype subtype) {
  if (_get_prop_helper(root, tree, tree_len, obj_p, type) == SDL_FALSE) {
    unsigned int i = 0;

    SDL_LogError(SDL_LOG_CATEGORY_APPLICATION, "Cannot get config option");

    for (i = 0; i < tree_len; i++) {
      SDL_LogError(SDL_LOG_CATEGORY_APPLICATION, "  -> %s", tree[i]);
    }

    SDL_LogError(SDL_LOG_CATEGORY_APPLICATION, "Using default value");

    switch (type) {
    case json_type_boolean:
      *((SDL_bool *) ret) = *((SDL_bool *) def);
      break;
    case json_type_int:
      *((unsigned int *) ret) = *((unsigned int *) def);
      break;
    case json_type_string:
      *((char **) ret) = *((char **) def);
      break;
    case json_type_array:
      if (subtype == ObjSubtype_Color) {
        *((SDL_Color *) ret) = *((SDL_Color *) def);
      } else if (subtype == ObjSubtype_Collection) {
        *((Collection *) ret) = *((Collection *) def);
      }
      break;
    default:
      SDL_LogCritical(SDL_LOG_CATEGORY_APPLICATION,
                      "Unsupported type in config file. Ignoring.");
      return SDL_FALSE;
      break;
    }

    return SDL_FALSE;
  }

  switch (type) {
  case json_type_boolean:
    *((SDL_bool *) ret) = json_object_get_boolean(*obj_p) == TRUE ?
      SDL_TRUE:SDL_FALSE;
    break;
  case json_type_int:
    *((unsigned int *) ret) = json_object_get_int(*obj_p);
    break;
  case json_type_string:
    *((char **) ret) = strdup(json_object_get_string(*obj_p));
    break;
  case json_type_array:
    if (subtype == ObjSubtype_Color) {
      if (json_object_array_length(*obj_p) == 4) {
        array_list *color = json_object_get_array(*obj_p);
        SDL_Color *res = (SDL_Color *) ret;

        /* TODO we actually get JSON objects, no integers, duh... */
        res->r = *((Uint8 *) color->array[0]);
        res->g = *((Uint8 *) color->array[1]);
        res->b = *((Uint8 *) color->array[2]);
        res->a = *((Uint8 *) color->array[3]);
      } else {
        SDL_LogWarn(SDL_LOG_CATEGORY_APPLICATION,
                    "Wrong number of elements in color array. Using default.");
        *((SDL_Color *) ret) = *((SDL_Color *) def);
        return SDL_FALSE;
      }
    } else if (subtype == ObjSubtype_Collection) {
      /* TODO actually implement that */
    }
    break;
  default:
    SDL_LogCritical(SDL_LOG_CATEGORY_APPLICATION,
                    "Unsupported type in config file. Ignoring.");
    return SDL_FALSE;
    break;
  }

  return SDL_TRUE;
}

SDL_bool conf_load (const char *conf_file, Conf *c) {
  struct json_tokener *tok = NULL;
  enum json_tokener_error err;
  json_object *conf_obj = NULL, *res = NULL;
  FILE *conf_fp = NULL;
  char buffer[_BS];
  int buffer_len = 0;
  SDL_bool rc = SDL_TRUE;

  const char *path_general_debug[2] = {"general", "debug"};
  const char *path_general_fps[2] = {"general", "fps"};
  const char *path_font_file[2] = {"font", "file"};
  const char *path_font_size[2] = {"font", "size"};
  const char *path_font_color[2] = {"font", "color"};
  const char *path_interface_padding[2] = {"interface", "padding"};
  const char *path_interface_boxcolor[3] = {"interface", "box", "color"};
  const char *path_interface_selectcolor[3] = {"interface", "select", "color"};
  const char *path_wallpaper_file[2] = {"wallpaper", "file"};
  const char *path_wallpaper_color[2] = {"wallpaper", "color"};
  const char *path_collections[1] = {"collections"};

  if ((conf_fp = fopen(conf_file, "r")) == NULL) {
    SDL_LogCritical(SDL_LOG_CATEGORY_SYSTEM,
                    "Cannot open configuration file (%s): %s",
                    conf_file, strerror(errno));
    return SDL_FALSE;
  }

  tok = json_tokener_new();

  do {
    if (feof(conf_fp) != 0) {
      break;
    }

    if ((buffer_len = fread(buffer, 1, _BS, conf_fp)) < _BS) {
      if (ferror(conf_fp) != 0) {
        SDL_LogCritical(SDL_LOG_CATEGORY_SYSTEM,
                        "Cannot read from configuration file (%s): %s",
                        conf_file, strerror(errno));
        goto quit_fail;
      }
    }

    conf_obj = json_tokener_parse_ex(tok, buffer, buffer_len);
  } while ((err = json_tokener_get_error(tok)) == json_tokener_continue);

  if (err != json_tokener_success) {
    SDL_LogCritical(SDL_LOG_CATEGORY_APPLICATION,
                    "Error parsing JSON configuration file (%s): %s",
                    conf_file, json_tokener_error_desc(err));
    goto quit_fail;
  }

  _get_prop(conf_obj, path_general_debug, 2, &res, json_type_boolean,
            &conf_default_general_debug, &c->debug,
            ObjSubtype_None);
  _get_prop(conf_obj, path_general_fps, 2, &res, json_type_int,
            &conf_default_general_fps, &c->fps,
            ObjSubtype_None);
  _get_prop(conf_obj, path_font_file, 2, &res, json_type_string,
            &conf_default_font_file, &c->font_file,
            ObjSubtype_None);
  _get_prop(conf_obj, path_font_size, 2, &res, json_type_int,
            &conf_default_font_size, &c->font_size,
            ObjSubtype_None);
  /* TODO font_color */
  _get_prop(conf_obj, path_font_color, 2, &res, json_type_array,
            &conf_default_font_color, &c->font_color,
            ObjSubtype_Color);
  _get_prop(conf_obj, path_interface_padding, 2, &res, json_type_int,
            &conf_default_interface_padding, &c->interface_padding,
            ObjSubtype_None);
  /* TODO box_color */
  /* TODO select_color */
  _get_prop(conf_obj, path_wallpaper_file, 2, &res, json_type_string,
            &conf_default_wallpaper_file, &c->wallpaper_file,
            ObjSubtype_None);
  /* TODO wallpaper_color */
  /* TODO collections */

 quit:
  json_tokener_free(tok);

  if (fclose(conf_fp) != 0) {
    SDL_LogCritical(SDL_LOG_CATEGORY_SYSTEM,
                    "Cannot close configuration file (%s): %s",
                    conf_file, strerror(errno));
    rc = SDL_FALSE;
  }

  return rc;

 quit_fail:
  rc = SDL_FALSE;
  goto quit;
}

void conf_destroy (Conf *c) {
  int i = 0;

  free(c->font_file);
  free(c->wallpaper_file);

  for (i = 0; i < c->num_collections; i++) {
    free(c->collections[i].title);
    free(c->collections[i].dir);
  }

  c->num_collections = 0;
  free(c->collections);

  c->font_file = NULL;
  c->wallpaper_file = NULL;
  c->collections = NULL;
}

void conf_print (Conf *c) {
  int i = 0;

  SDL_Log("Configuration");
  SDL_Log("  General");
  SDL_Log("    Debug = %d", c->debug);
  SDL_Log("    FPS = %d", c->fps);
  SDL_Log("  Font");
  SDL_Log("    File = %s", c->font_file);
  SDL_Log("    Size = %d", c->font_size);
  SDL_Log("    Color = %d, %d, %d, %d",
          c->font_color.r, c->font_color.g,
          c->font_color.b, c->font_color.a);
  SDL_Log("  Interface");
  SDL_Log("    Padding = %d", c->interface_padding);
  SDL_Log("    Box Color = %d, %d, %d, %d",
          c->interface_box_color.r, c->interface_box_color.g,
          c->interface_box_color.b, c->interface_box_color.a);
  SDL_Log("    Select Color = %d, %d, %d, %d",
          c->interface_select_color.r, c->interface_select_color.g,
          c->interface_select_color.b, c->interface_select_color.a);
  SDL_Log("  Wallpaper");
  SDL_Log("    File = %s", c->wallpaper_file);
  SDL_Log("    Color = %d, %d, %d, %d",
          c->wallpaper_color.r, c->wallpaper_color.g,
          c->wallpaper_color.b, c->wallpaper_color.a);
  SDL_Log("  Collections");

  for (i = 0; i < c->num_collections; i++) {
    SDL_Log("    %s = %s", c->collections[i].title, c->collections[i].dir);
  }
}
