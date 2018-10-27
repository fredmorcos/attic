#define _GNU_SOURCE

#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <time.h>

#include <SDL.h>
#include <SDL_ttf.h>
#include <SDL_image.h>

#include <dbus/dbus.h>

#include <glib.h>

#include "vid.h"
#include "obj.h"
#include "util.h"
#include "dir.h"
#include "event.h"
#include "font.h"
#include "img.h"
#include "frame.h"
#include "dirview.h"
#include "conf.h"
#include "dbus.h"

#define NUM_OBJS 5
#define TIME_MAX_LEN 9
#define DATE_MAX_LEN 30

#define ROOT_DIR "/home/fnm/Workspace/projects"

int main (void) {
  SDL_Window *win = NULL;
  SDL_Renderer *rend = NULL;

  SDL_Event ev, per_sec_ev;     /* Event placeholders */

  int rc = EXIT_SUCCESS;        /* Program exit code */
  int i = 0;                    /* General loop counter */

  int out_w = 0, out_h = 0;     /* Output size */

  /* Font and image stuff */
  TTF_Font *font = NULL;
  SDL_Texture *wp_texture = NULL;

  DirView dv;
  Obj time, date, fps, files, info;
  Obj *objs[NUM_OBJS] = { &time, &date, &fps, &files, &info };

  char time_text[TIME_MAX_LEN], date_text[DATE_MAX_LEN], *fps_text = NULL;

  /* FPS and frame capping calculation stuff */
  unsigned int time_l = 0, frames = 0, fc_time_l = 0;

  SDL_bool size_changed = SDL_TRUE, need_render = SDL_TRUE;

  Dir root, *dirp = NULL;
  SDL_bool dir_err = SDL_FALSE;

  Conf conf;

  DBusConnection *conn = NULL;
  char *player_cmd = NULL;
  char **player_argv = NULL;
  int player_argv_len = 0;
  GPid player_pid;
  GSpawnFlags player_spawn_flags;

  GError *err = NULL;

  /* zero out most objects */
  SDL_zero(conf);
  SDL_zero(dv);
  for (i = 0; i < NUM_OBJS; i++) SDL_zero(*(objs[i]));

  /* conf_set_defaults(&conf); */
  conf_load_from_file(CONF_FILE, &conf);
  if (conf.general_debug) conf_print(&conf);

  /* $$$$$$$$$$$$$$$$$  PLAYER + DBUS STUFF $$$$$$$$$$$$$$$$$ */
  if ((player_cmd = g_strdup_printf
       ("%s %s %s",
        conf.player_name,
        conf.player_flags,
        conf.player_file)) == NULL) {
    SDL_LogCritical(SDL_LOG_CATEGORY_SYSTEM,
                    "Cannot allocate string for player command");
    goto quit_fail;
  }

  if (g_shell_parse_argv
      (player_cmd, &player_argv_len, &player_argv, &err) == FALSE) {
    if (err == NULL) {
      g_warning("Could not parse player command (%s) but no error was reported",
                player_cmd);
    } else {
      g_warning("Could not parse player command (%s): %s",
                player_cmd, err->message);
      g_clear_error(&err);
    }
  } else {

  player_spawn_flags =
    G_SPAWN_SEARCH_PATH |
    G_SPAWN_SEARCH_PATH_FROM_ENVP |
    (conf.general_debug == TRUE ? 0 :
     G_SPAWN_STDOUT_TO_DEV_NULL |
     G_SPAWN_STDERR_TO_DEV_NULL);

  if (g_spawn_async_with_pipes
      (NULL, player_argv, NULL, player_spawn_flags,
       NULL, NULL, &player_pid, NULL, NULL, NULL, &err) == FALSE) {
    if (err) {
      g_warning("Error running player:");
      g_warning("  Command = %s", player_cmd);
      g_warning("  Message = %s", err->message);
      g_clear_error(&err);
    } else {
      g_warning("Could not run player but no error was returned");
    }
  }

  g_free(player_cmd);
  g_strfreev(player_argv);

  /* TODO Temporary wait for 5 seconds */
  SDL_Delay(5000);

  /* Initialize DBus connection */
  conn = dbus_init(&conf);
  dbus_mpris_quit(conn, conf.player_name);

  g_spawn_close_pid(player_pid);

  }

  /* $$$$$$$$$$$$$$$$$  END PLAYER + DBUS STUFF $$$$$$$$$$$$$$$$$ */

  /* Initialize video and other stuff */
  if (video_init() == SDL_FALSE) goto quit_fail;
  if (conf.general_debug) video_print_drivers();
  if (conf.general_debug) video_print_displays_and_modes();

  if (!(win = SDL_CreateWindow("Media Center",
                               SDL_WINDOWPOS_CENTERED,
                               SDL_WINDOWPOS_CENTERED,
                               0, 0, SDL_WINDOW_FULLSCREEN_DESKTOP))) {
    SDL_LogCritical(SDL_LOG_CATEGORY_VIDEO,
                    "Cannot initialize window: %s\n", SDL_GetError());
    goto quit_fail;
  }

  if (!(rend = SDL_CreateRenderer(win, -1, SDL_RENDERER_PRESENTVSYNC))) {
    SDL_LogCritical(SDL_LOG_CATEGORY_VIDEO,
                    "Cannot initialize renderer: %s\n", SDL_GetError());
    goto quit_fail;
  }

  video_renderer_print_info(rend, &conf, &out_w, &out_h);

  files.rect.x = conf.gui_padding;
  files.rect.y = conf.gui_padding;
  files.rect.w = (out_w / 2) - (conf.gui_padding * 2);
  files.rect.h = out_h - (conf.gui_padding * 2);

  obj_set_bg(&files, rend, &conf.gui_box_color);
  obj_blit_textures(&files, rend);

  dv.rect = files.rect;

  info.rect.x = files.rect.x + files.rect.w + conf.gui_padding;
  info.rect.w = out_w - files.rect.w - (conf.gui_padding * 3);
  info.rect.h = 1;

  if ((font = font_init(conf.font_file, conf.font_size)) == NULL) {
    goto quit_fail;
  }

  if (img_init() != SDL_FALSE) {
    wp_texture = img_get_texture(conf.wallpaper_file, rend);
  }

  if (event_create(&per_sec_ev) == SDL_FALSE) goto quit_fail;
  SDL_PushEvent(&per_sec_ev);

  frame_fps_text(&fps_text, 0);

  /* Dir stuff $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ */
  SDL_zero(root);
  root.name = ROOT_DIR;
  dirp = &root;

  if ((dirp->num_ents = dir_load_entries(dirp)) == -1) {
    goto quit_fail;
  }

  /* TODO remove that some time later... */
  for (i = 0; i < dirp->num_ents; i++) {
    if (dirp->ents[i]->d_type == DT_DIR) {
      SDL_Log("%s/", dirp->ents[i]->d_name);
    } else {
      SDL_Log("%s", dirp->ents[i]->d_name);
    }
  }

  dirp = dir_open_entry(dirp, dirp->ents[3], &dir_err);
  if ((dirp->num_ents = dir_load_entries(dirp)) == -1) goto quit_fail;

  for (i = 0; i < dirp->num_ents; i++) {
    if (dirp->ents[i]->d_type == DT_DIR) {
      SDL_Log("%s/", dirp->ents[i]->d_name);
    } else {
      SDL_Log("%s", dirp->ents[i]->d_name);
    }
  }

  dirp = dir_up(dirp);
  if ((dirp->num_ents = dir_load_entries(dirp)) == -1) goto quit_fail;

  for (i = 0; i < dirp->num_ents; i++) {
    if (dirp->ents[i]->d_type == DT_DIR) {
      SDL_Log("%s/", dirp->ents[i]->d_name);
    } else {
      SDL_Log("%s", dirp->ents[i]->d_name);
    }
  }

  /* End dir stuff $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ */

  dirview_load(&dv, dirp, rend,
               font,
               &conf.font_color,
               &conf.gui_sel_color);

  time_l = SDL_GetTicks();

  /* Main loop */
  while (1) {
    fc_time_l = SDL_GetTicks();

    while (SDL_PollEvent(&ev)) {
      if (ev.type == SDL_KEYUP) {
        switch (ev.key.keysym.sym) {
        case SDLK_ESCAPE:
          goto quit;
        case SDLK_DOWN:
          dirview_select_next(&dv, rend, &conf.gui_sel_color);
          need_render = SDL_TRUE;
          break;
        case SDLK_UP:
          dirview_select_prev(&dv, rend, &conf.gui_sel_color);
          need_render = SDL_TRUE;
          break;
        }
      } else if (ev.type == SDL_QUIT) {
        goto quit;
      } else if (ev.type == per_sec_ev.type) {
        util_time_date(time_text, TIME_MAX_LEN, date_text, DATE_MAX_LEN);

        obj_set_text(&time, rend,
                     &conf.font_color, font,
                     time_text, &size_changed);
        obj_set_text(&date, rend,
                     &conf.font_color, font,
                     date_text, &size_changed);
        obj_set_text(&fps, rend,
                     &conf.font_color, font,
                     fps_text, &size_changed);

        if (size_changed == SDL_TRUE) {
          time.rect.x = out_w - time.rect.w - conf.gui_padding;
          time.rect.y = conf.gui_padding;

          date.rect.x = out_w - date.rect.w - conf.gui_padding;
          date.rect.y = time.rect.y + time.rect.h;

          fps.rect.x = out_w - fps.rect.w - conf.gui_padding;
          fps.rect.y = date.rect.y + date.rect.h;

          info.rect.y = fps.rect.y + fps.rect.h + conf.gui_padding;
          info.rect.h = out_h - (fps.rect.y + fps.rect.h) -
            (conf.gui_padding * 2);

          obj_set_bg(&time, rend, &conf.gui_box_color);
          obj_set_bg(&date, rend, &conf.gui_box_color);
          obj_set_bg(&fps,  rend, &conf.gui_box_color);
          obj_set_bg(&info, rend, &conf.gui_box_color);

          size_changed = SDL_FALSE;
        }

        obj_blit_textures(&time, rend);
        obj_blit_textures(&date, rend);
        obj_blit_textures(&fps, rend);
        obj_blit_textures(&info, rend);

        need_render = SDL_TRUE;
      }
    }

    if (need_render == SDL_TRUE) {
      if (wp_texture) {
        SDL_RenderCopy(rend, wp_texture, NULL, NULL);
      } else {
        if (SDL_SetRenderDrawColor(rend,
                                   conf.wallpaper_color.r,
                                   conf.wallpaper_color.g,
                                   conf.wallpaper_color.b,
                                   conf.wallpaper_color.a) < 0) {
          SDL_LogError(SDL_LOG_CATEGORY_VIDEO,
                       "Cannot set render draw color: %s\n", SDL_GetError());
        }

        if (SDL_RenderClear(rend) < 0) {
          SDL_LogError(SDL_LOG_CATEGORY_VIDEO,
                       "Cannot clear renderer: %s\n", SDL_GetError());
        }
      }

      for (i = 0; i < NUM_OBJS; i++) {
        SDL_RenderCopy(rend, objs[i]->texture, NULL, &objs[i]->rect);
      }

      dirview_render(&dv, rend);

      need_render = SDL_FALSE;
      SDL_RenderPresent(rend);
    }

    frame_cap(conf.general_fps, fc_time_l);
    frames++;

    if (frame_fps_calc(time_l, frames, &fps_text) == SDL_TRUE) {
      SDL_PushEvent(&per_sec_ev);
      frames = 0;
      time_l = SDL_GetTicks();
    } else {
      fps_text = NULL;
    }
  }

 quit:
  free(fps_text);
  dir_destroy(dirp);
  dirview_destroy(&dv);
  conf_destroy(&conf);

  for (i = 0; i < NUM_OBJS; i++) obj_destroy_textures(objs[i]);

  SDL_DestroyTexture(wp_texture);
  SDL_DestroyRenderer(rend);
  SDL_DestroyWindow(win);

  TTF_CloseFont(font);

  IMG_Quit();
  TTF_Quit();
  SDL_Quit();

  dbus_close(conn);

  return rc;

 quit_fail:
  rc = EXIT_FAILURE;
  goto quit;
}
