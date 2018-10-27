#define _GNU_SOURCE

#include "dbus.h"
#include "conf.h"

#include <dbus/dbus.h>
#include <SDL.h>

DBusConnection *dbus_init (Conf *conf) {
  DBusConnection *conn;
  DBusError err;
  char *bus_id = NULL;

  dbus_error_init(&err);
  conn = dbus_bus_get_private(DBUS_BUS_SESSION, &err);

  if (dbus_error_is_set(&err)) {
    if (conn) {
      dbus_connection_unref(conn);
      conn = NULL;
    }

    SDL_LogError(SDL_LOG_CATEGORY_APPLICATION,
                 "Cannot initialize DBus connection: %s: %s",
                 err.name, err.message);

    dbus_error_free(&err);
    return NULL;
  }

  dbus_error_init(&err);
  bus_id = dbus_bus_get_id(conn, &err);

  if (dbus_error_is_set(&err)) {
    SDL_LogError(SDL_LOG_CATEGORY_APPLICATION,
                 "Error getting DBus bus ID: %s: %s",
                 err.name, err.message);
  } else {
    if (conf->general_debug) SDL_Log("DBUS: Bus ID: %s", bus_id);
  }

  if (conf->general_debug)
    SDL_Log("DBUS: Bus name: %s", dbus_bus_get_unique_name(conn));

  dbus_connection_set_exit_on_disconnect(conn, FALSE);
  return conn;
}

void dbus_close (DBusConnection *conn) {
  if (conn == NULL) {
    return;
  }

  if (dbus_connection_get_is_connected(conn)) {
    dbus_connection_close(conn);
  }

  dbus_connection_unref(conn);
  dbus_shutdown();
}

SDL_bool dbus_mpris_quit (DBusConnection *conn, const char *name) {
  char *dbus_channel_name = NULL;
  DBusMessage *msg = NULL;

  if (conn == NULL) {
    return SDL_FALSE;
  }

  if (asprintf(&dbus_channel_name, "org.mpris.MediaPlayer2.%s", name) == -1) {
    SDL_LogError(SDL_LOG_CATEGORY_SYSTEM,
                 "Cannot allocate DBus Channel Name string");
    return SDL_FALSE;
  }

  msg = dbus_message_new_method_call
    (dbus_channel_name,
     "/org/mpris/MediaPlayer2",
     "org.mpris.MediaPlayer2",
     "Quit");

  if (dbus_connection_send(conn, msg, NULL) == FALSE) {
    SDL_LogCritical(SDL_LOG_CATEGORY_SYSTEM,
                    "Error sending MPRIS message over DBus");
    dbus_message_unref(msg);
    return SDL_FALSE;
  }

  dbus_message_unref(msg);
  free(dbus_channel_name);

  return SDL_TRUE;
}

  /* char *username = "fred"; */
  /* char *userpath = NULL; */
  /* DBusMessage *msg = dbus_message_new_method_call */
  /*   ("org.freedesktop.Accounts", */
  /*    "/org/freedesktop/Accounts", */
  /*    "org.freedesktop.Accounts", */
  /*    "FindUserByName"); */
  /* dbus_message_append_args(msg, DBUS_TYPE_STRING, &username, DBUS_TYPE_INVALID); */
  /* DBusError err; */
  /* dbus_error_init(&err); */
  /* DBusMessage *reply = dbus_connection_send_with_reply_and_block */
  /*   (conn, msg, 300, &err); */
  /* dbus_message_unref(msg); */

  /* if (dbus_error_is_set(&err)) { */
  /*   SDL_LogError(SDL_LOG_CATEGORY_APPLICATION, */
  /*                "DBus ERROR: %s: %s", err.name, err.message); */
  /*   goto quit_fail; */
  /* } */

  /* if (reply) { */
  /*   dbus_error_init(&err); */
  /*   dbus_message_get_args(reply, &err, */
  /*                         DBUS_TYPE_OBJECT_PATH, &userpath, */
  /*                         DBUS_TYPE_INVALID); */
  /*   dbus_message_unref(reply); */

  /*   if (dbus_error_is_set(&err)) { */
  /*     SDL_LogError(SDL_LOG_CATEGORY_APPLICATION, */
  /*                  "DBus ERROR: %s: %s", err.name, err.message); */
  /*     goto quit_fail; */
  /*   } else { */
  /*     SDL_Log("%s", userpath); */
  /*   } */
  /* } */
