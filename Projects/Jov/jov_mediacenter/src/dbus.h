#ifndef __MC_DBUS__
#define __MC_DBUS__

#include <dbus/dbus.h>
#include <SDL.h>

#include "conf.h"

DBusConnection *dbus_init (Conf *conf);
void dbus_close (DBusConnection *conn);

SDL_bool dbus_mpris_quit (DBusConnection *conn, const char *name);

#endif
