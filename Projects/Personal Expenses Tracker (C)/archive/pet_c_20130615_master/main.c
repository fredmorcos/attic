/*
 * This file is part of PET.
 *
 * PET is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * PET is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with PET.  If not, see <http://www.gnu.org/licenses/>.
 */

#include <glib.h>
#include <stdlib.h>

#define SHORT_DESC "- Personal Expense Tracker"
#define DESCRIPTION                                                            \
  "Licensed under the GNU GPLv3.\n"                                            \
  "Fred Morcos <fred.morcos@gmail.com>\n"                                      \
  "http://github.com/fredmorcos/pet.git"
#define SUMMARY                                                                \
  "Easily store, manage and extract information\n"                             \
  "from your personal expenses."

static gboolean verbose = FALSE;
static gboolean extended = FALSE;

static GOptionEntry entries[] = {
  { "verbose", 'v',
    0, G_OPTION_ARG_NONE,
    &verbose,
    "Show verbose output", NULL
  },
  { "extended", 'e',
    0, G_OPTION_ARG_NONE,
    &extended,
    "Show extended dates", NULL
  }
};

int main (int argc, char *argv[])
{
  GError *error = NULL;
  GOptionContext *context;

  context = g_option_context_new(SHORT_DESC);

  g_option_context_set_summary(context, SUMMARY);
  g_option_context_set_description(context, DESCRIPTION);

  g_option_context_add_main_entries(context, entries, NULL);

  if (!g_option_context_parse(context, &argc, &argv, &error)) {
    g_print(error->message);
    g_option_context_free(context);
    g_error_free(error);
    return EXIT_FAILURE;
  }

  return EXIT_SUCCESS;
}
