#include "book-entry.h"

/* Global for properties*/

struct _BookEntry {
  GObject parent_instance;

  char *title;
  char *author;
  int rating;
  int pages;
};

G_DEFINE_TYPE(BookEntry, book_entry, G_TYPE_OBJECT);

enum {
  PROP_0,

  PROP_TITLE,
  PROP_AUTHOR,
  PROP_RATING,
  PROP_PAGES,

  N_PROPERTIES
};

static GParamSpec *properties[N_PROPERTIES];

static void book_entry_get_property(GObject *object, guint property_id,
                                    GValue *value, GParamSpec *pspec) {
  BookEntry *self = BOOK_ENTRY(object);

  switch (property_id) {
  case PROP_TITLE:
    g_value_set_string(value, self->title);
    break;

  case PROP_AUTHOR:
    g_value_set_string(value, self->author);
    break;

  case PROP_RATING:
    g_value_set_int(value, self->rating);
    break;

  case PROP_PAGES:
    g_value_set_int(value, self->pages);
    break;

  default:
    G_OBJECT_WARN_INVALID_PROPERTY_ID(object, property_id, pspec);
    break;
  }
}

static void book_entry_set_property(GObject *object, guint property_id,
                                    const GValue *value, GParamSpec *pspec) {
  BookEntry *self = BOOK_ENTRY(object);

  switch (property_id) {
  case PROP_TITLE:
    self->title = g_value_dup_string(value);
    break;

  case PROP_AUTHOR:
    self->author = g_value_dup_string(value);
    break;

  case PROP_RATING:
    self->rating = g_value_get_int(value);
    break;

  case PROP_PAGES:
    self->pages = g_value_get_int(value);
    break;

  default:
    G_OBJECT_WARN_INVALID_PROPERTY_ID(object, property_id, pspec);
    break;
  }
}

static void book_entry_class_init(BookEntryClass *klass) {

  GObjectClass *object_class = G_OBJECT_CLASS(klass);

  /* map vfuncs */

  object_class->set_property = book_entry_set_property;
  object_class->get_property = book_entry_get_property;

  /* properties */

  properties[PROP_TITLE] = g_param_spec_string(
      "title", "Title", "The Title of the Book", "No_Title?",
      (G_PARAM_READWRITE | G_PARAM_STATIC_STRINGS | G_PARAM_EXPLICIT_NOTIFY));

  properties[PROP_AUTHOR] = g_param_spec_string(
      "author", "Author", "The Author of the Book", "No_Author?",
      (G_PARAM_READWRITE | G_PARAM_STATIC_STRINGS | G_PARAM_EXPLICIT_NOTIFY));

  properties[PROP_RATING] = g_param_spec_int(
      "rating", "Rating", "The Rating of the Book", 1 /* minimum value */,
      5 /* maximium value */, 1 /* default value */,
      (G_PARAM_READWRITE | G_PARAM_EXPLICIT_NOTIFY));

  properties[PROP_PAGES] = g_param_spec_int(
      "pages", "Pages", "The number of Pages of the Book",
      1 /* minimum value */, 1500 /* maximium value */, 1 /* default value */,
      (G_PARAM_READWRITE | G_PARAM_EXPLICIT_NOTIFY));

  g_object_class_install_properties(object_class, N_PROPERTIES, properties);
}

static void book_entry_init(BookEntry *self) {

  self->title = g_strdup("dummy");
}

const gchar *book_entry_get_title(BookEntry *self) { return self->title; }

void book_entry_set_title(BookEntry *self, const gchar *title) {
  if (g_strcmp0(title, self->title) == 0) {
    g_free(self->title);
    self->title = g_strdup(title);
  }
}

static BookEntry *book_entry_new(const char *title, const char *author,
                                 int ratings, int pages) {
  BookEntry *result;

  result = g_object_new(BOOK_TYPE_ENTRY, "title", title, "author", author,
                        "ratings", ratings, "pages", pages, NULL);

  return result;
}
