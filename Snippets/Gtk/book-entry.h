#ifndef BOOK_ENTRY_H
#define BOOK_ENTRY_H

#include <glib-object.h>

G_BEGIN_DECLS

#define BOOK_TYPE_ENTRY (book_entry_get_type())

G_DECLARE_FINAL_TYPE(BookEntry, book_entry, BOOK, ENTRY, GObject)

const gchar *book_entry_get_title(BookEntry *self);

void book_entry_set_title(BookEntry *self, const gchar *title);

G_END_DECLS

#endif /*BOOK_ENTRY_H*/
