#ifndef BUFFER_H
#define BUFFER_H

#include <stdlib.h>

/**
 * @todo buffer_pop_back()  to remove and  return an element  from the
 * end of the buffer.
 *
 * @todo  buffer_trim() to  remove the  extraneous space  used by  the
 * buffer  once it  is known  that no  more elements  are going  to be
 * inserted.
 *
 * @todo buffer_prepend() and buffer_pop_front().
 *
 * @todo buffer_delete() to cleanup  and deallocate a buffer. Possibly
 * given a destructor which is called on each element in the buffer.
 *
 * @todo buffer_set() and buffer_get().
 */

/**
 * A semi-managed buffer.
 *
 * The  intention  of   this  buffer  implementation  is   to  keep  a
 * low-overhead abstraction  compared to manually managing  a buffer's
 * real  length  and  allocated   length.   This  buffer  will  expand
 * automatically as items are appended. An initial length may be given
 * for performance reasons of avoiding spurious allocation calls.
 */
struct buffer {
  void   *ptr;              /**
                             * Pointer to  the start of  the allocated
                             * buffer. This buffer  is realloc'd so be
                             * careful  when   keeping  references  to
                             * elements in it:  their memory locations
                             * may or may not change.
                             */

  size_t  element_size;     /**
                             * Length in bytes of a single element. In
                             * other  words,  the  sizeof()  a  single
                             * element.
                             */

  int     allocated_len;    /**
                             * The  currently  allocated  length  (aka
                             * real length) of the  buffer. This is in
                             * units of elements. `allocated_len` must
                             * always   be   larger    or   equal   to
                             * `len`. Multiplication by `element_size`
                             * is required  to get  `allocated_len` in
                             * bytes.
                             */

  int     len;              /**
                             * The actual length of data in the buffer
                             * in units of elements. Multiplication by
                             * `element_size` is required to get `len`
                             * in bytes.
                             */
};

/**
 * Creates a new buffer struct.
 *
 * @return The new buffer or `NULL` in case of allocation error.
 *
 * @param `initial_len` The desired  initially allocated length of the
 * buffer.
 *
 * @param  `element_size`  The size  in  bytes  (aka sizeof())  of  an
 * element that is to be inserted into the buffer.
 */
struct buffer *buffer_new(int initial_len, size_t element_size);

/**
 * Appends an element of size `element_size` into the buffer.
 *
 * @return The buffer struct or `NULL` in case of reallocation error.
 *
 * @param `buf` The buffer to have `element` inserted into.
 *
 * @param `element` The  element to be inserted into  the buffer. Note
 * that the element is `memcpy()`'d into the buffer: this shouldn't be
 * a problem for storing pointers.
 */
struct buffer *buffer_append(struct buffer *buf, void *element);

#endif
