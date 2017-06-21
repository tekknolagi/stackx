#ifndef SEG_H
#define SEG_H

#include <stdlib.h>
#include <assert.h>
#include <string.h>

#include "word.h"

struct seg {
        word id;
        word len;
        word contents[];
};

typedef struct seg *Seg_T;

#define T Seg_T

static T seg_new (word size, word id);
static word seg_id (T s);
static word seg_get (T seg, word off);
static word *seg_get_contents (T seg);
static T seg_dup (T seg);
static void seg_set (T seg, word off, word val);
static word seg_len (T seg);
static void seg_free (T *s);

static inline T seg_new (word size, word id)
{
        /* calloc ensures every bit is 0 */
        /* TODO: figure out why it needs 4 extra bytes */
        T seg = calloc(1, sizeof * seg + (size + 1) * sizeof(word));
	assert(seg != NULL);

        seg->id = id;
        seg->len = size;

        return seg;
}

static inline word seg_id (T seg)
{
        assert(seg != NULL);

        return seg->id;
}

static inline word seg_get (T seg, word off)
{
        assert(seg != NULL);

        return seg->contents[off];
}

static inline word *seg_get_contents (T seg)
{
	assert(seg != NULL);

	return seg->contents;
}

static inline T seg_dup (T seg)
{
        assert(seg != NULL);

        T duplicated = seg_new(seg->len, 0);
        memcpy(duplicated->contents, seg->contents, seg->len * sizeof(word));

        return duplicated;
}

static inline void seg_set (T seg, word off, word val)
{
        assert(seg != NULL);
        seg->contents[off] = val;
}

static inline word seg_len (T seg)
{
        assert(seg != NULL);

        return seg->len;
}

static inline void seg_free (T *seg)
{
	assert(seg != NULL);

        free(*seg);
        *seg = NULL;
}

#undef T
#endif
