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
static T seg_dup (T seg);
static void seg_free (T *s);

static inline T seg_new (word size, word id) {
    /* calloc ensures every bit is 0 */
    /* TODO: figure out why it needs 4 extra bytes */
    T seg = calloc(1, sizeof * seg + (size + 1) * sizeof(word));
    assert(seg != NULL);

    seg->id = id;
    seg->len = size;

    return seg;
}

static inline T seg_dup (T seg) {
    assert(seg != NULL);

    T duplicated = seg_new(seg->len, 0);
    memcpy(duplicated->contents, seg->contents, seg->len * sizeof(word));

    return duplicated;
}

static inline void seg_free (T *seg) {
    assert(seg != NULL);

    free(*seg);
    *seg = NULL;
}

#undef T
#endif
