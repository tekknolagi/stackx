#ifndef MEMORY_H
#define MEMORY_H

#include <string.h>

#include "seq.h"
#include "word.h"

struct seg {
    word id;
    word len;
    word contents[];
};
typedef struct seg *Seg_T;

struct mem {
    Seq_T segs;
    Seq_T unmapped;
    word seg_inc;
};
typedef struct mem *Mem_T;

Seg_T seg_new (word size, word id);

#define T Mem_T

T mem_new (Seg_T seg0);
void mem_free (T *mem);

/* Returns a value at $m[seg][off]. */
static word mem_load (T mem, word seg, word off);
static void mem_store (T mem, word seg, word off, word val);

/* Make a duplicate segment and put it in $m[0]. */
static void mem_dup (T mem, word seg);

/* Returns a segment ID. */
word mem_map (T mem, word len);
void mem_unmap (T mem, word seg);

/* Returns a value at $m[seg][off]. */
static inline word mem_load (T mem, word segid, word off) {
    assert(mem != NULL);
    assert(mem->segs != NULL);

    return ((Seg_T) Seq_get(mem->segs, segid))->contents[off];
}

static inline void mem_store (T mem, word segid, word off, word val) {
    assert(mem != NULL);
    assert(mem->segs != NULL);

    ((Seg_T) Seq_get(mem->segs, segid))->contents[off] = val;
}

static inline void mem_dup (Mem_T mem, word segid) {
    assert(mem != NULL);

    /* Duplicate the segment. */
    Seg_T seg = Seq_get(mem->segs, segid);
    Seg_T duplicated = seg_new(seg->len, 0);
    memcpy(duplicated->contents, seg->contents, seg->len * sizeof(word));

    Seg_T seg0 = Seq_get(mem->segs, 0);
    free(seg0);

    Seq_put(mem->segs, 0, duplicated);
}

static inline word get_next_id (T mem) {
    assert(mem != NULL);
    assert(mem->unmapped != NULL);

    if (Seq_length(mem->unmapped) > 0) {
        return (word) (uint64_t) Seq_remhi(mem->unmapped);
    }
    else {
        return mem->seg_inc++;
    }
}

#undef T
#endif
