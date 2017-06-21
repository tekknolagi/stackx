#ifndef MEMORY_H
#define MEMORY_H

#include "seq.h"

#include "word.h"
#include "seg.h"

struct mem {
	Seq_T segs;
        Seq_T unmapped;
        word seg_inc;
};

typedef struct mem *Mem_T;

#define T Mem_T

T mem_new (Seg_T seg0);
void mem_free (T *mem);

static Seg_T mem_seg (T mem, word seg);

/* Returns a value at $m[seg][off]. */
static word mem_load (T mem, word seg, word off);
static void mem_store (T mem, word seg, word off, word val);

/* Make a duplicate segment and put it in $m[0]. */
static void mem_dup (T mem, word seg);

/* Returns a segment ID. */
word mem_map (T mem, word len);
void mem_unmap (T mem, word seg);

static inline Seg_T memtable_get (Seq_T segs, word id)
{
	return Seq_get(segs, id);
}

static inline void memtable_set (Seq_T segs, word id, Seg_T seg)
{
	Seq_put(segs, id, seg);
}

static inline void memtable_rem (Seq_T segs, word id)
{
	Seq_put(segs, id, NULL);
}

static inline Seg_T mem_seg (T mem, word segid)
{
	assert(mem != NULL);
	assert(mem->segs != NULL);

	return memtable_get(mem->segs, segid);
}

/* Returns a value at $m[seg][off]. */
static inline word mem_load (T mem, word segid, word off)
{
        assert(mem != NULL);
        assert(mem->segs != NULL);

        Seg_T seg = memtable_get(mem->segs, segid);

        return seg_get(seg, off);
}

static inline void mem_store (T mem, word segid, word off, word val)
{
        assert(mem != NULL);
        assert(mem->segs != NULL);

        Seg_T seg = memtable_get(mem->segs, segid);

        seg_set(seg, off, val);
}

static inline void mem_dup (Mem_T mem, word segid)
{
        assert(mem != NULL);

        Seg_T duplicated = seg_dup(memtable_get(mem->segs, segid));
        Seg_T seg0 = memtable_get(mem->segs, 0);
        seg_free(&seg0);

        memtable_set(mem->segs, 0, duplicated);
}

static inline word get_next_id (T mem)
{
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
