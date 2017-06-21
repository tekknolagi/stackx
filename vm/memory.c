#include <stdlib.h>
#include <assert.h>

#include "seq.h"

#include "memory.h"
#include "seg.h"

static const int TABLE_RANDOM_HINT = 100000;

Mem_T mem_new (Seg_T seg0)
{
        assert(seg0 != NULL);

        Mem_T mem = calloc(1, sizeof *mem);
	assert(mem != NULL);

	mem->segs = Seq_new(TABLE_RANDOM_HINT);
        mem->unmapped = Seq_new(TABLE_RANDOM_HINT);

	Seq_addhi(mem->segs, seg0);

        mem->seg_inc = 1;

        return mem;
}

void mem_free (Mem_T *mem)
{
        assert(*mem != NULL);
        assert((*mem)->segs != NULL);
        assert((*mem)->unmapped != NULL);

	Seq_T segs = (*mem)->segs;
	word len = Seq_length(segs);

	/* free each segment */
	for (word i = 0; i < len; i++) {
		Seg_T seg = Seq_get(segs, i);
		seg_free(&seg);
	}

	/* free the segment "table" */
	Seq_free(&segs);

        /* free the unmapped segment stack */
        Seq_free(&(*mem)->unmapped);

        free(*mem);

        *mem = NULL;
}

/* Returns a segment ID. */
word mem_map (Mem_T mem, word len)
{
        assert(mem != NULL);
        assert(mem->segs != NULL);
        assert(mem->unmapped != NULL);

        word id = get_next_id(mem);
        Seg_T seg = seg_new(len, id);

	if (id >= (word) Seq_length(mem->segs)) {
		Seq_addhi(mem->segs, seg);
	}
	else {
		memtable_set(mem->segs, id, seg);
	}

        return id;
}

void mem_unmap (Mem_T mem, word segid)
{
        assert(mem != NULL);
        assert(mem->segs != NULL);
        assert(mem->unmapped != NULL);

        Seg_T seg = memtable_get(mem->segs, segid);
        memtable_rem(mem->segs, segid);
        seg_free(&seg);

        /*
          We know what we're doing... hopefully.  This nifty bit of
          casting allows us to make a pointer from a segment ID.
        */
        Seq_addhi(mem->unmapped, (void *) (uint64_t) segid);
}
