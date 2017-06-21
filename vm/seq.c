#include <stdlib.h>
#include <assert.h>

#include "seq.h"

#define T Seq_T

Seq_T Seq_new (uint32_t hint) {
    Seq_T seq = calloc(1, sizeof *seq);
    assert(seq != NULL);

    seq->contents = calloc(hint, sizeof *seq->contents);
    assert(seq->contents != NULL);

    seq->length = 0;
    seq->capacity = hint;

    return seq;
}

void Seq_free (Seq_T *s) {
    assert(s != NULL);

    free((*s)->contents);
    free(*s);

    *s = NULL;
}

inline void *Seq_get (T s, uint32_t id)
{
    assert(s != NULL);
    assert(s->contents != NULL);

    return s->contents[id];
}

inline void Seq_put (T s, uint32_t id, void *seg)
{
    assert(s != NULL);
    assert(s->contents != NULL);

    s->contents[id] = seg;
}

inline void resize (T s, uint32_t newsize) {
    assert(s != NULL);
    assert(s->contents != NULL);

    s->contents = realloc(s->contents, newsize * sizeof *s->contents);
    assert(s->contents != NULL);

    s->capacity = newsize;
}

inline void Seq_addhi (T s, void *seg)
{
    assert(s != NULL);
    assert(s->contents != NULL);

    if (s->length >= s->capacity) {
        resize(s, s->capacity * 2);
    }

    s->contents[s->length++] = seg;
}

inline void *Seq_remhi (T s)
{
    assert(s != NULL);
    assert(s->contents != NULL);

    return s->contents[--s->length];
}

inline uint32_t Seq_length (T s)
{
    assert(s != NULL);

    return s->length;
}

#undef T
