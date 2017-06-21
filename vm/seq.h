#ifndef SEQ_H
#define SEQ_H

#include <inttypes.h>

struct Seq_T {
    uint32_t length, capacity;
    void **contents;
};

typedef struct Seq_T *Seq_T;

#define T Seq_T

static void *Seq_get (T s, uint32_t id);
static void Seq_put (T s, uint32_t id, void *seg);

static void Seq_addhi (T s, void *seg);
static void *Seq_remhi (T s);

static uint32_t Seq_length (T s);

static inline void *Seq_get (T s, uint32_t id)
{
    assert(s != NULL);
    assert(s->contents != NULL);

    return s->contents[id];
}

static inline void Seq_put (T s, uint32_t id, void *seg)
{
    assert(s != NULL);
    assert(s->contents != NULL);

    s->contents[id] = seg;
}

static inline void resize (T s, uint32_t newsize) {
    assert(s != NULL);
    assert(s->contents != NULL);

    s->contents = realloc(s->contents, newsize * sizeof *s->contents);
    assert(s->contents != NULL);

    s->capacity = newsize;
}

static inline void Seq_addhi (T s, void *seg)
{
    assert(s != NULL);
    assert(s->contents != NULL);

    if (s->length >= s->capacity) {
        resize(s, s->capacity * 2);
    }

    s->contents[s->length++] = seg;
}

static inline void *Seq_remhi (T s)
{
    assert(s != NULL);
    assert(s->contents != NULL);

    return s->contents[--s->length];
}

static inline uint32_t Seq_length (T s)
{
    assert(s != NULL);

    return s->length;
}

#undef T
#endif
