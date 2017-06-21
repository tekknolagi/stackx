#ifndef SEQ_H
#define SEQ_H

#include <inttypes.h>

#include "seg.h"

struct Seq_T {
	uint32_t length, capacity;
	Seg_T *contents;
};

typedef struct Seq_T *Seq_T;

#define T Seq_T

T Seq_new (uint32_t hint);
void Seq_free (T *s);

static Seg_T Seq_get (T s, uint32_t id);
static void Seq_put (T s, uint32_t id, Seg_T seg);

static void Seq_addhi (T s, Seg_T seg);
static Seg_T Seq_remhi (T s);

static uint32_t Seq_length (T s);

static inline Seg_T Seq_get (T s, uint32_t id)
{
	assert(s != NULL);
	assert(s->contents != NULL);

	return s->contents[id];
}

static inline void Seq_put (T s, uint32_t id, Seg_T seg)
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

static inline void Seq_addhi (T s, Seg_T seg)
{
	assert(s != NULL);
	assert(s->contents != NULL);

	if (s->length >= s->capacity) {
		resize(s, s->capacity * 2);
	}

	s->contents[s->length++] = seg;
}

static inline Seg_T Seq_remhi (T s)
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
