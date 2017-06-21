#include <stdlib.h>
#include <assert.h>

#include "seq.h"

Seq_T Seq_new (uint32_t hint)
{
	Seq_T seq = calloc(1, sizeof *seq);
	assert(seq != NULL);

	seq->contents = calloc(hint, sizeof *seq->contents);
	assert(seq->contents != NULL);

	seq->length = 0;
	seq->capacity = hint;

	return seq;
}

void Seq_free (Seq_T *s)
{
	assert(s != NULL);

	free((*s)->contents);
	free(*s);

	*s = NULL;
}
