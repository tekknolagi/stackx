#ifndef SEQ_H
#define SEQ_H

#include <inttypes.h>

struct Seq_T {
    uint32_t length, capacity;
    void **contents;
};

typedef struct Seq_T *Seq_T;

#define T Seq_T

T Seq_new (uint32_t hint);
void Seq_free (T *s);

void *Seq_get (T s, uint32_t id);
void Seq_put (T s, uint32_t id, void *seg);

void Seq_addhi (T s, void *seg);
void *Seq_remhi (T s);

uint32_t Seq_length (T s);


#undef T
#endif
