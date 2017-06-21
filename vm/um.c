#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

#include "machine.h"

Seg_T fileio_read (char *fn);

int main (int argc, char **argv)
{
    (void)argc;

    char *fn = argv[1];
    FILE *fp = fopen(fn, "rb");
    assert(fp != NULL);

    struct stat fp_info;
    int success = stat(fn, &fp_info);
    assert(success == 0);

    Seg_T prog = seg_new(fp_info.st_size / sizeof(word), 0);
    word ind = 0;

    while (feof(fp) == 0) {
        /* Four byte array. */
        unsigned char current_word_chars[4];

        /* Read the bytes in backwards. Because endian-ness. */
        int nread = 0;
        nread += fread(&current_word_chars[3], 1, 1, fp);
        nread += fread(&current_word_chars[2], 1, 1, fp);
        nread += fread(&current_word_chars[1], 1, 1, fp);
        nread += fread(&current_word_chars[0], 1, 1, fp);
        assert(nread == 4);

        /* Since arrays are contiguous, we can interpret the
           four-byte char array as one 32-bit word. */
        word current_word = *(word *) current_word_chars;
        prog->contents[ind++] = current_word;
    }

    fclose(fp);

    Machine_T m = machine_new();
    machine_load(m, prog);
    machine_run(m);
    machine_free(&m);

    return 0;
}
