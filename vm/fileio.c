#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <assert.h>

#include "fileio.h"

Seg_T fileio_read (char *fn)
{
        assert(fn != NULL);

        FILE *fp = fopen(fn, "rb");

        if (fp == NULL) {
                /* Could not open the file for reading. */
                fprintf(stderr, "Could not open file for reading.\n");
                return NULL;
        }

        struct stat fp_info;

        if (stat(fn, &fp_info) != 0) {
                /* Something really weird happened. */
                return NULL;
        }

        Seg_T prog = seg_new(fp_info.st_size / sizeof(word), 0);
        word ind = 0;

        while (feof(fp) == 0) {
                /* Four byte array. */
                unsigned char current_word_chars[4];

                /* Read the bytes in backwards. Because endian-ness. */
                fread(&current_word_chars[3], 1, 1, fp);
                fread(&current_word_chars[2], 1, 1, fp);
                fread(&current_word_chars[1], 1, 1, fp);
                fread(&current_word_chars[0], 1, 1, fp);

                /* Since arrays are contiguous, we can interpret the
                   four-byte char array as one 32-bit word. */
                word current_word = *(word *) current_word_chars;
                seg_set(prog, ind++, current_word);
        }

        fclose(fp);

        return prog;
}
