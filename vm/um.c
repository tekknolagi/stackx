#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#include "fileio.h"
#include "machine.h"

int run_file (char *fn);

int main (int argc, char **argv)
{
        (void)argc;
        return run_file(argv[1]);
}

int run_file (char *fn)
{
        Seg_T prog = fileio_read(fn);

        Machine_T m = machine_new();
        machine_load(m, prog);
        machine_run(m);
        machine_free(&m);

        return 0;
}
