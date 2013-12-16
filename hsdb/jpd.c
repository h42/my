#include <stdio.h>
#include <unistd.h>
#include <malloc.h>
#include <fcntl.h>
#include "jdb.h"

typedef struct {
    DBPOS pos;
    char s[42000];
} ds;

#define N 5

void test(int x) {
    char *fn="temp.db";
    int i;
    DBPOS blk, blks[N];
    ds ds;

    if (x==1) jdb_init(fn);
    jdb *db = jdb_open(fn);

    for (i=0; i<N; i++) {
	blk = jdb_newblk(db);
	printf("new blk = %ld\n",blk);
        ds.pos=blk;
        sprintf(ds.s,"this is blk %d\n", i);
        jdb_putblk(db,&ds,blk);
    }

    jdb_putcblk(db);
    jdb_close(db);
}

int main() {
    test(1);
    return 0;
}

