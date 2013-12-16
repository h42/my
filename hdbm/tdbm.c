#include <stdio.h>
#include <string.h>
#include "hdbm.h"

int main() {
    puts("hey");
    hdbm *db=hdbm_open("temp.db","n",0);
    printf("%p\n",db);

    int i,rc;
    char key[256],value[256];
    for (i=0;i<10000;i++) {
        sprintf(key,"key %d",i);
        sprintf(value,"value %d",i);
        rc=hdbm_insert(db, key, strlen(key)+1, value,strlen(value)+1);
        if (rc) {
            fprintf(stderr,"insert failed; i=%d\n",i);
            break;
        }
    }

    sprintf(key,"key %d",5);
    rc=hdbm_remove(db, key, strlen(key)+1);
    printf("remove rc = %d\n",rc);

    for (i=0;i<10;i++) {
        sprintf(key,"key %d",i);
        char *dptr=(char *)hdbm_get(db, key, strlen(key)+1);
        if (!dptr) {
            printf("record %d is missing\n",i);
            continue;
        }
        puts(dptr);
    }

    char *buf=(char *)hdbm_first(db);
    puts("");
    i=100;
    while (buf && ++i<110) {
        puts(buf);
        buf=(char *)hdbm_next(db);
    }

    hdbm_close(db);
    return 0;
}
