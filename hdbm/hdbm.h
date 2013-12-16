#ifndef _GDBM_H
#include <gdbm.h>
#endif

typedef struct {
    GDBM_FILE zdb;
    datum zkey,zvalue;
    int zerrno;
} hdbm;

hdbm * hdbm_open(char *fn, char *cflags, int mode);
void hdbm_close(hdbm *db);
int hdbm_insert(hdbm *db, void *key, int klen, void *value, int vlen);
int hdbm_update(hdbm *db, void *key, int klen, void *value, int vlen);
void * hdbm_get(hdbm *db, void *key, int klen);
int    hdbm_remove(hdbm *db,void *key, int klen);
void * hdbm_first(hdbm *db);
void * hdbm_next(hdbm *db);
int    hdbm_errno();
void   hdbm_sync(hdbm *db);
int    hdbm_reorg(hdbm *db);

