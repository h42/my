#include <malloc.h>
#include <gdbm.h>

//ccinclude
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

//ccinclude

#define safefree(d) {if (d.dptr) {free(d.dptr); d.dptr=0;}}

hdbm * hdbm_open(char *fn, char *cflags, int mode) {
    int flags;
    if (mode==0) mode=0664;
    if (cflags[0]=='r') flags=GDBM_READER;
    else if (cflags[0]=='w') flags=GDBM_WRITER;
    else if (cflags[0]=='n') flags=GDBM_NEWDB;  //always new
    else if (cflags[0]=='c') flags=GDBM_WRCREAT;  //creat if necessary
    if (cflags[1]=='s') flags |= GDBM_SYNC;
    hdbm *hdbm1 = (hdbm *)malloc(sizeof(hdbm));
    hdbm1->zkey.dptr = hdbm1->zvalue.dptr = 0;
    hdbm1->zdb = gdbm_open(fn, 1024, flags, mode, 0);
    return hdbm1;
}

void hdbm_close(hdbm *db) {
    gdbm_close(db->zdb);
}

int hdbm_insert(hdbm *db, void *key, int klen, void *value, int vlen) {
    datum k,v;
    k.dptr=(char *)key;
    k.dsize=klen;
    v.dptr=(char *) value;
    v.dsize=vlen;
    //returns +1 instead of -1 for existing record
    return gdbm_store(db->zdb, k, v, GDBM_INSERT);
}

int hdbm_update(hdbm *db, void *key, int klen, void *value, int vlen) {
    datum k,v;
    k.dptr=(char *)key;
    k.dsize=klen;
    v.dptr=(char *) value;
    v.dsize=vlen;
    return gdbm_store(db->zdb, k, v, GDBM_REPLACE);
}

void * hdbm_get(hdbm *db, void *key, int klen) {
    datum k;
    if (key==0) {
        key = db->zkey.dptr;
        klen = db->zkey.dsize;
    }
    k.dptr=(char *)key;
    k.dsize=klen;
    safefree(db->zvalue);
    db->zvalue=gdbm_fetch (db->zdb, k);
    return db->zvalue.dptr;
}

int hdbm_remove(hdbm *db,void *key, int klen) {
    datum k;
    k.dptr=(char *)key;
    k.dsize=klen;
    return gdbm_delete (db->zdb, k);
}

void * hdbm_first(hdbm *db) {
    safefree(db->zkey);
    db->zkey = gdbm_firstkey(db->zdb);
    if (!db->zkey.dptr) return 0;
    return hdbm_get(db,0,0);
}

void * hdbm_next(hdbm *db) {
    void * d = db->zkey.dptr;
    if (!d) return 0;
    db->zkey = gdbm_nextkey(db->zdb,db->zkey);
    free(d);
    if (!db->zkey.dptr) return 0;
    return hdbm_get(db,0,0);
}

int hdbm_errno() {
    return gdbm_errno;
}

void hdbm_sync(hdbm *db) {
    gdbm_sync(db->zdb);
}

int hdbm_reorg(hdbm *db) {
    return gdbm_reorganize(db->zdb);
}

