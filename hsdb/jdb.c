#include <stdio.h>
#include <unistd.h>
#include <malloc.h>
#include <fcntl.h>

//ccinclude
#ifndef _JDB_H
#define _JDB_H


#include <stdint.h>
#define DBPOS int64_t

typedef struct {
    int eye;
    int64_t top, free;
    int blksize;
} cblk ;

typedef struct {
    int fd;
    cblk cblk;
    void *buf0, *buf1;
} jdb;

int   jdb_init(const char *dbname);
jdb*  jdb_open(const char *dbname);
void  jdb_close(jdb *);
DBPOS jdb_newblk(jdb *t);
int   jdb_freeblk(jdb *t, DBPOS blk);
int   jdb_getblk(jdb *t, void *, DBPOS);
int   jdb_putblk(jdb *t, void *, DBPOS);
int   jdb_putcblk(jdb *t);
int   jdb_getcblk(jdb *t);

#endif
//ccinclude

#define EYE 1123
#define zfd t->fd
#define ztop t->cblk.top
#define zfree t->cblk.free
#define zblksize t->cblk.blksize
#define zcblk t->cblk
#define zbuf0 t->buf0
#define zbuf1 t->buf1
//#define t-> z

int jdb_putcblk(jdb *t) {
    int rc;
    if ((rc=lseek(zfd,0,0))<0) return rc;
    if ( (rc=write(zfd,&zcblk,sizeof(cblk))) != sizeof(cblk)) return -1;
    return 0;
}

int jdb_getcblk(jdb *t) {
    int rc;
    if ((rc=lseek(zfd,0,0))<0) return rc;
    if ( (rc=read(zfd,&zcblk,sizeof(cblk))) != sizeof(cblk)) return -1;
    return 0;
}

int jdb_putblk(jdb *t, void *buf, DBPOS pos) {
    int rc;
    if ((rc=lseek(zfd,pos,0))<0) return rc;
    if ( (rc=write(zfd, buf, zblksize)) != zblksize) return -1;
    return 0;
}

int jdb_getblk(jdb *t, void *buf, DBPOS pos) {
    int rc;
    if ((rc=lseek(zfd,pos,0))<0) return rc;
    if ( (rc=read(zfd, buf, zblksize)) != zblksize) return -1;
    return 0;
}

DBPOS jdb_newblk(jdb *t) {
    DBPOS blk;
    int rc;
    if (!zcblk.free) {
        blk=zcblk.top;
        zcblk.top += zcblk.blksize;
        if ( (rc=jdb_putcblk(t)) ) return 0;
        return blk;
    }

    //rc = jdb_getblk(jdb *t, zcblk.free);

    return 0;
}

int jdb_freeblk(jdb *t, DBPOS blk) {
    return 0;
}

void jdb_close(jdb *t) {
    close(zfd);
    free(zbuf0);
    free(zbuf1);
    free(t);
}

jdb * jdb_open(const char *dbname) {
    jdb *t=malloc(sizeof(jdb));
    zfd=open(dbname, O_RDWR,0640);
    if (zfd<0) return 0;
    int rc;
    if ((rc=jdb_getcblk(t))) return 0;
    if (zcblk.eye != EYE) return 0;    //throw("bad eyecatcher in cblk");
    zbuf0=malloc(zblksize);
    zbuf1=malloc(zblksize);
    return t;
}

int jdb_init(const char *dbname) {
    int fd,rc;
    remove(dbname);
    rc=access(dbname,F_OK);
    if (rc==0) return -1;
    fd=open(dbname,O_CREAT|O_WRONLY,0640);
    if (fd<0) return fd;
    cblk sk1;
    sk1.blksize=4096;
    sk1.top=sk1.blksize;
    sk1.free=0;
    sk1.eye=EYE;
    rc=write(fd,&sk1,sizeof(sk1));
    if (rc<0) return rc;
    printf("cblk size = %ld\n",sizeof(cblk));
    return 0;
}

