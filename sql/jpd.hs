{-# LANGUAGE BangPatterns #-}

import System.IO.Error
import My.Sql

main = do
    db <- sql_open "temp.db"

    sql_exec db "begin;"

    tryIOError (sql_exec db "drop table t1;")

    sql_exec db "create table t1 ( i1 int, s1 text);"

    stmt <- sql_prepare db "insert into t1 values (?,?)"

    let looper 0 = return ()
	looper !n = do
	    sql_bind stmt [Sqlint 17,Sqltext "hey"]
	    sql_step stmt
	    sql_reset stmt
	    looper (n-1)

    looper 100000

    sql_finalize stmt

    sql_exec db "commit;"

    stmt2 <- sql_prepare db "select i1,s1 from t1;"
    rc <- sql_step stmt2
    print rc
    [Sqlint i2, Sqltext s2] <- sql_columns stmt2
    print (i2,s2)
    sql_finalize stmt2

    sql_close db
