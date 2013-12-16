{-# LANGUAGE ForeignFunctionInterface #-}

module My.Sql (
    sql_open
   ,sql_close
   ,sql_exec
   ,sql_prepare
   ,sql_step
   ,sql_reset
   ,sql_bind
   ,sql_columns
   ,sql_finalize
   ,sql_errmsg
   ,SqlType (..)
) where

import Control.Monad
import Foreign
import Foreign.C

foreign import ccall "sqlite3.h sqlite3_open" sqlite3_open
    :: CString -> Ptr (Ptr a) -> IO CInt
foreign import ccall "sqlite3.h sqlite3_close" sql_close :: Ptr a -> IO ()
foreign import ccall "sqlite3.h sqlite3_exec" sqlite3_exec
    :: Ptr a -> CString -> CInt -> CInt -> Ptr (CString) -> IO CInt
foreign import ccall "sqlite3.h sqlite3_free" sql_free :: Ptr a -> IO ()
foreign import ccall "sqlite3.h sqlite3_prepare_v2" sqlite3_prepare_v2
    :: Ptr a -> CString -> CInt -> Ptr (CStmt) -> (Ptr a) -> IO CInt
foreign import ccall "sqlite3.h sqlite3_finalize" sqlite3_finalize
    :: Ptr a -> IO CInt
foreign import ccall "sqlite3.h sqlite3_bind_int64" sqlite3_bind_int64
    :: Ptr a -> CInt -> CLong -> IO CInt
foreign import ccall "sqlite3.h sqlite3_bind_int" sqlite3_bind_int
    :: Ptr a -> CInt -> CLong -> IO CInt
foreign import ccall "sqlite3.h sqlite3_bind_text" sqlite3_bind_text
    :: Ptr a -> CInt -> CString -> CInt -> CLong -> IO CInt

foreign import ccall "sqlite3.h sqlite3_reset" sqlite3_reset :: Ptr a -> IO CInt
foreign import ccall "sqlite3.h sqlite3_step" sqlite3_step :: Ptr a -> IO CInt
foreign import ccall "sqlite3.h sqlite3_column_int64" sqlite3_column_int64
    :: Ptr a -> CInt -> IO CLong
foreign import ccall "sqlite3.h sqlite3_column_text" sqlite3_column_text
    :: Ptr a -> CInt -> IO CString
foreign import ccall "sqlite3.h sqlite3_column_count" sql_column_count
    :: CStmt -> IO CInt
foreign import ccall "sqlite3.h sqlite3_column_type" sql_column_type
    :: CStmt -> CInt -> IO CInt
foreign import ccall "sqlite3.h sqlite3_errmsg" sqlite3_errmsg
    :: Ptr a -> IO CString

type CStmt = Ptr [CString]

data SqlType = Sqlint Int | Sqlint64 Integer | Sqltext String | Sqlbad

sql_open :: String -> IO (Ptr a)
sql_open s = do
    cs <- newCString s
    db <- malloc :: IO (Ptr (Ptr a))
    rc <- sqlite3_open cs db
    db2 <- peek db -- dereference **
    free cs
    free db
    when (rc /= 0) (do
	sql_close db2
	fail "open failed")
    return db2

sql_exec :: Ptr a -> String -> IO ()
sql_exec db sql = do
    perr <- malloc :: IO (Ptr (CString))
    csql <- newCString sql
    rc <- sqlite3_exec db csql 0 0 perr
    crc <- if rc /= 0
	then peek perr >>= peekCString    -- >>= print
	else return ""
    free csql
    peek perr >>= sql_free
    free perr
    when (rc/=0) (fail crc)

sql_prepare :: Ptr a -> String -> IO CStmt
sql_prepare db sql = do
    csql <- newCString sql
    pstmt <- malloc :: IO (Ptr CStmt)
    rc <- sqlite3_prepare_v2 db csql (fromIntegral $ length sql) pstmt nullPtr
    stmt <- peek pstmt -- :: IO (CStmt)
    free csql
    free pstmt
    when (rc/=0) $ do
	s <- sql_errmsg db
	fail s
    return stmt

sql_finalize  stmt = do
    rc <- sqlite3_finalize stmt
    when (rc/=0) $ fail "finalize failed"

sql_reset  stmt = do
    rc <- sqlite3_reset stmt
    when (rc/=0) $ fail "reset failed"

sql_step  stmt = do
    rc <- sqlite3_step stmt
    when (rc/=100 && rc /=101) $ fail "step failed"
    return $ fromIntegral rc

--
-- COLUMNS
--
sql_columns :: CStmt -> IO [SqlType]
sql_columns stmt = do
    cnt <- sql_column_count stmt
    if cnt>0 then sql_cols stmt (cnt-1) []
             else return []

sql_cols :: CStmt -> CInt -> [SqlType] -> IO [SqlType]
sql_cols stmt n ts
    | n < 0 = return ts
    | otherwise = do
	sqltype <- sql_column_type stmt n
	sql_cols2 stmt n ts sqltype

sql_cols2 stmt n ts sqltype
    | sqltype == 1 = do
	i<-sql_column_int64 stmt n
	sql_cols stmt (n-1) (Sqlint i:ts)
    | sqltype == 3 = do
	s<-sql_column_text stmt n
	sql_cols stmt (n-1) (Sqltext s:ts)
    | otherwise = sql_cols stmt (n-1) (Sqlbad:ts)

sql_column_int64 stmt col =
    fmap fromIntegral $ sqlite3_column_int64 stmt (fromIntegral col)

sql_column_text stmt col =
    sqlite3_column_text stmt (fromIntegral col) >>= peekCString

--
-- BIND
--
sql_bind :: CStmt -> [SqlType] -> IO Int
sql_bind stmt ts = sql_bind2 ts 1 stmt

sql_bind2 [] _ _ = return 0
sql_bind2 (Sqlint i:xs) col stmt = do
    sql_bind_int64 stmt col i
    sql_bind2 xs (col+1) stmt
sql_bind2 (Sqltext s:xs) col stmt = do
    sql_bind_text stmt col s
    sql_bind2 xs (col+1) stmt


sql_bind_int64 stmt col val =
    fmap fromIntegral $ sqlite3_bind_int64 stmt (fromIntegral col) (fromIntegral val)
sql_bind_int stmt col val =
    fmap fromIntegral $ sqlite3_bind_int stmt col val
sql_bind_text stmt col val = do
    rc <- withCString val (\cval ->
	sqlite3_bind_text stmt (fromIntegral col) cval (-1) (-1) )
    return $ fromIntegral rc

--
-- ERRMSG
--
sql_errmsg :: Ptr a -> IO String
sql_errmsg db = sqlite3_errmsg db >>= peekCString
