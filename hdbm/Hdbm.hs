{-# LANGUAGE ForeignFunctionInterface #-}

module My.Hdbm (
    hdbmOpen
    ,hdbmClose
    ,hdbmInsert
    ,hdbmUpdate
    ,hdbmRemove
    ,hdbmGet
    ,hdbmFirst
    ,hdbmNext
    ,hdbmErrno
) where

import Control.Monad
import Foreign
import Foreign.C

foreign import ccall "hdbm.h hdbm_open" hdbm_open
    :: CString -> CString -> CInt -> IO (Ptr ())

foreign import ccall "hdbm.h hdbm_close" hdbmClose :: Ptr () -> IO ()

foreign import ccall "hdbm.h hdbm_insert" hdbm_insert
    :: Ptr () -> CString -> CInt -> CString -> CInt -> IO CInt

foreign import ccall "hdbm.h hdbm_update" hdbm_update
    :: Ptr () -> CString -> CInt -> CString -> CInt -> IO CInt

foreign import ccall "hdbm.h hdbm_remove" hdbm_remove
    :: Ptr () -> CString -> CInt -> IO CInt

foreign import ccall "hdbm.h hdbm_get" hdbm_get
    :: Ptr () -> CString -> CInt -> IO CString

foreign import ccall "hdbm.h hdbm_first" hdbm_first
    :: Ptr () -> IO CString

foreign import ccall "hdbm.h hdbm_next" hdbm_next
    :: Ptr () -> IO CString

foreign import ccall "hdbm.h hdbm_errno" hdbmErrno :: IO CInt
foreign import ccall "hdbm.h hdbm_sync" hdbmSync :: Ptr () -> IO ()
foreign import ccall "hdbm.h hdbm_reorg" hdbmReorg :: Ptr () -> IO CInt

hdbmOpen fn flags mode = do
    cfn <- newCString fn
    cflags <- newCString flags
    db <- hdbm_open cfn cflags (fromIntegral mode)
    free cflags
    free cfn
    when (db==nullPtr) (fail "open failed")
    return db

hdbmInsert db key value = do
    ckey <- newCString key
    cval <- newCString value
    rc <- hdbm_insert db ckey (fromIntegral $ length key + 1)
                         cval (fromIntegral $ length value + 1)
    free cval
    free ckey
    when (rc/=0) (fail "Insert failed")

hdbmUpdate db key value = do
    ckey <- newCString key
    cval <- newCString value
    rc <- hdbm_update db ckey (fromIntegral $ length key + 1)
                         cval (fromIntegral $ length value + 1)
    free cval
    free ckey
    when (rc/=0) (fail "Update failed")

hdbmRemove :: Ptr () -> String -> IO ()
hdbmRemove db key = do
    ckey <- newCString key
    rc <- hdbm_remove db ckey (fromIntegral $ length key + 1)
    free ckey
    when (rc/=0) (fail "Remove failed")

hdbmGet db key = do
    ckey <- newCString key
    rc <- hdbm_get db ckey (fromIntegral $ length key + 1)
    free ckey
    when (rc==nullPtr) (fail "Get failed")
    peekCString rc

hdbmFirst db = do
    rc <- hdbm_first db
    when (rc==nullPtr) (fail "First failed")
    peekCString rc

hdbmNext db = do
    rc <- hdbm_next db
    when (rc==nullPtr) (fail "Next failed")
    peekCString rc
