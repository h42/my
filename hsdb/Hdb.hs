{-# LANGUAGE TemplateHaskell #-}

module Hdb (
    Hdb(..)
    ,openHdb
    ,closeHdb
    ,initHdb
--    ,getblk
--    ,putblk
) where

import Control.Lens
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State

import System.IO
import System.IO.Error
import System.Directory
import Foreign
import Foreign.C
--import Text.Printf

foreign import ccall "string.h strlen" strlen :: Ptr a -> IO CInt
foreign import ccall "open" c_open :: CString -> CInt -> IO CInt
foreign import ccall "fchmod" c_fchmod :: CInt -> CUInt -> IO CInt
foreign import ccall "close" c_close :: CInt -> IO CInt
foreign import ccall "read" c_read :: CInt -> CString -> CLong -> IO CLong
foreign import ccall "write" c_write :: CInt -> CString -> CLong -> IO CLong
foreign import ccall "lseek" c_lseek :: CInt -> CLong -> CInt -> IO CLong
foreign import ccall "fdatasync" c_fdatasync :: CInt -> IO CInt

o_rdonly = 0
o_wronly = 1
o_rdwr = 2
o_creat = 64
o_trunc = 512

h_read fd str len = do
    cstr <- mallocBytes len :: IO CString
    rc <- c_read fd cstr (fromIntegral len)
    cbuf <- peekCString cstr
    free cstr
    return (rc,cbuf)

data Cblk = Cblk {
    ztop :: Int
    ,zfree :: Int
    ,zblksize :: Int
} deriving (Show, Read)
cblk0 = Cblk 4096 0 4096

data Hdb = Hdb {
    zblk :: Cblk
    ,zfd  :: CInt
    ,zfn  :: String
    ,zbuf :: CString
    ,zstr :: String
} --deriving (Show)
hdb0 = Hdb cblk0 undefined undefined undefined ""

instance Show Hdb where
  show h =
    "fd = " ++ show (zfd h) ++ " fn = " ++ show (zfn h)
    ++ " " ++ show (zblk h)

type Hdbkey = String
type Hdbdata = String

{-
newblk :: Hdb -> IO (Int, Hdb)
newblk h
    | blk == 0 = do
        let c = (zblk h){ztop=(ztop c) + (zblksize c)}
        putblk 0 (show c) h
        return (ztop $ zblk $ h, h{zblk=c})
    | otherwise = do
        h' <- getblk (fromIntegral blk) h
        let blk2 = read (zstr h')
            c=(zblk h'){zfree=blk2}
        return (blk2, h'{zblk=c})
  where blk = (zfree $ zblk $ h)

-- freeblk ::

getblk :: CLong -> Hdb -> IO Hdb
getblk pos h = do
    rc <- c_lseek (zfd h) pos 0
    when (rc < 0) (fail "getblk: lseek failed")
    rc' <- c_read (zfd h) (zbuf h) (fromIntegral $ zblksize $ zblk h)
    when (rc'<=0) (fail $ "getblk: read <= 0 - rc'=" ++ show rc')
    cbuf <- peekCString (zbuf h)
    return h{zstr=cbuf}

putblk :: CLong -> String -> Hdb -> IO Hdb
putblk pos str h = do
    rc <- c_lseek (zfd h) pos 0
    when (rc < 0) (fail "putblk: lseek failed")
    withCString str $ \cbuf -> do
	len <- fmap (+1) (strlen cbuf)
	rc' <- c_write (zfd h) cbuf  (fromIntegral len)
        when (rc'<=0) (fail $ "putblk: write <= 0 - rc'=" ++ show rc')
        return h{zstr=str}
-}

openHdb :: String -> StateT Hdb IO ()
openHdb fn = do
    fd <- lift $ withCString fn $ \cfn -> c_open cfn o_rdwr
    when (fd < 0) (fail "openHdb: open failed")
    cstr <- lift (mallocBytes 4096 :: IO CString)
    rc <- lift $ c_read fd cstr (4096)
    if (rc>0) then do
        cbuf <- lift $ peekCString cstr
	let cblk = read cbuf :: Cblk
        put hdb0{zfd=fd, zfn=fn, zblk=cblk, zbuf=cstr}
    else do
        lift $ free cstr
        fail "openHdb: read Cblk failed"

closeHdb:: StateT Hdb IO CInt
closeHdb = do
    hdb <- get
    lift $ c_close $ zfd hdb

initHdb fn = do
    let hdb = hdb0 {zblk=cblk0, zfd=(-1), zfn=fn}
    cfn <- newCString fn
    cbuf <- newCString $ show cblk0
    fd <- c_open cfn (o_wronly+o_trunc+o_creat)
    when (fd>=0) $ do
        c_fchmod fd 0o0640
        len <- fmap (+1) (strlen cbuf)
        rc <- c_write fd cbuf  (fromIntegral len)
        c_close fd
        return ()
    free cbuf
    free cfn
    return $ hdb{zfd=fd}

getpos h pos = pos * (zblksize $ zblk $ h)

