{-# LANGUAGE DoAndIfThenElse #-}
module My.Ftp (
    Ftp (..)
    ,cmd_cd
    ,cmd_chmod
    ,cmd_connect
    ,cmd_pass
    ,cmd_user
    ,cmd_get
    ,cmd_help
    ,cmd_nlst
    ,cmd_list
    ,cmd_pasv
    ,cmd_put
    ,cmd_pwd
    ,cmd_quit
    ,cmd_rm
    ,cmd_rmdir
    ,cmd_mkdir
    ,cmd_type
) where

import System.IO
import System.IO.Error
import System.Environment
import Data.Char
import Data.List
import qualified Data.ByteString.Char8 as B
import Control.Concurrent
import Control.Monad
import Network

--------------------------
-- GLOBAL
--------------------------
data Ftp = Ftp {
    zh          :: Handle
    ,zh2        :: Handle
    ,zh2addr    :: String
    ,zh2port    :: Int
    ,zrc        :: Int
    ,zresp      :: Int
    ,zbuf       :: String
    ,zdata      :: String -- return get_data in string
    ,zloud      :: Bool
}

initFtp :: Ftp
initFtp = Ftp{zh=stderr,zh2=stderr,zh2addr="",zh2port=0,
		    zrc=0,zresp=0,zbuf="",zdata="",zloud=True}

putcmd cmd g = if zrc g < 0 then return g else putcmd2 cmd g
putcmd2 cmd g = do
    catchIOError
	(do 
	    B.hPutStr (zh g) $ B.pack $ cmd ++ "\r\n"
	    return g)
	(\e -> return g{zrc= -1})

--------------------------
-- GET_RESP
--------------------------
get_resp :: Ftp -> IO Ftp
get_resp g 
    | zrc g < 0 = return g
    | otherwise = catchIOError (get_resp2 g) (\e -> return g{zrc= -1})

get_resp2 g = do
    s <- fmap B.unpack (B.hGetLine (zh g))
    when (zloud g) (putStrLn s)
    let rc = if (length s > 5 && s!!3 == ' ') then chk_resp (take 3 s) else -1
    if rc > 0 then return g{zrc=rc, zresp=rc, zbuf=s}
              else get_resp g

chk_resp :: String -> Int
chk_resp s =
    if all isDigit s
	then (read s :: Int)
	else -1

get_resp_rc rc g = do
    g' <- get_resp g
    return g'{zrc=if (zrc g') == rc then 0 else -1}

--------------------------
-- put_data
--------------------------
put_data s fn g = do
    g' <- cmd_pasv g
    if zrc g' == 0 then put_data2 s fn g'  else return g'

put_data2 s fn g = do
    g' <- cmd_type "I" g
    if zrc g' == 0  then put_data3 s fn g'  else return g'

put_data3 s fn g = do 
    m <- newEmptyMVar
    tid <- forkIO (get_conn m g)
    g' <- putcmd  ("Stor " ++ fn) g >>= get_resp
    if zrc g' == 150  then put_data4 m s g'  else return g'{zrc= -1}

put_data4 m buf g = do 
    h2 <- takeMVar m
    B.hPutStr h2 buf
    hClose h2
    g' <- get_resp g
    if zrc g' == 226 then  return g'{zrc=0}  else return g'{zrc= -1}

--------------------------
-- get_data
--------------------------
get_data cmd fn g = do
    g' <- cmd_pasv g
    if zrc g' == 0 then get_data2 cmd fn g'
                   else return g'

get_conn mv g = do
    when (zloud g)
      (putStrLn $ "connecting to " ++ zh2addr g ++ " / " ++ show (zh2port g))
    h2 <- connectTo (zh2addr g) (PortNumber (fromIntegral (zh2port g)))
    putMVar mv h2

get_data2 s fn g = do
    g' <- cmd_type "I" g
    if zrc g' == 0  then get_data3 s fn g'
                    else return g'

get_data3 cmd fn g = do
    m <- newEmptyMVar
    tid <- forkIO (get_conn m g)
    g' <- putcmd cmd g >>= get_resp
    case zrc g' of
	150 -> do
	    h2 <- takeMVar m
	    get_data4 h2 fn g'
	_ -> do
	    putStrLn $ "resp = " ++ show (zrc g')
	    return g'{zrc= -1}

get_data4 h2 fn g = do
    s <- B.hGetContents h2
    hClose h2
    g' <- get_resp g
    case zrc g' of
	226 -> do
                 if fn == "[]" then return ()
                 else if fn /= "" then B.writeFile fn s
                 else putStrLn $ B.unpack s
		 return g'{zrc=0,zdata=if fn=="[]" then B.unpack s else ""}
	_ -> return g'{zrc= -1}


--------------------- API -----------------------

--------------------------
-- CMD_CD
--------------------------
cmd_cd parm g = putcmd  ("CWD " ++ parm) g >>= get_resp_rc 250

--------------------------
-- CMD_CHMOD
--------------------------
cmd_chmod parm g = do
    -- parm = "770 fn"
    g' <- putcmd  ("SITE CHMOD " ++ parm) g >>= get_resp
    return g'{zrc=if (zrc g') == 200 then 0 else -1}

--------------------------
-- CMD_CONNECT
--------------------------
cmd_connect :: String -> String -> String -> IO Ftp
cmd_connect host user pw = do
    h <- connectTo host (PortNumber 21)
    hSetBuffering h NoBuffering
    let g = initFtp
    g' <- get_resp g{zh=h}
    if (zrc g') == 220 then cmd_user user pw g'
                       else return g

cmd_pass pw g= do
    g' <- putcmd  ("PASS " ++ pw) g >>= get_resp
    case zrc g' of
	230 -> return g'{zrc=0}
	_   -> return g'{zrc= -1}

cmd_user user pw g = do
    g' <- putcmd  ("USER " ++ user) g >>= get_resp
    case zrc g' of
	230 -> return g'{zrc=0}
	331 -> cmd_pass pw g'
	_   -> return g{zrc= -1}

--------------------------
-- CMD_GET
--------------------------
cmd_get src dst g = get_data ("RETR " ++ src) dst  g

--------------------------
-- CMD_HELP
--------------------------
cmd_help g = putcmd  "HELP " g >>= get_resp_rc 214

--------------------------
-- CMD_LIST
--------------------------
cmd_nlst parm fn g = get_data ("NLST " ++ parm) fn g

cmd_list parm fn g = get_data ("LIST " ++ parm) fn g

--------------------------
-- CMD_MKDIR
--------------------------
cmd_mkdir parm g = putcmd  ("MKD " ++ parm) g >>= get_resp_rc 257

--------------------------
-- CMD_PASSIVE
--------------------------
cmd_pasv g = do
    g' <- putcmd  "PASV " g >>= get_resp
    if zresp g' == 227  then cmd_pasv2 g'
                        else return g'{zrc= -1}

cmd_pasv2 g = do
    let p1 = elemIndices '(' (zbuf g)
	p2 = elemIndices ')' (zbuf g)
    if null p1 || null p2 || last p1 > last p2 then return g{zrc= -1}
    else do
	let m =  split ',' $ (drop (last p1 + 1)) (take (last p2) (zbuf g))
	if length m /= 6 then return g{zrc= -1}
	else do
	    let addr = m!!0 ++ "." ++ m!!1 ++ "." ++ m!!2 ++ "." ++ m!!3
		port = (read (m!!4) :: Int) * 256  + (read (m!!5) :: Int) 
	    return g{zrc=0,zh2addr=addr,zh2port=port}

split sep s = split0 [] [] (reverse s) where
    split0 nl pl [] = pl:nl
    split0 nl pl (x:xs) =
	if (x==sep) then split0  (pl:nl) [] xs
	else split0 nl (x:pl) xs

--------------------------
-- CMD_PWD
--------------------------
cmd_pwd g = putcmd  "PWD " g >>= get_resp_rc 257

--------------------------
-- CMD_QUIT
--------------------------
cmd_quit g = putcmd  "QUIT " g >>= get_resp_rc 221

--------------------------
-- CMD_RM
--------------------------
cmd_rm parm g = do
    let cmd = ("DELE " ++ parm)
    putcmd cmd g >>= get_resp_rc 250

--------------------------
-- CMD_RMDIR
--------------------------
cmd_rmdir parm g = putcmd  ("RMD " ++ parm) g >>= get_resp_rc 250

--------------------------
-- CMD_TYPE
--------------------------
cmd_type parm g = putcmd  ("TYPE " ++ parm) g >>= get_resp_rc 200

--------------------------
-- CMD_PUT
--------------------------
cmd_put ifn ofn g = do
    catchIOError
	( do
	      s <- B.readFile ifn
	      put_data s ofn g
	)
	(\e -> return g{zrc= -1} )
    return g
