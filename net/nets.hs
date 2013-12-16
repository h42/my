import System.Environment
import qualified Data.ByteString.Char8 as B
import System.IO
import Control.Monad
import Net

portnum = 4247
msgmore = B.pack "more"
msgquit = B.pack "quit"

serverproc :: Handle -> IO ()
serverproc h = do
    --threadDelay 500000
    putStrLn "Server proc"
    doit
    putStrLn "Server proc out"
  where
    doit  = do
	(rc, buf) <- netGet h 100 2000
	if rc>0 then do
	    B.hPutStr h $ B.pack ("server get len = " ++ show rc)
	    when (buf == msgmore) doit
	else putStrLn "server -> no data "

main = do
    server portnum serverproc
    return ()

    {-
    let m = mkpacket (B.pack "This is a test") 0
    B.hPutStrLn stdout m
    let rc = ckpacket m 
    print rc
    let rc = ckpacket $ m `B.snoc` '1'
    print rc
    -}
