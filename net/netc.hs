import System.IO
import System.Timeout
import Network
import Net
import qualified Data.ByteString.Char8 as BS

pnum=4247

doit 0 h = do
    BS.hPut h (BS.pack "quit")
    (rc, buf) <- netGet h 100 1000
    if rc > 0 then
	putStrLn $ (BS.unpack buf) ++ " " ++ show (BS.length buf)
    else print rc

doit n h = do
    BS.hPut h (BS.pack "more")
    (rc, buf) <- netGet h 100 1000
    doit (n-1) h

main = do
    client "127.0.0.1" pnum (doit 10)
    return ()
