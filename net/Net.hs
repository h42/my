module Net (
    netGet
    ,netGetf
    ,server
    ,client
) where

import System.IO
import System.IO.Error
import Data.Char
import Control.Exception
import Control.Concurrent
import Network
import System.Posix
import System.Timeout
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BS

----------------------
-- NETGET
----------------------
-- netGet - return up to n bytes
netGet :: Handle -> Int -> Int -> IO (Int, B.ByteString)
netGet h n t = catchIOError (netGet2 h n t) (\x -> return (-1, B.empty))
netGet2 h n t = do
    -- hGetSome is much slower than using hWaitForInput -> hGetNonBlocking
    (flip catchIOError) (\e -> return (-1, B.empty)) $ do
	gotsome <- hWaitForInput h t  -- requires t in millisecs
	if gotsome then do
	    m <- B.hGetNonBlocking h n
	    return (B.length m, m)
	else return (0, B.empty)

-- netGetx - return exactly n bytes
netGetf :: Handle -> Int -> Int -> IO (Int, B.ByteString)
netGetf h n t = catchIOError (netGetf2 h n t) (\x -> return (-1, B.empty))
netGetf2 h n t =  do
    -- timeout like microsecs but we like millisecs
    (flip catchIOError) (\e -> return (-1, B.empty)) $ do
	td <- timeout (t*1000) (B.hGet h n)
	case td of
	    Nothing -> return (0, B.empty)
	    Just m -> return (B.length m, m)

----------------------
-- SERVER
----------------------
server :: PortNumber -> (Handle -> IO ()) -> IO ()
server pnum cproc = do
    bracket
	(listenOn $ PortNumber pnum)
	(sClose)
	(server2 cproc)

server2 cproc sock = do
    (h,n,p) <- accept sock
    --putStrLn $ "accepting"
    tid <- forkIO (server3 cproc h)
    server2 cproc sock

server3 cproc h = do
    hSetBuffering h NoBuffering
    cproc h
    hClose h

----------------------
-- Client
----------------------
client url port cproc = do
    h <- connectTo url (PortNumber port)
    hSetBuffering h NoBuffering
    cproc h
    hClose h

