{-# LANGUAGE OverloadedStrings #-}

import System.Environment
import qualified Data.ByteString.Char8 as B
import System.IO
import System.Directory
import Control.Monad
import Net

portnum = 8080

notFound fn=("404 Not Found",
          "The requested URL " ++ fn ++ " was not found on this server.")
errmsg h msg = B.hPutStr h $ B.pack $ unlines
   ["HTTP/1.1 " ++ fst msg,
    --"Content-Length: 136",
    "Connection: close",
    "Content-Type: text/html\n",
    "<html><head>",
    "<title>" ++ fst msg ++ "</title>",
    "</head>",
    "<body>",
    "<h1>" ++ fst msg ++"</h1>",
    snd msg,
    "</body>","</html>"]

getStatic h fn = do
    let fn' = if fn == "/" then "www/index.html" else ("www" ++ fn)
    e <- doesFileExist fn'
    if e then do
        f <- B.readFile fn'
        B.hPut h "HTTP/1.1 200 OK\nConnection: close\nContent-Type: text/html\n\n"
        B.hPut h f
    else errmsg h $ notFound fn'

serverproc :: Handle -> IO ()
serverproc h = do
    (rc, buf) <- netGet h 100 2000
    when (rc>0) $ do
        if B.isPrefixOf "GET" buf then do
            case (words $ head $ lines $ B.unpack buf) of
                (_:fn:_) -> do
                    getStatic h fn
                _  -> return ()
        else return ()

main = server portnum serverproc
