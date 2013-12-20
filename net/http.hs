{-# LANGUAGE OverloadedStrings #-}

import Data.Char
import Data.List
import System.Environment
import qualified Data.ByteString.Char8 as B
import System.IO
import System.Directory
import System.Process
import Control.Monad
import Net

portnum = 8080
root = "/www"

notFound fn=("404 Not Found",
          "The requested URL " ++ fn ++ " was not found on this server.")
forbidden fn=("403 Forbidden",
          "The requested URL, file type or operation is not allowed/supported")
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

extensions = [("gif", "image/gif" ),
        ("jpg", "image/jpg" ),
        ("jpeg","image/jpeg"),
        ("png", "image/png" ),
        ("ico", "image/ico" ),
        ("zip", "image/zip" ),
        ("gz",  "image/gz"  ),
        ("tar", "image/tar" ),
        ("htm", "text/html" ),
        ("/", "text/html" ),  -- for index.html
        ("html","text/html" ) ] :: [(String,String)]

getMimeType fn = if null xs then [] else (snd.head) xs  where
    xs = filter (\x->isSuffixOf (fst x) fn) extensions

getStatic h fn = do
    let fn' = if fn == "/" then root ++ "/index.html" else (root ++ fn)
        m = getMimeType fn
        bad = isInfixOf ".." fn
    e <- doesFileExist fn'

    if m==[]  || bad  then errmsg h $ forbidden fn
    else if not e then errmsg h $ notFound fn'
    else do
        f <- B.readFile fn'
        B.hPut h $ B.pack $
         "HTTP/1.1 200 OK\nConnection: close\nContent-Type: " ++ m ++ "\n\n"
        B.hPut h f

getCgi h fn lbuf = do
    let bad = isInfixOf ".." fn
    e <- doesFileExist $ root ++ fn

    if bad  then errmsg h $ forbidden fn
    else if not e then errmsg h $ notFound fn
    else do
        (fd0,fd1,fd2,pid) <- runInteractiveProcess (root++fn) [] Nothing Nothing
        --mapM hClose [fd0,fd2]
        d <- B.hGetContents fd1
        B.hPut h (B.append "HTTP/1.1 200 OK\nConnection: close\n" d)
        waitForProcess pid
        --B.putStr (B.append "HTTP/1.1 200 OK\nConnection: close\n" d)
        return ()
    return ()

serverproc :: Handle -> IO ()
serverproc h = do
    putStrLn " "
    (rc, buf) <- netGet h 4200 2000
    let lbuf = lines $ B.unpack buf
    mapM putStrLn lbuf
    when (rc>0) $ do
        if isPrefixOf "GET" (map toUpper $ head lbuf) then do
            case (words $ head $ lbuf) of
                (_:fn:_) -> do
                    perms <- getPermissions (root++fn)
                    print fn
                    if (executable perms) then getCgi h fn lbuf
                    else getStatic h fn
                _  -> return ()
        else if isPrefixOf "POST" (map toUpper $ head lbuf) then do
            case (words $ head $ lbuf) of
                (_:fn:_) -> do
                    perms <- getPermissions (root++fn)
                    print fn
                    if (executable perms) then getCgi h fn lbuf
                    else getStatic h fn
        else errmsg h $ forbidden ""

main = server portnum serverproc
