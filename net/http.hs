{-# LANGUAGE OverloadedStrings #-}

import Data.Char
import Data.List
import qualified Data.Map as M
import System.Environment
import qualified Data.ByteString.Char8 as B
import System.IO
import System.Directory
import System.Process
import Control.Monad
import My.Net
import My.Time
import My.Parse

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

getCgi h fn llbuf = do
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

parseInput ubuf = do
    let prc = runParse parseInput2 ubuf
    print prc

parseInput2 = do
    (w1,w2,w3) <- getHdr
    hs <- many1 httpData
    return (w1,w2,w3,hs)

httpData = do
    d1 <- many1 (noneOf ":")
    string ": "
    d2 <- toEol
    return (d1,d2)

getHdr  = do
    w1 <- word
    spaces
    w2 <- word
    spaces
    w3 <- word
    eol
    return (w1,w2,w3)


serverproc :: Handle -> IO ()
serverproc h = do
    (rc, buf) <- netGet h 4200 2000
    let ubuf = B.unpack buf
    prc <- parseInput ubuf
    return ()

{-
        lbuf = lines ubuf

    putStrLn ""
    mapM putStrLn lbuf
    print $ take 3 $ words ubuf

    case (words ubuf) of
        (op:ifn:_) -> do
            let (fn,qstr) = span (/= '$') ifn
                bad = isInfixOf ".." fn
            --print (op,ifn,fn,qstr)
            e <- doesFileExist $ root ++ fn
            perms <- getPermissions (root++fn)

            if bad  then errmsg h $ forbidden fn
            else if not e then errmsg h $ notFound fn
            else if isPrefixOf "GET" (map toUpper op) then do
                if (executable perms) then getCgi h fn lbuf
                else getStatic h fn
            else if isPrefixOf "POST" (map toUpper op) then do
                if (executable perms) then getCgi h fn lbuf
                else getStatic h fn
            else return ()
        _ -> return ()

getContent buf = if B.length s11 < B.length s21  then s12  else s22  where
    (s11,s12) = B.breakSubstring "\r\n\r\n" buf
    (s21,s22) = B.breakSubstring "\n\n" buf
-}

main = server portnum serverproc
