{-# LANGUAGE OverloadedStrings #-}

import Data.Char
import Data.List
import qualified Data.Map as M
import System.Environment
import System.IO.Error
import qualified Data.ByteString.Char8 as B
import System.IO
import System.Directory
import System.Process
import Control.Monad
import My.Net
import My.Time
import My.Parse

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

--
-- GET STATIC
--
getMimeType fn = if null xs then [] else (snd.head) xs  where
    xs = filter (\x->isSuffixOf (fst x) fn) extensions

getStatic h fn = do
    let m = getMimeType fn
    if m==[] then errmsg h $ forbidden fn
    else do
        f <- B.readFile fn
        B.hPut h $ B.pack $
         "HTTP/1.1 200 OK\nConnection: close\nContent-Type: " ++ m ++ "\n\n"
        B.hPut h f

--
-- GET/POST CGI
--
getCgi h fn op qstr hs ds = do
    print op
    mapM_ print hs
    (fd0,fd1,fd2,pid) <- runInteractiveProcess fn [] Nothing e
    d <- B.hGetContents fd1
    B.hPut h (B.append "HTTP/1.1 200 OK\nConnection: close\n" d)
    waitForProcess pid
    return ()
  where
    -- ("Referer","http://localhost:8080/
    qs = if null qstr then
             case lookup "Referer" hs of
             Just q -> ("QUERY_STRING", if null q then ""
                                        else tail $ dropWhile (/='?') q)
             Nothing -> ("","")
         else ("QUERY_STRING", tail qstr)
    e = Just $ filter (/=("",""))
            [("REQUEST_METHOD","GET") ,qs ]

postCgi h fn op qstr hs ds = do
    print op
    --mapM_ print hs
    (fd0,fd1,fd2,pid) <- runInteractiveProcess fn [] Nothing e
    hPutStr fd0 ds  >>  hFlush fd0
    d <- B.hGetContents fd1
    B.hPut h (B.append "HTTP/1.1 200 OK\nConnection: close\n" d)
    waitForProcess pid
    return ()
  where
    ml = case lookup "Content-Length" hs of
                 Just l -> ("CONTENT-lENGTH",l)
                 Nothing -> ("","")
    e = Just $ filter (/=("","")) [("REQUEST_METHOD","POST"), ml]

--
-- Parse Input
--
parseInput ubuf = do
    let rc = runParse parseInput2 ubuf
    return rc

--parseInput2 :: Parse String (String,String,String,[[Char],String],String)
parseInput2 = do
    (h1,h2,h3) <- getHdr
    hs <- many1 httpData
    ds <- onLeft "" (eol >> toEod)
    return (h1,h2,h3,hs,ds)

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

--
-- SERVERPROC
--
serverproc :: String -> Handle -> IO ()
serverproc root h = do
    (rc, buf) <- netGet h 4200 2000
    let ubuf = B.unpack buf
    pdata <- parseInput ubuf
    case pdata of
        Left err -> print err
        Right ((iop,ifn,iver,i_hs,i_ds), i_st) -> do
            let (fn',qstr) = span (/= '?') ifn
                bad = isInfixOf ".." fn'
                fn = if fn' == "/" then root ++ "/index.html"
                                   else (root ++ fn')
            e <- doesFileExist $ fn
            putStrLn $ "fn="++fn
            if bad  then errmsg h $ forbidden fn
            else if not e then print "NOTFOUND" >> (errmsg h $ notFound fn)
            else do
                perms <- getPermissions (fn)
                if isPrefixOf "GET" (map toUpper iop) then do
                    if (executable perms) then getCgi h fn iop qstr i_hs i_ds
                    else getStatic h fn
                else if isPrefixOf "POST" (map toUpper iop) then do
                    if (executable perms) then postCgi h fn iop qstr i_hs i_ds
                    else getStatic h fn
                else errmsg h $ forbidden ("command " ++ iop ++ " not implemented")
    return ()

main = do
    let portnum = 8080
        root = "/www"
    server portnum (serverproc root)
