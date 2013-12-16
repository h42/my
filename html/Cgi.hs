module Cgi (
    CGI (..)
    ,cgi_init
    ,cgi_init_mime
    ,cgi_var
    ,cgi_vars
    ,cgi_evar
) where

import System.Environment
import Data.Char
import Data.List
import qualified Data.ByteString.Char8 as B

-------------------------------
-- GETDATA
-------------------------------

type Vtab = [(String,String)]

data CGI = CGI {zenv :: Vtab, zvtab :: Vtab}
    deriving (Show)

-- CGI_INIT_MIME
cgi_init_mime :: IO String
cgi_init_mime = do
    env <- getEnvironment
    get_mime env

get_mime env = do
    case (cgi_lookup "REQUEST_METHOD" env) of
	"POST" -> post_mime
	_      -> return ""

post_mime = do
    s <- B.getContents
    B.writeFile "/apache/formdata" s
    return $ B.unpack s

-- CGI_INIT
cgi_init :: IO CGI
cgi_init = do
    env <- getEnvironment
    vtab <- case (cgi_lookup "REQUEST_METHOD" env) of
	"POST" -> fmap getdata getContents
	"GET"  -> return (getdata $ cgi_lookup "QUERY_STRING" env)
	_      -> return []
    return CGI {zenv=env, zvtab=vtab}

-- CGI_VAR
cgi_var :: String -> CGI -> String
cgi_var v cgi = cgi_lookup v (zvtab cgi)

-- CGI_VARS
cgi_vars :: String -> CGI -> [String]
cgi_vars v cgi = foldl' (f v) [] (zvtab cgi)
f v  acc (var,val)
    | var==v = val:acc
    | otherwise = acc

-- CGI_EVAR
cgi_evar :: String -> CGI -> String
cgi_evar v cgi = cgi_lookup v (zenv cgi)

-- CGI_LOOKUP
cgi_lookup v e = case (lookup v e) of
    Just x -> x
    Nothing -> []

-- GETDATA
getdata :: String -> Vtab
getdata xs = getdata2 xs "" "" []

getdata2 :: String -> String -> String -> Vtab -> Vtab
getdata2 [] _ _ vt = vt
getdata2 ('=':xs) n v vt = getdata3 xs n "" vt
getdata2 (x:xs) n v vt = getdata2 xs (x:n) "" vt

getdata3 ('&':xs) n v vt = getdata2 xs "" "" ((reverse n,reverse v):vt)
getdata3 [] n v vt = getdata2 [] "" "" ((reverse n,reverse v):vt)
getdata3 ('%':xs) n v vt = getdata4 xs n v vt
getdata3 (x:xs) n v vt = getdata3 xs n (x:v) vt

getdata4 xs n v vt = getdata3 xs' n (x':v) vt where
    (x',xs') = if length xs >= 2 && all isHexDigit (take 2 xs)
		   then (dehex $ take 2 xs, drop 2 xs)
		   else ('$',xs)

dehex xs = chr $ foldl (\a x -> a*16 + digitToInt x) 0 xs

-- SETCOOKIE
{-
    Set-Cookie: name=val; Expire=Tue, 15 Jan 2013 21:47:38 GMT;
                 Path=/; Domain=jld2.net; Secure; HttpOnly;
    * Optional Max-Age=3600; time in seconds instead
-}


