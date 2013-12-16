module My.Xml (
    Xml (..)
    ,xmlParseFile
    ,xmlParse
) where

import Data.List
import Data.Char
--import qualified Control.Exception as E
import System.IO.Error

data 
  Xml = XmlTag  String [(String,String)] 
      | XmlData String String 
      | XmlEtag String
  deriving (Show)

xmlParseFile :: String -> IO (Either String [Xml])
xmlParseFile fn = catchIOError
    (do
	s <- readFile fn
	return $ xmlParse s)
    (\e -> return $ Left $ "Error reading file " ++ fn)

xmlParse :: String -> Either String [Xml]
xmlParse sx = state0 sx 1 [] []

state0 :: String -> Int -> [Xml] -> [String] -> Either String [Xml]
state0 [] n xmls _ = Right $ reverse xmls
state0 s n xmls taglist
    | x=='<' && rc==0 = state2 xs n' xmls taglist
    | null taglist = if isSpace x then state0 xs n' xmls taglist
		     else errmsg "Failed in state0" n'
    | otherwise = data0 xs n' [x] xmls taglist
  where (rc,x,xs,n') = fixChar s n

state2 [] n _ taglist = errmsg "Failed in state2 with empty input" n
state2 s n xmls taglist
    | isSpace x = state2 xs n' xmls taglist
    | isAlpha x = btag xs n' [x] xmls taglist
    | x=='/' = etag xs n' [] xmls taglist
    | otherwise = errmsg "Failed in state2" n'
  where (rc,x,xs,n') = fixChar s n


btag [] n _ _ _ = errmsg "Failed in btag" n
btag s n name xmls taglist
    | isAlphaNum x = btag xs n' (x:name) xmls taglist
    | otherwise = btag2 s n' name xmls taglist
  where (rc,x,xs,n') = fixChar s n

btag2 [] n _ _ _ = errmsg "Failed in btag2" n
btag2 s n name xmls taglist
    | isSpace x = btag2 xs n' name xmls taglist
    | x=='>' && rc==0  = state0 xs n' xmls' (name':taglist)
    | isAlpha x = attrS0 xs n' name' [x] [] xmls (name':taglist)
    | otherwise = errmsg "Failed in btag2" n'
  where  xmls' = (XmlTag name' [] ):xmls 
	 (rc,x,xs,n') = fixChar s n
	 name' = reverse name

--attr :: String -> Int -> String -> String -> [(string,String)] -> [Xml] -> [String] -> Either String [Xml]
attrS0 s n name aname attrs xmls taglist
    | isAlphaNum x = attrS0 xs n' name (x:aname) attrs xmls taglist
    | x=='>' && rc==0  = attrEnd xs n' name (attrAdd aname "" attrs) xmls taglist
    | isSpace x = attrS1 xs n' name aname attrs xmls taglist
    | x=='=' = attrS2 xs n' name aname [] attrs xmls taglist
    | otherwise = errmsg "Failed in attr" n'
  where (rc,x,xs,n') = fixChar s n
	--name' = reverse name

attrEnd xs n name attrs xmls taglist = state0 xs n xmls' taglist
  where xmls' = (XmlTag name (reverse attrs) ):xmls

attrAdd aname aval attrs = (reverse aname,reverse aval):attrs

attrS1 s n name aname attrs xmls taglist
    | x=='=' = attrS2 xs n' name aname [] attrs xmls taglist
    | isSpace x = attrS1 xs n' name aname attrs xmls taglist
    | isAlpha x = attrS0 xs n' name [x] (attrAdd aname "" attrs) xmls taglist
    | x=='>' && rc==0  = attrEnd xs n' name (attrAdd aname "" attrs) xmls taglist
  where  (rc,x,xs,n') = fixChar s n

attrS2 s n name aname aval attrs xmls taglist
    | isSpace x        = attrS2 xs n' name aname aval attrs xmls taglist
    | (x=='"' || x=='\'') && rc==0  = attrS3 xs n' x name aname aval attrs xmls taglist
    | otherwise        = errmsg "Expecting space or '=' in attrS2" n'
  where  (rc,x,xs,n') = fixChar s n

attrS3 [] n qtype name aname aval attrs xmls taglist = errmsg "Expecting \" in attrS3" n
attrS3 s n qtype name aname aval attrs xmls taglist
    | x==qtype && rc==0  = attrS4 xs n' name (attrAdd aname aval attrs) xmls taglist
    | otherwise        = attrS3 xs n' qtype name aname (x:aval) attrs xmls taglist
  where  (rc,x,xs,n') = fixChar s n

attrS4 s n name attrs xmls taglist
    | isSpace x        = attrS4 xs n' name attrs xmls taglist
    | isAlpha x        = attrS0 xs n' name [x] attrs xmls taglist
    | x=='>' && rc==0  = attrEnd xs n' name attrs xmls taglist
    | otherwise        = errmsg "Unexpected char in attrS4" n'
  where  (rc,x,xs,n') = fixChar s n


pdata xs n xdata xmls taglist
    | all isSpace xdata = state2 xs n xmls taglist
    | null taglist = errmsg "Data not nested" n
    | otherwise = state2 xs n ( (XmlData (head taglist)
	  (reverse xdata)) : xmls ) taglist

data0 s n xdata xmls taglist
    | x=='<' && rc==0 = pdata xs n' xdata xmls taglist
    | otherwise = data0 xs n' (x:xdata) xmls taglist
  where (rc,x,xs,n') = fixChar s n


etag [] n _ _ _ = errmsg "Failed in etag" n
etag s n name xmls taglist
    | isSpace x = etag xs n' name xmls taglist
    | isAlpha x = etag1 xs n' [x] xmls taglist
    | otherwise = errmsg "Failed in etag" n'
  where (rc,x,xs,n') = fixChar s n

etag1 [] n _ _ _ = errmsg "Failed in etag1" n
etag1 s n name xmls taglist
    | isAlphaNum x = etag1 xs n' (x:name) xmls taglist
    | otherwise = etag2 s n' name xmls taglist
  where (rc,x,xs,n') = fixChar s n

etag2 [] n _ _ _ = errmsg "Failed in etag2" n
etag2 s n name xmls taglist
    | isSpace x = etag2 xs n' name xmls taglist
    | x=='>' && rc==0 && (not $ null taglist) && name'==head taglist
	= state0 xs n' xmls' (tail taglist)
    | otherwise = errmsg ("Failed in etag2"
	++ show taglist ++ " - " ++ show name) n'
  where  xmls' = (XmlEtag name'):xmls 
	 (rc,x,xs,n') = fixChar s n
	 name' = reverse name

--
-- UTILITIES
--
fixChar :: String -> Int -> (Int, Char, String, Int)
fixChar (x:xs) n
    | x == '\n' = (0,x,xs,n+1)
    | x /= '&' = (0,x,xs,n)
    | isPrefixOf "gt;" xs = (1, '>', drop 3 xs, n)
    | isPrefixOf "lt;" xs = (1, '<', drop 3 xs, n)
    | isPrefixOf "amp;" xs = (1, '&', drop 4 xs, n)
    | isPrefixOf "apos;" xs = (1, '\'', drop 5 xs, n)
    | isPrefixOf "quot;" xs = (1, '"', drop 5 xs, n)
    | otherwise = (0,'&',xs, n)

errmsg :: String -> Int -> Either String [Xml]
errmsg s n = Left $ s ++ "- Line " ++ show n
