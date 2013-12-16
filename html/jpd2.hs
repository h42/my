import Debug.Trace
import Data.List
import Data.Char
import qualified Data.ByteString.Char8 as C
import Control.Monad.State
import GHC.Types

type Vtab = [(String,String)]

data Mime = Mime {
    boundary :: String
    ,zdisp :: String
    ,ztype :: String
    ,zname :: String
    ,zfilename :: String
    ,zval  :: [String]
    ,zfile :: String
    ,ztab  :: [(String,String)]
} deriving Show

initMime = Mime {boundary=""
		,zdisp=""
		,ztype=""
		,zname=""
		,zfilename=""
		,zval=[]
		,zfile=""
		,ztab=[]
		}

--init_mime :: IO (Either String Vtab)
init_mime = do
    --s <- getContents
    s <- readFile "mimedata"
    let (rc,m) = runState (mime_begin s) initMime
    case rc of
	Left _ -> return []
	_      -> return $ ztab m

mime_begin :: String -> State Mime (Either String ())
mime_begin s = do
    let (bnd,xs) = mime_line s
    if null xs then return $ Left "unexpected eof in state0"
    else do
	put initMime{boundary=bnd}
	mimeloop xs

mimeloop :: String -> State Mime (Either String ())
mimeloop [] = return $ Right ()
mimeloop s
    | null ln = do
	st <- get
	if null (zname st) then return $ Left "Missing zname in state 10"
	else if null (zfilename st) then get_mime_val xs []
	else get_mime_file xs []
    | isPrefixOf con1 ln = do
	rc <- chkdisp (ln2 con1) []
	case rc of
	    Left _ -> return rc
	    Right _ -> mimeloop xs
    | isPrefixOf con2 ln = do
	rc <- chktype (ln2 con2) []
	case rc of
	    Left _ -> return rc
	    Right _ -> mimeloop xs
    | otherwise = return (Left "bad input in state11")
  where
      (ln,xs) = mime_line s
      con1 = "Content-Disposition:"
      con2 = "Content-Type:"
      ln2 convar = dropWhile isSpace $ drop (length convar) ln

get_mime_val :: String -> String -> State Mime (Either String ())
get_mime_val s val
    | null s || (null ln && null xs) = return (Left "Error in get_mime_val")
    | otherwise = do
	st <- get
	if isPrefixOf (boundary st) ln then do
	    put st{ztab=(zname st, val) : (ztab st)
		   ,zname="",zfilename="",zval=[]}
	    mimeloop xs
	else get_mime_val xs ln
  where
    (ln,xs) = mime_line s

get_mime_file [] val= return (Left "Error in get_file")
get_mime_file xxs@(x:xs) val = do
    st <- get
    if (boundary st) `isPrefixOf` xxs then do
	put st{ztab=(zname st, reverse val) : (ztab st)
	       ,zname="",zfilename="",zval=[]}
	mimeloop $ safetail $ snd (span (/= '\n') xs)
    else get_mime_file xs (x:val)

safetail [] = []
safetail (x:xs) = xs

--
-- CHKTYPE CHKDISP
--
chktype :: String -> String -> State Mime (Either String ())
chktype [] var = chktype2 var
chktype (x:xs) var
    | x==';' = chktype2 var
    | otherwise = chktype xs (x:var)

chktype2 var = do
    st <- get
    put st{ztype=reverse var}
    return $ Right ()


chkdisp :: String -> String -> State Mime (Either String ())
chkdisp [] var = return $ Left "unexpected EOL in chkdisp"
chkdisp (x:xs) var
    | x==';' = do
	st <- get
	put st{zdisp=reverse var}
	chkvars (dropWhile isSpace xs) []
    | otherwise = chkdisp xs (x:var)

--chkvars :: String -> String -> State Mime (Either String ())
chkvars (x:xs) var
    | x=='='    = chkvars3 xs (reverse var)
    | otherwise = chkvars xs (x:var)

chkvars3 :: String -> String -> State Mime (Either String ())
chkvars3 (x:xs) var
    | x==' ' = chkvars3 xs var
    | x=='"' = chkvars4 xs var ""
    | otherwise = return $ Left "bad input in chkvars3"

chkvars4 [] var val = return $ Left "unexpected input in chkvars4"
chkvars4 (x:xs) var val
    | x=='"' = do
	st <- get
	if var == "name" then do
	    put st{zname=reverse val}
	    chkvars5 xs
	else if var == "filename" then do
	    put st{zfilename=reverse val}
	    chkvars5 xs
	else chkvars5 xs
    | otherwise = chkvars4 xs var (x:val)

chkvars5 [] = return $ Right ()
chkvars5 (x:xs)
    | x==';'  =  chkvars xs []
    | x==' '  =  chkvars5 xs
    | otherwise = return $ Left "unexpected input in chkvars5"

--
-- UTILS
--
mime_line xs = mime_line' xs []
mime_line' [] ls = (reverse ls,[])
mime_line' ('\r':'\n':xs) ls = mime_line' ('\n':xs) ls
mime_line' ('\n':xs) ls = (reverse ls, xs)
mime_line' (x:xs) ls = mime_line' xs (x:ls)

--
-- MAIN
--

main = do
    rc <- init_mime
    mapM_ print rc
