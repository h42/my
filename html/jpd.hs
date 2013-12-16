{-# LANGUAGE OverloadedStrings #-}
import Data.List
import Data.Char
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString as B
import Control.Monad.State
import GHC.Types
--import Debug.Trace

type Vtab = [(String,String,B.ByteString)]

data Mime = Mime {
    boundary :: C.ByteString
    ,bound2  :: C.ByteString
    ,zdisp :: String
    ,ztype :: String
    ,zname :: String
    ,zfilename :: String
    ,ztab  :: Vtab
} deriving Show

initMime = Mime {boundary=""
		,bound2=""
		,zdisp=""
		,ztype=""
		,zname=""
		,zfilename=""
		,ztab=[]
		}

init_mime :: IO (Either String Vtab)
init_mime = do
    --s <- getContents
    s <- C.readFile "mimedata"
    let (rc,m) = runState (mime_begin s) initMime
    case rc of
	--Left err -> trace (show m ++ err) (return $ Left err)
	Left err ->  (return $ Left err)
	_      -> return $ Right (ztab m)

mime_begin :: B.ByteString -> State Mime (Either String ())
mime_begin s = do
    let (bnd,xs) = mime_line s
    if C.null xs then return $ Left "unexpected eof in state0"
    else do
	put initMime{boundary=bnd,bound2=(B.append (C.pack "\r\n") bnd)}
	mimeloop xs

mimeloop :: C.ByteString -> State Mime (Either String ())
mimeloop s
    | C.null s = return $ Right ()
    | C.null ln = do
	st <- get
	if null (zname st) then return $ Left "Missing zname in mimeloop"
	else if null (zfilename st) then get_mime_val xs ""
	else (get_mime_file xs)
    | C.isPrefixOf con1 ln = do
	rc <- chkdisp (ln2 con1) []
	case rc of
	    Left _ -> return rc
	    Right _ -> mimeloop xs
    | C.isPrefixOf con2 ln = do
	rc <- chktype (ln2 con2) []
	case rc of
	    Left _ -> return rc
	    Right _ -> mimeloop xs
    | otherwise = mimeloop xs
  where
      (ln,xs) = mime_line s
      con1 = "Content-Disposition:"
      con2 = "Content-Type:"
      ln2 convar = C.dropWhile isSpace $ C.drop (C.length convar) ln

get_mime_val :: C.ByteString -> String -> State Mime (Either String ())
get_mime_val s val
    | C.null s || (C.null ln && C.null xs) =
	return (Left "Error in get_mime_val")
    | otherwise = do
	st <- get
	if B.isPrefixOf (boundary st) ln then do
	    put st{ztab=(zname st, dehex val, "") : (ztab st)
		   ,zname="",zfilename=""}
	    mimeloop xs
	else get_mime_val xs val2
  where (ln,xs) = mime_line s
	val2 = val ++ (if (not.null) val then "\n" else "") ++ C.unpack ln

get_mime_file :: B.ByteString -> State  Mime (Either String ())
get_mime_file s
    | B.null s = return (Left "Error in get_file")
    | otherwise = do
	st <- get
	let (zf,xs) = B.breakSubstring (bound2 st) s
	if (not . B.null) xs then do
	    put st{ztab=(zname st, zfilename st, zf) : (ztab st)
		   ,zname="",zfilename=""}
	    mimeloop $ C.tail $ C.dropWhile (/='\n') (B.drop 2 xs)
	else return (Left "Error in get_file")

--
-- CHKTYPE CHKDISP
--
chktype :: C.ByteString -> String -> State Mime (Either String ())
chktype s var
    | C.null s = chktype2 var
    | x==';' = chktype2 var
    | otherwise = chktype xs (x:var)
  where (x,xs) = (C.head s, C.tail s)

chktype2 var = do
    st <- get
    put st{ztype=reverse var}
    return $ Right ()

chkdisp :: C.ByteString -> String -> State Mime (Either String ())
chkdisp s var
    | C.null s = return $ Left "unexpected EOL in chkdisp"
    | x==';' = do
	st <- get
	put st{zdisp=reverse var}
	chkvars (C.dropWhile isSpace xs) []
    | otherwise = chkdisp xs (x:var)
  where (x,xs) = (C.head s, C.tail s)

chkvars s var
    | x=='='    = chkvars3 xs (dropWhile (==' ') $ reverse var)
    | otherwise = chkvars xs (x:var)
  where (x,xs) = (C.head s, C.tail s)

chkvars3 :: C.ByteString -> String -> State Mime (Either String ())
chkvars3 s var
    | x==' ' = chkvars3 xs var
    | x=='"' = chkvars4 xs var ""
    | otherwise = return $ Left "bad input in chkvars3"
  where (x,xs) = (C.head s, C.tail s)

chkvars4 s var val
    | C.null s = return $ Left "unexpected input in chkvars4"
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
  where (x,xs) = (C.head s, C.tail s)

chkvars5 s
    | C.null s = return $ Right ()
    | x==';'  =  chkvars xs []
    | x==' '  =  chkvars5 xs
    | otherwise = return $ Left "unexpected input in chkvars5"
  where (x,xs) = (C.head s, C.tail s)

--
-- UTILS
--
--mime_line s = trace ("trace + " ++ C.unpack ln) (ln2,xs2) where
mime_line s = (ln2,xs2) where
    (ln,xs) = C.span (/='\n') s
    ln2 = if (not $ C.null ln) && C.last ln == '\r' then C.init ln else ln
    xs2 = if C.head xs == '\n' then C.tail xs else xs

dehex [] = []
dehex ('%':xs)
    | length xs2 == 2 && all isHexDigit xs2 = c : dehex (drop 2 xs)
    | otherwise = '%' : dehex xs
  where
      xs2 = take 2 xs
      c = chr $ foldl (\a x -> a*16 + digitToInt x) 0 (take 2 xs)
dehex (x:xs) = x : dehex xs

mime_lookup k [] = Nothing
mime_lookup k ((x,y,z):vt) = if k == x then Just (x,y,z) else mime_lookup k vt

--
-- MAIN
--

main = do
    {-
    let IO c :: IO Char = getChar
    putChar c
    -}
    rc <- init_mime
    case rc of
	Left err -> print rc
	Right vt ->  do
	    --mapM_ print vt
	    process vt "user"
	    process vt "myfile"
	    process vt "myfile2"
    print $ dehex "ab%37def"

process vt fn = do
    let f = mime_lookup fn vt
    case f of
	Nothing -> return ()
	Just (x,y,z) -> do
	    print y
	    --C.putStr z
    putStrLn $ replicate 42 '-' ++ "\n"

