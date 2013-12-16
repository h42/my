
newtype State s a = State { runState :: s -> (a,s) }

instance Monad (State s) where
    return x = State $ \s -> (x,s)
    (State h) >>= f = State $ \s -> let (aa, newState) = h s
					(State g) = f aa
				    in g newState

evalState st st0 = snd $ runState st st0

data Html = Htag String | Hetag String | Hstr String
    deriving Show

data Stack = Stack {
    zhtml :: [Html]
    ,zint  :: Int
} deriving Show
stack0 = Stack {zhtml=[],zint=0}

render st0 = concat $ reverse s where
    stack = evalState st0 stack0
    (s,_) = foldr rend ([],"") (zhtml stack)

    rend ::Html -> ([String],String) -> ([String],String)
    rend (Htag x) (tags,lev) = ((t1:tags), lev) where
	t1 = lev ++ ('<' : x) ++ ">\n"
    rend (Hetag x) (tags,lev) = (((lev ++ "</" ++ x ++ ">\n") : tags), lev)
    rend (Hstr x) (tags,lev) = ( ( (lev ++ x ++ "\n") : tags), lev)
    
str :: String -> State Stack ()
str x = State $ \st -> ((), st {zhtml=Hstr x:zhtml st} )

tag :: String -> State Stack ()
--tag x = State $ \(Stack xs _) -> ((), Stack ( Htag x : xs) undefined)
tag x = State $ \st -> ((), st {zhtml=Htag x:zhtml st} )

etag :: String -> State Stack ()
etag x = State $ \st -> ((), st {zhtml=Hetag x:zhtml st} )

attrs t as = t ++ " " ++ intercalate ", " as
attr x y = x ++ "=\"" ++ y ++ "\""

--
-- DEFAULTS
--
def_page header mytitle mybody  = do
    if null header then str def_http_hdr
    else str header
    str def_doctype
    html $ do
	h_head $ h_title $ str mytitle
	body $ mybody


def_http_hdr :: String
def_http_hdr = "Content-type: " ++ "text/html" ++ "; charset=utf-8"

-- <html> attribute if you want to conform to XHTML
def_xmlns :: String
def_xmlns = "http://www.w3.org/1999/xhtml\n"

def_doctype :: String
def_doctype = "\n<!DOCTYPE HTML>"

setCookie cname val xs =
    "Set-Cookie: " ++  cname  ++  "="  ++  val  ++  "; "
    ++ if not (null xs) then (intercalate "; " xs  ++  ";")  else ""

--
-- INPUT
--
--input_text xs =
--    tag  (attrs "input" ("type=\"text\"" : xs) ++ " /")

