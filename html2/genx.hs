import Data.List
import System.IO

exports = [
     "str"
    ,"(!)"
    ,"(-)"
    ,"Logger (..)"
 ]

tags = [
     ("a","0")
    ,("abbr","1")
    ,("b","1")
    ,("blockquote","1")
    ,("body","1")
    ,("br","2")
    ,("del","1")
    ,("div","1")
    ,("form","1")
    ,("h_head","1")
    ,("html","1")
    ,("h1","1"),("h2","1"),("h3","1"),("h4","1"),("h5","1"),("h6","1")
    ,("hr","0") -- horizontal rule  --  attrs deprecated
    ,("i","1")
    ,("img","0")
    ,("input","0")
    ,("ins","1")
    ,("li","7")
    ,("meta","0")
    ,("ol","1")
    ,("p","7")
    ,("pre","1")
    ,("q","1")
    ,("small","1")
    ,("span","1")
    ,("style","1")
    ,("sub","1")
    ,("super","1")
    ,("table","1")
    ,("td","7")
    ,("tr","7")
    ,("h_title","1")
    ,("ul","1")
 ]

--
-- Note - functions for attrs are not necessary for literals such as
--   "/" used to close tags with attrs like <img>
--
attrs = [
    "h_class","id","title","xmlns"
    ,"http_equiv"

    ,"accesskey"
    ,"action"
    ,"align"
    ,"alt"
    ,"border"
    ,"cite"
    ,"content"
    ,"height"
    ,"href"
    ,"maxlength"
    ,"method"
    ,"name"
    ,"width"
    ,"size"
    ,"src"
    ,"start"
    ,"tabindex"
    ,"h_type"
    ,"value"
 ]

gen_mod =
    "--\n"
    ++ "-- PROGRAM IS GENERATED - DO NOT ALTER BY HAND\n"
    ++ "--\n\n"
    ++ "module Html (\n"
    ++  "    " ++ drop 5 (concatMap (\(t,_)-> "    ," ++ t ++ "\n") tags)
    ++  "\n"
    ++  (concatMap (\t-> "    ," ++ t ++ "\n") attrs)
    ++  "\n"
    ++  (concatMap (\t-> "    ," ++ t ++ "\n") exports)
    ++  ") where\n"

gen_funcs =
    "\n------ Functions --------------\n"
    ++  concatMap fstr tags
    ++ "\n"
    ++  concatMap astr attrs

  where fstr (f,pri) = f ++ " x = tell \"<" ++ f ++ ">\" >> x >> tell \"</"
	  ++ f ++ ">\""  ++ "\n"
	astr f = f  ++  " val = atag "  ++  (show.fix) f  ++  " val\n"

	fix ('h' : '_' : xs) = xs
	fix xs = xs

misc_funcs =
    "\n"
 ++ "data Lstr = Tag String | Attr String"
 ++ "  deriving Show"
 ++ "\n"
 ++ "newtype Logger a = Logger {runLogger :: (a,[Lstr])} deriving (Show)\n"
 ++ "\n"
 ++ "instance Monad Logger where\n"
 ++ "    return a = Logger (a,[])\n"
 ++ "    Logger(a,w) >>= f =\n"
 ++ "        let Logger (b,w') = f a\n"
 ++ "        in Logger (b,w++w')\n"
 ++ "\n"
 ++ "tell s = Logger ((),[Tag s])\n"
 ++ "tella f s = Logger ((),[Attr s]) >> f\n"
 ++ "\n"
 ++ "str x = tell x\n"
 ++ "\n"

 ++ "infixr 0 -\n"
 ++ "(-) f x = f x\n"

 ++ "(!) f s = tella f s\n"
 ++ "\n"

 ++ "atag cls val = \"\"\n"
 ++ "\n"

gen_imports =
    "\n"
    ++ "import Prelude hiding (div,id,span,(-) )\n"
    ++ "import Data.List hiding (span)\n"


genx = gen_mod ++ gen_imports ++ gen_funcs ++ misc_funcs

main = do
    hPutStrLn stderr "Genx running ..."
    putStr genx
