import Data.List
import System.IO

exports = [
     "str"
    ,"Stack (..)"
    ,"State (..)"
    ,"evalState"
    ,"render"
    ,"def_page"
    ,"def_http_hdr"
    ,"def_doctype"
    ,"setCookie"
 ]

inputtags = [
    "checkbox"
    ,"file"
    ,"hidden"
    ,"image"
    ,"password"
    ,"radio"
    ,"search"
    ,"submit"
    ,"text"
 ]

ctags = [
     ("a0","a0'","a")
    ,("br","br'","br")
    ,("hr","hr'","hr") -- horizontal rule  --  attrs deprecated
    ,("img0","img0'","img")
    ,("input","input'","input")
    ,("td0","td0'","td")
    ,("th0","th0'","th")
 ]

stags = [
     ("abbr","abbr'","abbr")
    ,("b","b'","b")
    ,("dd","dd'","dd")
    ,("dt","dt'","dt")
    ,("h1","h1'","h1"),("h2","h2'","h2"),("h3","h3'","h3")
    ,("h4","h4'","h4"),("h5","h5'","h5"),("h6","h6'","h6")
    ,("i","i'","i")
    ,("legend","legend'","legend")
    ,("li_s","li_s'","li")
    ,("p_s","p_s'","p")
    ,("q","q'","q")
    ,("sub","sub'","sub")
    ,("super","super'","super")
    ,("textarea","textarea'","textarea")
 ]

tags = [
     ("a","a'","a")
    ,("blockquote","bloclquote'","blockquote")
    ,("body","body'","body")
    ,("del","del'","del")
    ,("dl","dl'","dl")
    ,("div","div'","div")
    ,("fieldset","fieldset'","fieldset")
    ,("figure","figure'","figure")
    ,("figcaption","figcaption'","figcaption")
    ,("form","form'","form")
    ,("h_head","h_head'","head")
    ,("html","html'","html")
    ,("img","img'","img")
    ,("ins","ins'","ins")
    ,("li","li'","li")
    ,("meta","meta'","meta")
    ,("ol","ol'","ol")
    ,("option","option'","option")
    ,("p","p'","p")
    ,("pre","pre'","pre")
    ,("select","select'","select")
    ,("small","small'","small")
    ,("span","span'","span")
    ,("style","style'","style")
    ,("table","table'","table")
    ,("td","td'","td")
    ,("tbody","tbody'","tbody")
    ,("tfoot","tfoot'","tfoot")
    ,("th","th'","th")
    ,("thead","thead'","thead")
    ,("tr","tr'","tr")
    ,("h_title","h_title'","title")
    ,("ul","ul'","ul")
 ]

--
-- Note - functions for attrs are not necessary for literals such as
--   "/" used to close tags with attrs like <img>
--
attrs = [
    "c_class","id","title","xmlns"
    ,"checked"
    ,"http_equiv"
    ,"accesskey"
    ,"action"
    ,"align"
    ,"alt"
    ,"border"
    ,"cite"
    ,"content"
    ,"enctype"
    ,"height"
    ,"href"
    ,"maxlength"
    ,"method"
    ,"multiple"
    ,"name"
    ,"width"
    ,"required"
    ,"rows"
    ,"selected"
    ,"size"
    ,"src"
    ,"start"
    ,"c_style"
    ,"tabindex"
    ,"c_type"
    ,"value"
 ]

gen_mod =
    "--\n"
    ++ "-- PROGRAM IS GENERATED - DO NOT ALTER BY HAND\n"
    ++ "--\n\n"
    ++ "module Html (\n"
    ++  "    " ++  drop 5 (concatMap (\(t,t2,_)-> "    ," ++ t ++ "," ++ t2 ++ "\n")
			 (tags++ctags++stags))
    ++  "\n"
    ++  (concatMap (\(t)-> "    ,input_" ++ t ++ "\n") (inputtags))
    ++  "\n"
    ++  (concatMap (\t-> "    ," ++ t ++ "\n") attrs)
    ++  "\n"
    ++  (concatMap (\t-> "    ," ++ t ++ "\n") exports)
    ++  ") where\n"

gen_funcs =
    "\n------ Functions --------------\n"
    ++  concatMap fstr tags
    ++ "\n"
    ++  concatMap cstr ctags
    ++ "\n"
    ++  concatMap sstr stags
    ++ "\n"
    ++  concatMap astr attrs
    ++ "\n"
    ++  concatMap istr inputtags

  where
    fstr (f,f2,t) =
	f ++ " x = tag  \"" ++ t ++ "\" >> x >> etag \"" ++ t ++ "\"" ++ "\n" ++
	f2 ++ " as x = tag  (attrs \"" ++ t ++ "\" as) >> x >> etag \""
	  ++ t ++ "\"" ++ "\n"

    sstr (f,f2,t) =
	f ++ " x = tag  \"" ++ t ++ "\" >> str x >> etag \"" ++ t ++ "\"" ++ "\n" ++
	f2 ++ " as x = tag  (attrs \"" ++ t ++ "\" as) >> str x >> etag \""
	  ++ t ++ "\"" ++ "\n"

    cstr (f,f2,t) = f ++ " = tag \"" ++ t ++ " /\"\n" ++
	f2 ++ " as = tag  (attrs \"" ++ t ++ "\" as ++ \" /\")\n"

    istr f = "input_" ++ f ++
	" as = tag  (attrs \"input\" (\"type=\\\"" ++ f
        ++ "\\\"\":as) ++ \" /\")\n"

    astr f = f  ++  " x = attr "  ++  (show.cssfix) f  ++  " x\n"

    cssfix ('c' : '_' : xs) = xs
    cssfix xs = xs

gen_imports =
    "\n"
    ++ "import Prelude hiding (div,id,span, )\n"
    ++ "import Data.List hiding (span)\n"


genx = gen_mod ++ gen_imports ++ gen_funcs

main = do
    hPutStrLn stderr "Genx running ..."
    putStr genx
    readFile "Html0.hs" >>= putStr
