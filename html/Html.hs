--
-- PROGRAM IS GENERATED - DO NOT ALTER BY HAND
--

module Html (
    a,a'
    ,blockquote,bloclquote'
    ,body,body'
    ,del,del'
    ,dl,dl'
    ,div,div'
    ,fieldset,fieldset'
    ,figure,figure'
    ,figcaption,figcaption'
    ,form,form'
    ,h_head,h_head'
    ,html,html'
    ,img,img'
    ,ins,ins'
    ,li,li'
    ,meta,meta'
    ,ol,ol'
    ,option,option'
    ,p,p'
    ,pre,pre'
    ,select,select'
    ,small,small'
    ,span,span'
    ,style,style'
    ,table,table'
    ,td,td'
    ,tbody,tbody'
    ,tfoot,tfoot'
    ,th,th'
    ,thead,thead'
    ,tr,tr'
    ,h_title,h_title'
    ,ul,ul'
    ,a0,a0'
    ,br,br'
    ,hr,hr'
    ,img0,img0'
    ,input,input'
    ,td0,td0'
    ,th0,th0'
    ,abbr,abbr'
    ,b,b'
    ,dd,dd'
    ,dt,dt'
    ,h1,h1'
    ,h2,h2'
    ,h3,h3'
    ,h4,h4'
    ,h5,h5'
    ,h6,h6'
    ,i,i'
    ,legend,legend'
    ,li_s,li_s'
    ,p_s,p_s'
    ,q,q'
    ,sub,sub'
    ,super,super'
    ,textarea,textarea'

    ,input_checkbox
    ,input_file
    ,input_hidden
    ,input_image
    ,input_password
    ,input_radio
    ,input_search
    ,input_submit
    ,input_text

    ,c_class
    ,id
    ,title
    ,xmlns
    ,checked
    ,http_equiv
    ,accesskey
    ,action
    ,align
    ,alt
    ,border
    ,cite
    ,content
    ,enctype
    ,height
    ,href
    ,maxlength
    ,method
    ,multiple
    ,name
    ,width
    ,required
    ,rows
    ,selected
    ,size
    ,src
    ,start
    ,c_style
    ,tabindex
    ,c_type
    ,value

    ,str
    ,Stack (..)
    ,State (..)
    ,evalState
    ,render
    ,def_page
    ,def_http_hdr
    ,def_doctype
    ,setCookie
) where

import Prelude hiding (div,id,span, )
import Data.List hiding (span)

------ Functions --------------
a x = tag  "a" >> x >> etag "a"
a' as x = tag  (attrs "a" as) >> x >> etag "a"
blockquote x = tag  "blockquote" >> x >> etag "blockquote"
bloclquote' as x = tag  (attrs "blockquote" as) >> x >> etag "blockquote"
body x = tag  "body" >> x >> etag "body"
body' as x = tag  (attrs "body" as) >> x >> etag "body"
del x = tag  "del" >> x >> etag "del"
del' as x = tag  (attrs "del" as) >> x >> etag "del"
dl x = tag  "dl" >> x >> etag "dl"
dl' as x = tag  (attrs "dl" as) >> x >> etag "dl"
div x = tag  "div" >> x >> etag "div"
div' as x = tag  (attrs "div" as) >> x >> etag "div"
fieldset x = tag  "fieldset" >> x >> etag "fieldset"
fieldset' as x = tag  (attrs "fieldset" as) >> x >> etag "fieldset"
figure x = tag  "figure" >> x >> etag "figure"
figure' as x = tag  (attrs "figure" as) >> x >> etag "figure"
figcaption x = tag  "figcaption" >> x >> etag "figcaption"
figcaption' as x = tag  (attrs "figcaption" as) >> x >> etag "figcaption"
form x = tag  "form" >> x >> etag "form"
form' as x = tag  (attrs "form" as) >> x >> etag "form"
h_head x = tag  "head" >> x >> etag "head"
h_head' as x = tag  (attrs "head" as) >> x >> etag "head"
html x = tag  "html" >> x >> etag "html"
html' as x = tag  (attrs "html" as) >> x >> etag "html"
img x = tag  "img" >> x >> etag "img"
img' as x = tag  (attrs "img" as) >> x >> etag "img"
ins x = tag  "ins" >> x >> etag "ins"
ins' as x = tag  (attrs "ins" as) >> x >> etag "ins"
li x = tag  "li" >> x >> etag "li"
li' as x = tag  (attrs "li" as) >> x >> etag "li"
meta x = tag  "meta" >> x >> etag "meta"
meta' as x = tag  (attrs "meta" as) >> x >> etag "meta"
ol x = tag  "ol" >> x >> etag "ol"
ol' as x = tag  (attrs "ol" as) >> x >> etag "ol"
option x = tag  "option" >> x >> etag "option"
option' as x = tag  (attrs "option" as) >> x >> etag "option"
p x = tag  "p" >> x >> etag "p"
p' as x = tag  (attrs "p" as) >> x >> etag "p"
pre x = tag  "pre" >> x >> etag "pre"
pre' as x = tag  (attrs "pre" as) >> x >> etag "pre"
select x = tag  "select" >> x >> etag "select"
select' as x = tag  (attrs "select" as) >> x >> etag "select"
small x = tag  "small" >> x >> etag "small"
small' as x = tag  (attrs "small" as) >> x >> etag "small"
span x = tag  "span" >> x >> etag "span"
span' as x = tag  (attrs "span" as) >> x >> etag "span"
style x = tag  "style" >> x >> etag "style"
style' as x = tag  (attrs "style" as) >> x >> etag "style"
table x = tag  "table" >> x >> etag "table"
table' as x = tag  (attrs "table" as) >> x >> etag "table"
td x = tag  "td" >> x >> etag "td"
td' as x = tag  (attrs "td" as) >> x >> etag "td"
tbody x = tag  "tbody" >> x >> etag "tbody"
tbody' as x = tag  (attrs "tbody" as) >> x >> etag "tbody"
tfoot x = tag  "tfoot" >> x >> etag "tfoot"
tfoot' as x = tag  (attrs "tfoot" as) >> x >> etag "tfoot"
th x = tag  "th" >> x >> etag "th"
th' as x = tag  (attrs "th" as) >> x >> etag "th"
thead x = tag  "thead" >> x >> etag "thead"
thead' as x = tag  (attrs "thead" as) >> x >> etag "thead"
tr x = tag  "tr" >> x >> etag "tr"
tr' as x = tag  (attrs "tr" as) >> x >> etag "tr"
h_title x = tag  "title" >> x >> etag "title"
h_title' as x = tag  (attrs "title" as) >> x >> etag "title"
ul x = tag  "ul" >> x >> etag "ul"
ul' as x = tag  (attrs "ul" as) >> x >> etag "ul"

a0 = tag "a /"
a0' as = tag  (attrs "a" as ++ " /")
br = tag "br /"
br' as = tag  (attrs "br" as ++ " /")
hr = tag "hr /"
hr' as = tag  (attrs "hr" as ++ " /")
img0 = tag "img /"
img0' as = tag  (attrs "img" as ++ " /")
input = tag "input /"
input' as = tag  (attrs "input" as ++ " /")
td0 = tag "td /"
td0' as = tag  (attrs "td" as ++ " /")
th0 = tag "th /"
th0' as = tag  (attrs "th" as ++ " /")

abbr x = tag  "abbr" >> str x >> etag "abbr"
abbr' as x = tag  (attrs "abbr" as) >> str x >> etag "abbr"
b x = tag  "b" >> str x >> etag "b"
b' as x = tag  (attrs "b" as) >> str x >> etag "b"
dd x = tag  "dd" >> str x >> etag "dd"
dd' as x = tag  (attrs "dd" as) >> str x >> etag "dd"
dt x = tag  "dt" >> str x >> etag "dt"
dt' as x = tag  (attrs "dt" as) >> str x >> etag "dt"
h1 x = tag  "h1" >> str x >> etag "h1"
h1' as x = tag  (attrs "h1" as) >> str x >> etag "h1"
h2 x = tag  "h2" >> str x >> etag "h2"
h2' as x = tag  (attrs "h2" as) >> str x >> etag "h2"
h3 x = tag  "h3" >> str x >> etag "h3"
h3' as x = tag  (attrs "h3" as) >> str x >> etag "h3"
h4 x = tag  "h4" >> str x >> etag "h4"
h4' as x = tag  (attrs "h4" as) >> str x >> etag "h4"
h5 x = tag  "h5" >> str x >> etag "h5"
h5' as x = tag  (attrs "h5" as) >> str x >> etag "h5"
h6 x = tag  "h6" >> str x >> etag "h6"
h6' as x = tag  (attrs "h6" as) >> str x >> etag "h6"
i x = tag  "i" >> str x >> etag "i"
i' as x = tag  (attrs "i" as) >> str x >> etag "i"
legend x = tag  "legend" >> str x >> etag "legend"
legend' as x = tag  (attrs "legend" as) >> str x >> etag "legend"
li_s x = tag  "li" >> str x >> etag "li"
li_s' as x = tag  (attrs "li" as) >> str x >> etag "li"
p_s x = tag  "p" >> str x >> etag "p"
p_s' as x = tag  (attrs "p" as) >> str x >> etag "p"
q x = tag  "q" >> str x >> etag "q"
q' as x = tag  (attrs "q" as) >> str x >> etag "q"
sub x = tag  "sub" >> str x >> etag "sub"
sub' as x = tag  (attrs "sub" as) >> str x >> etag "sub"
super x = tag  "super" >> str x >> etag "super"
super' as x = tag  (attrs "super" as) >> str x >> etag "super"
textarea x = tag  "textarea" >> str x >> etag "textarea"
textarea' as x = tag  (attrs "textarea" as) >> str x >> etag "textarea"

c_class x = attr "class" x
id x = attr "id" x
title x = attr "title" x
xmlns x = attr "xmlns" x
checked x = attr "checked" x
http_equiv x = attr "http_equiv" x
accesskey x = attr "accesskey" x
action x = attr "action" x
align x = attr "align" x
alt x = attr "alt" x
border x = attr "border" x
cite x = attr "cite" x
content x = attr "content" x
enctype x = attr "enctype" x
height x = attr "height" x
href x = attr "href" x
maxlength x = attr "maxlength" x
method x = attr "method" x
multiple x = attr "multiple" x
name x = attr "name" x
width x = attr "width" x
required x = attr "required" x
rows x = attr "rows" x
selected x = attr "selected" x
size x = attr "size" x
src x = attr "src" x
start x = attr "start" x
c_style x = attr "style" x
tabindex x = attr "tabindex" x
c_type x = attr "type" x
value x = attr "value" x

input_checkbox as = tag  (attrs "input" ("type=\"checkbox\"":as) ++ " /")
input_file as = tag  (attrs "input" ("type=\"file\"":as) ++ " /")
input_hidden as = tag  (attrs "input" ("type=\"hidden\"":as) ++ " /")
input_image as = tag  (attrs "input" ("type=\"image\"":as) ++ " /")
input_password as = tag  (attrs "input" ("type=\"password\"":as) ++ " /")
input_radio as = tag  (attrs "input" ("type=\"radio\"":as) ++ " /")
input_search as = tag  (attrs "input" ("type=\"search\"":as) ++ " /")
input_submit as = tag  (attrs "input" ("type=\"submit\"":as) ++ " /")
input_text as = tag  (attrs "input" ("type=\"text\"":as) ++ " /")

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

