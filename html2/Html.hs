--
-- PROGRAM IS GENERATED - DO NOT ALTER BY HAND
--

module Html (
    a
    ,abbr
    ,b
    ,blockquote
    ,body
    ,br
    ,del
    ,div
    ,form
    ,h_head
    ,html
    ,h1
    ,h2
    ,h3
    ,h4
    ,h5
    ,h6
    ,hr
    ,i
    ,img
    ,input
    ,ins
    ,li
    ,meta
    ,ol
    ,p
    ,pre
    ,q
    ,small
    ,span
    ,style
    ,sub
    ,super
    ,table
    ,td
    ,tr
    ,h_title
    ,ul

    ,h_class
    ,id
    ,title
    ,xmlns
    ,http_equiv
    ,accesskey
    ,action
    ,align
    ,alt
    ,border
    ,cite
    ,content
    ,height
    ,href
    ,maxlength
    ,method
    ,name
    ,width
    ,size
    ,src
    ,start
    ,tabindex
    ,h_type
    ,value

    ,str
    ,(!)
    ,(-)
    ,Logger (..)
) where

import Prelude hiding (div,id,span,(-) )
import Data.List hiding (span)

------ Functions --------------
a x = tell "<a>" >> x >> tell "</a>"
abbr x = tell "<abbr>" >> x >> tell "</abbr>"
b x = tell "<b>" >> x >> tell "</b>"
blockquote x = tell "<blockquote>" >> x >> tell "</blockquote>"
body x = tell "<body>" >> x >> tell "</body>"
br x = tell "<br>" >> x >> tell "</br>"
del x = tell "<del>" >> x >> tell "</del>"
div x = tell "<div>" >> x >> tell "</div>"
form x = tell "<form>" >> x >> tell "</form>"
h_head x = tell "<h_head>" >> x >> tell "</h_head>"
html x = tell "<html>" >> x >> tell "</html>"
h1 x = tell "<h1>" >> x >> tell "</h1>"
h2 x = tell "<h2>" >> x >> tell "</h2>"
h3 x = tell "<h3>" >> x >> tell "</h3>"
h4 x = tell "<h4>" >> x >> tell "</h4>"
h5 x = tell "<h5>" >> x >> tell "</h5>"
h6 x = tell "<h6>" >> x >> tell "</h6>"
hr x = tell "<hr>" >> x >> tell "</hr>"
i x = tell "<i>" >> x >> tell "</i>"
img x = tell "<img>" >> x >> tell "</img>"
input x = tell "<input>" >> x >> tell "</input>"
ins x = tell "<ins>" >> x >> tell "</ins>"
li x = tell "<li>" >> x >> tell "</li>"
meta x = tell "<meta>" >> x >> tell "</meta>"
ol x = tell "<ol>" >> x >> tell "</ol>"
p x = tell "<p>" >> x >> tell "</p>"
pre x = tell "<pre>" >> x >> tell "</pre>"
q x = tell "<q>" >> x >> tell "</q>"
small x = tell "<small>" >> x >> tell "</small>"
span x = tell "<span>" >> x >> tell "</span>"
style x = tell "<style>" >> x >> tell "</style>"
sub x = tell "<sub>" >> x >> tell "</sub>"
super x = tell "<super>" >> x >> tell "</super>"
table x = tell "<table>" >> x >> tell "</table>"
td x = tell "<td>" >> x >> tell "</td>"
tr x = tell "<tr>" >> x >> tell "</tr>"
h_title x = tell "<h_title>" >> x >> tell "</h_title>"
ul x = tell "<ul>" >> x >> tell "</ul>"

h_class val = atag "class" val
id val = atag "id" val
title val = atag "title" val
xmlns val = atag "xmlns" val
http_equiv val = atag "http_equiv" val
accesskey val = atag "accesskey" val
action val = atag "action" val
align val = atag "align" val
alt val = atag "alt" val
border val = atag "border" val
cite val = atag "cite" val
content val = atag "content" val
height val = atag "height" val
href val = atag "href" val
maxlength val = atag "maxlength" val
method val = atag "method" val
name val = atag "name" val
width val = atag "width" val
size val = atag "size" val
src val = atag "src" val
start val = atag "start" val
tabindex val = atag "tabindex" val
h_type val = atag "type" val
value val = atag "value" val

data Lstr = Tag String | Attr String  deriving Show
newtype Logger a = Logger {runLogger :: (a,[Lstr])} deriving (Show)

instance Monad Logger where
    return a = Logger (a,[])
    Logger(a,w) >>= f =
        let Logger (b,w') = f a
        in Logger (b,w++w')

tell s = Logger ((),[Tag s])
tella f s = (Logger ((),[Attr s]) >> f)

str x = tell x

infixr 0 -
(-) f x = f x
(!) f s = tella f s

atag cls val = ""

