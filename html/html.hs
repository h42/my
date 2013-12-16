import Html
import Prelude hiding(id,div,span)

--
-- MAIN
--
main = do
    putStrLn $ render (def "test" yyy)
    {-
    let Stack xs y = (evalState yyy (Stack [] 0))
    mapM print xs
    -}

def mytitle mybody  = do
    str def_http_hdr
    str def_doctype
    html $ do
	h_head $ h_title $ str mytitle
	body $ mybody

yyy = do
    div' [id "h1div"] $ h1 "HCGI Demo Program"
    toc

    ulist ["one","two","three"]
    nestedlist
    deflist

    atable
    simple
    preformat
    biggy
    image
    str "all done"

--
-- Table of COntents
--
toc = do
    div' [id "toc"] $ do
	h3 "Table of Contents"
	p $ do
	  blockquote $ do
	    a' [href "../algorithms.pdf"] $ do
		str "algorithms.pdf"
	    br
	    a' [href "#simple"] $ str"Pdf download"
	    br
	    a' [href "#simple"] $ str"Simple Formatting"
	    br
            a' [href "#preformatted"] $ str "Preformatted Text"
	    br
	    a' [href "#bigsmall"] $ str "Big / Small Text"
	    br
	    a' [href "#list"] $ str "List"
	    br
	    a' [href "#table"] $ str "Table"
	    br
	    a' [href "#images"] $ str "Images"
	    br

	a' [href "http://stackoverflow.com/questions/tagged/haskell"
	      ,accesskey "o", tabindex "2"] $ do
	    str "A link to "
	    b "stackoverflow"
	br

	a' [href "http://www.reddit.com/r/haskell/"
	       ,accesskey "r",  tabindex "1"] $ do
	    str "A link to "
	    b "reddit/r/haskell"

      {-
	a ! href "http://http://ALG_3rd.pdf"
	s "A link to a" >> b >> s"PDF" >> b_ >> s"on my computer"
	a_ >> br
      -}

--
-- NestedList
--
ulist xs = do
    --a0' [id "list"]
    div' [id "list"] $ do
	h3 "Recursive List Example"
	ol $  ulist' xs

ulist' [] = return ()
ulist' (x:xs) = do
    li_s x
    ulist' xs

--
-- List
--
nestedlist = do
    --a0' [id "list"]
    div' [id "list"] $ do
	h3 "List Example"
	ul $ do
	  p $ do
	    li $ do
		str "This is an outer list item"
		ol $ do
		    li_s "This is a list item"
		    li_s "This is a list item"
		    li_s "This is a list item"
	  p $ do
	    li $ do
		str "This is an outer list item"
		ol $ do
		    li_s "This is a list item"
		    li_s "This is a list item"
		    li_s "This is a list item"
	  p $ do
	    li $ do
		str "This is an outer list item"
		ol $ do
		    li_s "This is a list item"
		    li_s "This is a list item"
		    li_s "This is a list item"

--
-- Definition List
--
deflist = do
    h3 "Def List Example"
    dl $ do
	dt "one"
	dd "definition one"
	dt "two"
	dd "definition two"
	dt "three"
	dd "definition three"

--
-- TABLE
--
atable = do
    --a0' [id "table"]
    div' [id "table"] $ do
	h3 "Table Example"
	table $ do
	    thead $ do
		tr $ do
		    th0
		    th $ str "head 1"
		    th $ str "head 2"
	    tbody $ do
		tr $ do
		    th $ str "row head 1"
		    td $ str "table 1 1"
		    td $ str "table 1 2"
		tr $ do
		    th $ str "row head 2"
		    td $ str"table 2 1"
		    td $ str"table 2 2"
	    tfoot $ do
		tr $ do
		    th0
		    th $ str "foot 1"
		    th $ str "foot 2"


	h3 "Long Table Example"
	table $ do
	    thead $ do
		tr $ do
		    td $ str "head 1"
		    td $ str "head 2"
	    tbody $ do
		tr $ do
		    td $ str "inline data 0 1"
		    td $ str "inline data 0 2"
		longtable 1 10
	    tfoot $ do
		tr $ do
		    td $ str "foot 1"
		    td $ str "foot 2"


longtable ind n
    | ind > n = return ()
    | otherwise = do
	tr $ do
	    td $ str $"func data " ++ show ind ++ " 1"
	    td $ str $"func data " ++ show ind ++ " 2"
	longtable (ind+1) n

--
-- SIMPLE
--
simple = do
    --a0'[id "simple"]
    div' [id "simple"] $ do
	h3 "Simple Formatting"
	p' [c_class "par1"] $ do
	    str "This is the first paragraph. we can"
	    b "bold"
	    str "things or"
	    i "italicize things."
	    br
	    str "This sample program will eventually show a great many features"
	    str "of html"
    
	    span' [c_class "span 1"] $ do
		br
		str "This sentence is enclosed in a span which we can format separately"
		br

	p $ do
	    mapM_ (\_ -> do
		str "This sentence is just here to fill up space so we can test links"
		br) [1..5]


--
-- PREFORMAT
--
preformat = do
    --a0' [id "preformatted"]
    div' [id "preformatted"] $ do
	h3 "Preformatted Text"
	p $ do
	    str "This paragraph will be used to demonstrate preformatted code."
	    str "like this code snippet"
	    pre $ do
		mycode
mycode = do
    str $ unlines [
	 "close2 :: String -> [String] -> [String] -> ([String],[String])"
	,"close2 s tags []  = (tags,[])"
	,"close2 s (e:es) tags"
	,"    | s == e    = ((etagit s):tags,es)"
	,"    | otherwise = close2 s es (etagit s:tags)"
	]

--
-- BIGGY
--
biggy = do
    --a0' [id "bigsmall"]
    div' [id "bigsmall"] $ do
	p $ do
	    h3 "Demonstrate big/small"
	    small $ str "this is small line 1."
	    br
	    small $ str "this is small line 2."
	p $ do
	    br >> str "Normal line 1"
	    br >> str "Normal line 2"
	p $ do
	    br >> str "Normal line 3"
	p $ do
	    let sval = "this will be big line 1 when I set BIG style"
	    br
	    span' [c_class "bigtext"] $ str sval


--
-- IMAGE
--
image = do
    --a0' [id "images"]
    div' [id "images"] $ do
	h3 "Demonstrate Simple Images"
	figure $ do
	    figcaption $ str "This is a figcaption."
	    img0' [src "../plumber.jpg", alt "plumber.jpg"
		 ,width "150", height  "180", title "Plumber"]
	    img0' [src "../sara.jpg", alt "sara.jpg"
		 ,width "150", height  "180", title "Plumber"]
	    figcaption $ str "This is a figcaption"

