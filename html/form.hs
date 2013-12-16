import Data.List
import System.Environment
import Html
import Cgi
--import Prelude hiding (id,div,span)

-- textInput
-- input_text nm len val =
--     input' [c_type "text", name nm, value val, size (show len)]

-- input_vtext nm len cgi = input_text nm len (cgi_var nm cgi)

--
-- Application
--

main2 = do
    s <- cgi_init_mime
    let myheaders=(def_http_hdr ++ "\n" ++ setCookie "jpdform" "jpdformdata" [])
    putStrLn $ render (def_page myheaders "form" (myhtml2 s))

main = do
    cgi <- cgi_init
    let myheaders=(def_http_hdr ++ "\n" ++ setCookie "jpdform" "jpdformdata" [])
    --let myheaders=def_http_hdr
    putStrLn $ render (def_page myheaders "form" (myhtml cgi))

myhtml2 s = do
    h1' ["style=color:#008"] $ "Html Form  Test Program"
    form' [method "post", action "form", enctype "multipart/form-data"] $ do
        p $ input_search [name "user"]
        p $ textarea' [name "comments", rows "4"]  "Hey Now!"
        p $ input_file [name "myfile"]
        p $ input_file [name "myfile2"]
        p $ input_submit [name "sub1", value "upload"]
    --p $ str (show (zvtab cgi)) >> br

myhtml cgi = do
    h1' ["style=color:#008"] $ "Html Form  Test Program"
    p $ do
	form' [method "post", action "form"] $ do
	    input_hidden [name "hidden", value "HIDDEN STUF"]
	    br
	    input_submit [name "sub1", value "sub"]
	    fieldset $ do
		legend "details"
		--input_vtext "input1" 20 cgi                -- ! id "t1"
		input_text [name "user", value (cgi_var "user" cgi)]
		br
		input_password [name "pw", value (cgi_var "pw" cgi)]
		br
		input_text [name "input2", value "b12345"]
	    fieldset $ do
		legend "set 2"
		str "What do you think?"
		br
		textarea' [name "comments", rows "4"]  "Hey Now!"
		textarea' [name "comments2", rows "4"] "111\n222\n"
	    fieldset $ do
		legend "radio buttons"
		input_radio [name "radio", value "button1"]
		str "button1"
		input_radio [name "radio", value "button2", checked "y"]
		str "button2"
	    fieldset $ do
		legend "checkbox"
		input_checkbox [name "box", value "box1"]
		str "box1"
		input_checkbox [name "box", value "box2"]
		str "box2"
	    fieldset $ do
		legend "select"
		select' [name "selector"] $
		    fill_sel (cgi_var "selector" cgi) ["ipad","ipod","imac"]
		select' [name "msel", multiple "y"] $
		    fill_msel (cgi_vars "msel" cgi) ["ipad","ipod","imac"]
	    p $ do
		input_image [name "plumber", src "../plumber.jpg", width "32"]
		str (xypos cgi)

    str (show (zvtab cgi)) >> br
    --str (unlines $ map (\(x,y)->x ++ " = " ++ y ++ "<br />") (zenv cgi)) >> br

xypos cgi = s where
    x = cgi_var "plumber.x" cgi
    y = cgi_var "plumber.y" cgi
    s = x ++ " " ++ y

fill_sel def [] = return ()
fill_sel def (x:xs) = do
    if def == x then
	option' [value x, selected "y"] $ str x
    else
	option' [value x] $ str x
    fill_sel def xs

fill_msel def [] = return ()
fill_msel def (x:xs) = do
    case (elem x def) of
	True -> option' [value x, selected "y"] $ str x
	_    -> option' [value x] $ str x
    fill_msel def xs

