--
-- XML TEST PROGRAM
--

import MY.Xml

speed_test = do
    let y = "<aaa at1=\"at1data\"> <bb at1 at2=\"123\" at3 > some data </bb> some aaa data </aaa>\n"
	n = 10000
	y' =  "<start>\n" ++ (take (length y * n) $ cycle y) ++  "</start>"
	xxx = xmlParse y'
    case xxx of
	Left x -> print x
	Right x -> putStrLn $ (show $ length x) ++ " - " ++ (show $ length y')

show_xml = do
    let y = "<aaa at1=\"at1data\"> <bb at1 at2=\"123\" at3 > some data </bb> some aaa data </aaa>\n"
    case (xmlParse y) of
	Left e -> putStrLn $ "Left " ++ e
	Right xmls -> mapM_  putStrLn (map show xmls)

{-fixchar_test =
    print $  map (\(_,c,_)->c)
	(fixChar "aaa&lt;bbb&gt;&amp;&amp;&apos;&quot;")
-}

data HP = HP {zuser::String, zpasswd::String, zproto::String,
	      zhost::String, zfiles::[String]} deriving (Show)

inithp = HP "" "" "" "" []

loadhp :: [Xml] -> HP -> HP
loadhp [] hp = hp
loadhp ((XmlTag "archive" atts ):xmls) hp = loadhp xmls (process_atts atts hp)
loadhp ((XmlData "files" files ):xmls) hp = loadhp xmls (process_files files hp)
loadhp (_:xmls) hp = loadhp xmls hp

process_files flist hp = hp{zfiles=(zfiles hp) ++ words flist}

process_atts :: [(String,String)] -> HP -> HP
process_atts [] hp = hp
process_atts ((att,val):atts) hp =
    case att of "user"   -> process_atts atts hp{zuser=val}
		"passwd" -> process_atts atts hp{zpasswd=val}
		"proto"  -> process_atts atts hp{zproto=val}
		"host"   -> process_atts atts hp{zhost=val}
		_        -> process_atts atts hp

--
-- MAIN
--
main = do
    xmlrc <- xmlParseFile "xml.dat"
    case xmlrc of
	Left x -> print xmlrc
	Right xmls -> do
	    mapM_ putStrLn (map show xmls)
	    let hp = loadhp xmls inithp
	    print hp
    --print xmlrc
