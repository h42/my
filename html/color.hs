import Data.Char
import Data.List
import Html
import Prelude hiding (id,div,span)

cellout red green blue = "<td style=\"color:"  ++ hc
      ++ "\">" ++ hc ++ "</td>" where
    (hc,hc') = hexout red green blue

hexout red green blue = (h,h') where
    h = "#" ++ hex red ++ hex green ++ hex blue
    h' = "#" ++ hex (255-red) ++ hex (255-green) ++ hex (255-blue)

hex x = sx where
    sx = [hexc (quot x 16), hexc (mod x 16) ]
    hexc x' | x'<10 = chr $ (ord '0') + x'
	    | otherwise = chr $ (ord 'A') + x' - 10

colors = [0,63,127]
cs = [(x,y,z) | x<-colors,y<-colors,z<-colors]
cs1 = map
 (\c'->unlines $ map (\(x,y,z)->cellout x y z) (filter (\(x',_,_)->x'==c') cs))
 colors

myhtml = do
    h1' [c_style "color:red"]  "Color Test Program"

    table $ mapM_ (tr . str) cs1

main = do
    putStrLn $ render (def_page "" "color" myhtml)

