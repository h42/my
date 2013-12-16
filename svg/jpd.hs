import Data.List
import My.Svg

myfunc = do
    svg $ do
        --mapM_ myCircle [50,100..250]
        --myRect
        --myRound
        --myEllipse
        --myLine
        --myPolygon
        --myPolyline
        myPath
        myText

myCircle x =
        circle x 150 100 [fill "blue", stroke "black", stroke_width 8]

myEllipse = do
        ellipse 300 500 100 70 [fill "indigo"]
        ellipse 300 500 90 60 [fill "white"]
        ellipse 300 500 80 50 [fill "red"]

myLine = do
        line 300 0 500 200 [stroke "lime", stroke_width 3]

myPath = do
        path ["M150 0", "L75 200", "L225 200","Z"] [stroke "red",fill "none"]

myPolygon = do
        {-
        polygon [(200,10), (250,190), (160,210)]
                [fill "lime", stroke "purple", stroke_width 1]
        polygon [(220,10), (300,210), (170,250), (123,234)]
                [fill "lime", stroke "purple", stroke_width 1]
        polygon [(100,10), (40,180), (190,60), (10,60), (160,180)]
                [fill "lime", stroke "purple", stroke_width 5,
                 fill_rule "nonzero"]
        -}
        polygon [(100,10), (40,180), (190,60), (10,60), (160,180)]
                [fill "lime", stroke "purple", stroke_width 5,
                 fill_rule "evenodd"]

myPolyline = do
    polyline [(20,20),(40,25),(60,40),(80,120),(120,140),(200,180)]
             [fill "none", stroke "black", stroke_width 3]
    polyline [(0,40),(40,40),(40,80),(80,80),(80,120),(120,120),(120,160)]
             [fill "red", stroke "black", stroke_width 3]

myRect = do
        rect 200 50 100 100 [fill "red", stroke "black", opacity 0.5]

myRound = do
        rounded 200 350 20 20 100 100 [fill "red", stroke "black"]

myText = do
    text 0 30 [fill "red",font_size 26] $  str "I Love SVG"
    text' [("x","0"),("y","60")] [fill "red",font_size 26] $  str "I Love SVG"
    text' [("x","0"),("y","90 120 150")] [fill "red",font_size 26] $
      str "HEY"
    text' [("x","0"),("y",positions 180 30 7) ]
          [fill "blue",font_size 16] $ str "HEY NOW"

positions start inc cnt = sx where
    xs = [start,start+inc..start+(cnt-1)*inc]
    sx = intercalate " " (map show xs)

main = do
    putStr $ render myfunc
