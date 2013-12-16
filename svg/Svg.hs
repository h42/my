module My.Svg (
    Stack (..)
    ,stack0
    ,State (..)
    ,evalState
    ,tag
    ,etag
    ,str
    ,render

    ,svg
    ,circle
    ,ellipse
    ,line
    ,polygon
    ,polyline
    ,rect
    ,rounded
    ,text
    ,text'

    ,fill
    ,fill_opacity
    ,fill_rule
    ,font_size
    ,opacity
    ,path
    ,rgb
    ,stroke
    ,stroke_width
    ,stroke_opacity

) where

import Data.List

svg nested = tag "svg xmlns=\"http://www.w3.org/2000/svg\""
                 >> nested >> etag "svg"

text x y styles0 nested = tag (attrs "text" [("x",show x),("y",show y)] styles0)
                            >> nested >> etag "text"

-- text' uses explicit attributes
text' as styles0 nested = tag (attrs "text" as styles0)
                            >> nested >> etag "text"

circle :: (Num a, Show a) => a -> a -> a -> [String] -> State Stack ()
circle cx cy r styles0 =
    tag  (attrs "circle" [("cx",show cx),("cy",show cy),("r",show r)] styles0)
        >>  etag "circle"

ellipse :: (Num a, Show a) => a -> a -> a -> a -> [String] -> State Stack ()
ellipse cx cy rx ry styles0 =
    tag  (attrs "ellipse" [("cx",show cx),("cy",show cy),
                          ("rx",show rx),("ry",show ry)] styles0)
        >>  etag "ellipse"

line x1 y1 x2 y2 styles0 =
    tag  (attrs "line" [("x1",show x1),("y1",show y1),
                          ("x2",show x2),("y2",show y2)] styles0)
        >>  etag "line"

path :: [String] -> [String] -> State Stack ()
path ds styles0 =
    tag  (attrs "path" [("d", intercalate " " ds)] styles0)
        >>  etag "path"


polygon :: (Num a, Show a) => [(a,a)] -> [String] -> State Stack ()
polygon points styles0 =
    tag (attrs "polygon" [("points",ps)] styles0)
        >> etag "polygon"
  where ps = concatMap (showPt) points
        showPt (x,y) = show x ++ "," ++ show y ++ " "

polyline :: (Num a, Show a) => [(a,a)] -> [String] -> State Stack ()
polyline points styles0 =
    tag (attrs "polyline" [("points",ps)] styles0)
        >> etag "polyline"
  where ps = concatMap (showPt) points
        showPt (x,y) = show x ++ "," ++ show y ++ " "

rect :: (Num a, Show a) => a -> a -> a -> a -> [String] -> State Stack ()
rect x y w h styles0 =
    tag  (attrs "rect" [("x",show x),("y",show y),
                        ("width",show w),("height",show h)] styles0)
        >>  etag "rect"

rounded :: (Num a, Show a) => a -> a -> a -> a -> a -> a -> [String] -> State Stack ()
rounded x y rx ry w h styles0 =
    tag  (attrs "rect" [("x",show x),("y",show y),
                        ("rx",show rx),("ry",show ry),
                        ("width",show w),("height",show h)] styles0)
        >>  etag "rect"

fill xs = "fill:" ++ xs
fill_rule xs = "fill-rule:" ++ xs
font_size :: Int -> String
font_size x = "font-size:" ++ show x ++ "px"
opacity :: Float -> String
opacity x = "opacity:" ++ show x
fill_opacity :: Float -> String
fill_opacity x = "fill-opacity:" ++ show x
rgb :: Int -> Int -> Int -> String
rgb x y z = "rgb("  ++  show x  ++  ","  ++  show y  ++  ","  ++  show z  ++  ")"
rotate :: Int -> Int -> Int -> String
rotate x y z = "rotate("  ++  show x  ++  ","  ++  show y  ++  ","  ++  show z  ++  ")"
stroke xs = "stroke:" ++ xs
stroke_opacity :: Float -> String
stroke_opacity x = "stroke-opacity:" ++ show x
stroke_width :: Int -> String
stroke_width x = "stroke-width:" ++ show x

newtype State s a = State { runState :: s -> (a,s) }

instance Monad (State s) where
    return x = State $ \s -> (x,s)
    (State h) >>= f = State $ \s -> let (aa, newState) = h s
					(State g) = f aa
				    in g newState

evalState st st0 = snd $ runState st st0

data Svg = Stag String | Setag String | Sstr String
    deriving Show

data Stack = Stack {
    zsvg :: [Svg]
    ,zint  :: Int
} deriving Show
stack0 = Stack {zsvg=[],zint=0}

str :: String -> State Stack ()
str x = State $ \st -> ((), st {zsvg=Sstr x:zsvg st} )

tag :: String -> State Stack ()
--tag x = State $ \(Stack xs _) -> ((), Stack ( Htag x : xs) undefined)
tag x = State $ \st -> ((), st {zsvg=Stag x:zsvg st} )

etag :: String -> State Stack ()
etag x = State $ \st -> ((), st {zsvg=Setag x:zsvg st} )

attrs t ps as = t ++ " " ++ parms ps ++ " " ++ styles as
attr x y = x ++ "=\"" ++ y ++ "\""

parm (x,y) = x ++ "=\"" ++ y ++ "\" "
parms :: [(String,String)] -> String
parms xs = foldl' (\s y -> s ++ parm y) "" xs

styles :: [String] -> String
styles xs = "style=\""
    ++ (intercalate ";" xs)
    ++ "\""

render st0 = concat $ reverse s where
    stack = evalState st0 stack0
    (s,_) = foldr rend ([],"") (zsvg stack)

    rend ::Svg -> ([String],String) -> ([String],String)
    rend (Stag x) (tags,lev) = ((t1:tags), lev) where
	t1 = lev ++ ('<' : x) ++ ">\n"
    rend (Setag x) (tags,lev) = (((lev ++ "</" ++ x ++ ">\n") : tags), lev)
    rend (Sstr x) (tags,lev) = ( ( (lev ++ x ++ "\n") : tags), lev)
    
