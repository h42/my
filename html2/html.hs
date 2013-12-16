import Prelude hiding ((-))
import Html


myhtml = do
    (body ! "a1") - do
	p - do
	    str "rrr"
	    str "rrr"

main = do
    --putStrLn $ unlines $ snd $ runLogger myhtml
    print myhtml
    --print $ runLogger lf2
    --print $ snd $ runLogger lf2
