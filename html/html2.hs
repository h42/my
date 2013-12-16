import Prelude hiding ((-))

newtype Logger a = Logger {runLogger :: (a,[String])} deriving (Show)

instance Monad Logger where
    return a = Logger (a,[])
    Logger(a,w) >>= f =
	let Logger (b,w') = f a
	in Logger (b,w++w')

tell s = Logger ((),[s])

-----------------
-- DSL
-----------------
infixr 0 -
(-) f x = f x

p x = tell "<p>" >> x >> tell "</p>"
q x = tell "<q>" >> x >> tell "</q>"
r x = tell "<r>" >> x >> tell "</r>"
str x = tell x

html = do
    p - do
	q - str "qqq"
	q - str "qqq"
	r - do
	    str "rrr"
	    str "rrr"

main = do
    putStrLn $ unlines $ snd $ runLogger html
    print html
    --print $ runLogger lf2
    --print $ snd $ runLogger lf2
