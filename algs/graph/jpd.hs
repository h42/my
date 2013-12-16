import qualified Data.Map as M
import Data.Map ((!))

getter fn = do
    vs0 <- fmap (\f -> map words (lines f)) (readFile fn)
    let vs = getVerts (concat vs0) 0 M.empty
        es = concatMap f vs0
        f [x,y] = [(x',y'),(y',x')] where (x',y') = (vs ! x, vs ! y)
    print vs
    print es

getVerts [] i m = m
getVerts (x:xs) i m = case (M.lookup x m) of
    Nothing -> getVerts xs (i+1) (M.insert x i m)
    Just _ -> getVerts xs i m

main = do
    getter "g1.dat"
    print 42
