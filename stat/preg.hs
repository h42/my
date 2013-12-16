import Data.Char
import Control.Monad
import qualified Data.ByteString.Char8 as B
import Stat

getField :: B.ByteString -> Int -> Int -> String
getField bs x1 x2 = B.unpack $ B.take (x2-x1+1) $ B.drop (x1-1) bs

caseid bs = read (getField bs 1 12) :: Int
prglength bs = read (getField bs 275 276) :: Int
outcome bs = read (getField bs 277 277) :: Int
birthord bs = ans where
    bo = getField bs 278 279
    ans | any isDigit bo = read bo :: Int
        | otherwise = 0
finalwgt bs = read (getField bs 423 440) :: Float

disp bs = do
    print $ show (caseid bs) ++ " " ++ show (prglength bs)
         ++ " " ++ show (outcome bs) ++ " " ++ show (birthord bs)
         ++ " " ++ show (finalwgt bs)

checker (n1,l1,l1sqr,n2,l2,l2sqr) bs  = do
    let b = birthord bs
        pl = fromIntegral $ prglength bs
    return $
        if (b==1) then (n1+1,l1+pl,l1sqr+pl^2,n2,l2,l2sqr)
        else if (b>1) then (n1,l1,l1sqr,n2+1,l2+pl,l2sqr+pl^2)
        else (n1,l1,l1sqr,n2,l2,l2sqr)

main = do
    preg <- fmap B.lines (B.readFile "2002FemPreg.dat")
    print $ length preg
    mapM_ disp (take 10 preg)
    (n1,l1,l1sqr,n2,l2,l2sqr) <- foldM checker (0,0,0,0,0,0) (take 100000 preg)
    let m1 = l1 / n1
    let m2 = l2 / n2
    let v1 = l1sqr / n1 - m1^2
    let v2 = l2sqr / n2 - m2^2
    print $ (n1, l1, l1 / n1)
    print $ (n2, l2, l2 / n2)
    print $ "variance 1 = " ++ show v1
    print $ "variance 2 = " ++ show v2
    --print $
