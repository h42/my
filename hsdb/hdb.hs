import Control.Monad
import System.IO
import System.IO.Error
import Hdb

main = do
    let fn = "temp.db"
    return ()

{-
    tryIOError (
        initHdb "temp.db"
        )

    catchIOError ( do
        hdb <- openHdb fn
        print hdb
        b0 <- getblk 0 hdb
	print b0
        putblks 10 hdb
        getblks 10 hdb
        return ()
        )
        (\e -> print $ show e )

getblks 0 h = return $ Right h
getblks 7 h = fail "just checking"
getblks n h = do
    hdb <- getblk (n*4096) h
    putStrLn $ show n ++ ". " ++ show (zstr hdb)
    getblks (n-1) hdb

putblks 0 h = return $ Right h
putblks n h = do
    hdb <- do
        let str = ("record " ++ show n)
        putblk (n*4096) str h
    putblks (n-1) hdb

--instance Eq Key where
--    (==) k1 k2 = (i1 k1) == (i1 k2) && (i2 k1) == (i2 k2)
-}
