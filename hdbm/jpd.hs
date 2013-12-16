import System.IO.Error
import My.Hdbm

loader db i = hdbmInsert db ("key " ++ show i) ("value " ++ show i)

modder db i = hdbmUpdate db ("key " ++ show i) ("update " ++ show i)

remmer db i = hdbmRemove db ("key " ++ show i)

getter db i = do
    catchIOError (hdbmGet db ("key " ++ show i)) (\e->return $ show e)
         >>= print

getall db i n = do
    rc <- if i==0 then hdbmFirst db else hdbmNext db
    print rc
    getall db (i+1) n

main = do
    db <- hdbmOpen "temp.dbm" "n" 0
    print db
    let n = 8
    mapM (loader db) [1..n]
    remmer db 2
    modder db $quot n 2
    putStrLn "\n"
    mapM (getter db) [1..n]
    putStrLn "\n"
    catchIOError (getall db 0 22) (\e->return ())
    hdbmClose db
