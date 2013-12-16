import System.IO
import System.IO.Error
import System.Cmd
import System.Environment
import Control.Applicative
import Control.Monad
import My.Ftp

main = do
    args <- getArgs
    let (cmd,arg) = case args of
	    ["rls",fn]   -> ("rls",fn)
	    ["rrm",fn]   -> ("rrm",fn)
	    [('t':_)]    -> ("test","")
	    _            -> ("","")

    if  cmd /= "" then main2 cmd arg
    else putStrLn "Usage :(rls | rrm | test) arg"

main2 cmd arg = do
    g <- connect "jld2.com" "kbswinge"
    if zrc g == 0 then do
	putStrLn "Connected"
	if cmd == "rls" then do
	    (ans,g') <- pdir arg [] g
	    mapM_ putStrLn ans
	else if cmd == "rrm" then do
	    (ans,g') <- pdir arg [] g
	    mapM_ putStrLn ans
	    putStr $ "Do you really want to dlete directory "++arg++"?(Y/n)"
	    hFlush stdout
	    yorn <- getLine
	    when (yorn=="Y") (rmdir ans g')
	else if cmd == "test" then pgtest g
	else putStrLn "Not implmented"
    else putStrLn "Unable to connect"
    cmd_quit g
    return ()

--
-- PGTEST
--
pgtest g = do
    cmd_mkdir "testdir" g
    cmd_mkdir "testdir/t2" g
    cmd_put "ftp.dat" "testdir/t2/ftp.dat" g
    {-
    cmd_rm "testdir/t2/ftp.dat" g
    cmd_rmdir "testdir/t2" g
    cmd_rmdir "testdir" g
    -}
    main2 "rrm" "testdir"
    return ()

--
-- PDIR
--
pdir dir ans g = do
    putStrLn $ "\nProcessing " ++ dir
    g' <- cmd_list dir "[]" g
    if zrc g' == 0  then pdir2 (lines (zdata g')) dir ans g'
    else return (ans,g')

pdir2 [] dir ans g = return (ans,g)
pdir2 (l:ls) dir ans g = do
    let ws = words l
	wsl = last ws
	wsf = head ws
	dir2 = dir++"/"++wsl
    if null ws || wsl=="." || wsl==".." then pdir2 ls dir ans g
    else do
	if (head wsf) == 'd' then do
	    (ans',g') <- pdir dir2 ans g
	    pdir2 ls dir (ans'++[dir2++"/"]) g'
	else pdir2 ls dir (ans++[dir++"/"++wsl]) g

--
--RMDIR
--
rmdir [] g = return ()
rmdir ("":paths) g = rmdir paths g
rmdir (p:paths) g = do
    if last p == '/' then cmd_rmdir p g
		     else cmd_rm p g
    rmdir paths g


--
-- getPassword
--
getPassword msg = do
    putStr msg
    hFlush stdout
    rawSystem "stty" ["-echo"] *> getLine <* rawSystem "stty" ["echo"]

--
-- CONNECT
--
connect dom user = do
    pw <- catchIOError (getEnv "PW3") (\e -> getPassword "Password => ")
    cmd_connect dom user pw

