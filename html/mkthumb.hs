import Data.List
import System.Cmd
import System.Environment
import System.Directory

--convert sara.jpg    -resize 256x256  resize_sara.jpg

cmdx fn x y = "convert " ++ ifn ++ " -resize "
    ++ tn ++ " " ++ ofn
  where
    ifn = if sufl > 0 then fn else fn ++ ".jpg"
    ofn = if sufl==0  then fn ++ "_" ++ tn ++ ".jpg"
	  else take (fnl-4) fn ++ "_" ++ tn ++ ".jpg"
    tn = x ++ "x" ++ y
    fnl = length fn
    sufl = if (any ((flip isSuffixOf) fn) [".jpg",".JPG"] ) then 4 else 0

cmd fn = "convert -define jpeg:size=500x180 "
    ++ ifn
    ++ " -auto-orient -thumbnail 250x90 -unsharp 0x.5 "
    ++ ofn
  where
    ifn = if sufl > 0 then fn else fn ++ ".jpg"
    ofn = if sufl==0  then fn ++ ".th.gif"
	  else take (fnl-4) fn ++ ".th.gif"
    fnl = length fn
    sufl = if (any ((flip isSuffixOf) fn) [".jpg",".JPG"] ) then 4 else 0

main = do
    args <- getArgs
    case args of
	[fn] -> do
	    let s = cmd fn
	    print s
	    system $ cmd fn
	    return ()
	[fn,x,y] -> do
	    let s = cmdx fn x y
	    putStrLn s
	    --system $ cmd fn
	    return ()
	_ ->  putStrLn "Usage: mkthumb <Filename> [ x y ]"

