
import System.Environment
import System.Random
import Data.Char

main = getArgs >>= mapM (load) >>= rand . concat >>= eval

load = (>>= return . map trim . splitBy '%') . readFile

eval = putStrLn -- replace $VARIABLES in here!

rand list = getStdRandom (randomR (0, (length list) - 1)) >>= return . (!!) list

splitBy _ [] = []
splitBy sep string =
	[takes] ++ splitBy sep drops
	where
		takes = takeWhile (sep /=) string
		drops = dropWhile (sep ==) (drop (length takes) string)

trim = reverse . dropWhile (isSpace) . reverse . dropWhile (isSpace)
