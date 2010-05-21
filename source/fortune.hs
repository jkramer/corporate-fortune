
import System.Environment
import System.Random

import Data.List.Split
import Data.String.Utils
import Data.Char

import Control.Monad
import Control.Arrow

main = getArgs >>= mapM load >>= rand . concat >>= eval

load = fmap (map strip . splitOn "%") . readFile

eval s =
    ioVarMap (varList s) >>= putStrLn . foldl (\ s (v, r) -> replace v r s) s
    where
        safeEnv v = catch (getEnv v) (return . const ('$' : v))
        ioVarMap = mapM (\ v -> liftM ((,) ('$' : v)) (safeEnv v))



varList [] = []
varList ('$':xs) =
    case takeWhile (\ x -> isUpper x && isAlpha x) xs of
        "" -> varList xs
        name -> name : varList (drop (length name) xs)

varList (_:xs) = varList xs

rand [] = error "no fortunes"
rand xs = liftM ((!!) xs) (getStdRandom (randomR (0, length xs - 1)))
