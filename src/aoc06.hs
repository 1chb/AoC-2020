import Data.List (groupBy, intersect)
import System.Environment (getArgs)
import System.IO

main = do
  [fname] <- getArgs
  withFile fname ReadMode $ \file -> do
    dat <- hGetContents file
    let groups = group (lines dat)
    print groups
    let answers = foldr1 intersect <$> groups
    print answers
    print $ sum $ length <$> answers

group :: [String] -> [[String]]
group = filter (/= [""]) . groupBy (\a b -> length a > 0 && length b > 0)
