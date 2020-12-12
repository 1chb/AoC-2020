import Control.Monad
import Data.List ((\\))
import System.IO

main = do
  withFile "aoc05.dat" ReadMode $ \file -> do
    dat <- hGetContents file
    let nums = map decode $ lines dat
    forM nums print
    print $ maximum $ map sid nums
    print $ map sid $ filter (\rc -> rc > minimum nums && rc < maximum nums) $ [(r,c) | r <- [0..127], c <- [0..7]] \\ nums

sid (r,c) = 8*r+c

decode :: String -> (Int, Int)
decode str = (bin 'B' $ reverse row, bin 'R' $ reverse col)
  where
    (row, col) = splitAt 7 str
    bin h = foldr (\c t -> 2*t + if c==h then 1 else 0) 0
