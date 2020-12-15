import System.Environment (getArgs)
import Data.List.Split (wordsBy)
import qualified Data.Map as M
import System.IO

main = do
  [filename] <- getArgs
  withFile filename ReadMode $ \file -> do
    input <- map read . wordsBy (==',') <$> hGetContents file :: IO [Int]
    let len = length input
    let (inpu,[t]) = splitAt (len-1) input
    let map = M.fromList $ zip inpu [1..] :: Map
    let many = inpu ++ next len t map
    print $ take 10 many
    print $ many !! (2020-1)
    print $ many !! (30000000-1)
    
type Map = M.Map Int Int

-- Result(big)=2159626, time=86.892u 4.871s 1:32.05 99.6%	0+0k 2352+0io 2pf+0w
next :: Int -> Int -> Map -> [Int]
next ix last map = last : next (ix+1) (maybe 0 (ix-) (M.lookup last map)) (M.insert last ix map)
