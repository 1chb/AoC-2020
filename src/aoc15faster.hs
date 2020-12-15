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
    
    print $ inpu ++ [next n len t map | n <- [len..10+len]]
    print $ next     2020 len t map
    print $ next 30000000 len t map
    
type Map = M.Map Int Int

-- Result(big)=2159626, time=46.749u 0.538s 0:47.45 99.6%	0+0k 2344+0io 2pf+0w
next :: Int -> Int -> Int -> Map -> Int
next n = go where
  go ix last map
    | ix == n = last
    | otherwise = go (ix+1) (maybe 0 (ix-) (M.lookup last map)) $ M.insert last ix map
