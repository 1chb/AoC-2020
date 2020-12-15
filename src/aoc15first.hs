import System.Environment (getArgs)
import Data.List.Split (wordsBy)
import qualified Data.Map as M
import System.IO

main = do
  [filename] <- getArgs
  withFile filename ReadMode $ \file -> do
    input <- map read . wordsBy (==',') <$> hGetContents file :: IO [Int]
    let map = M.fromList $ zip input $ zip [1..] [0,0..] :: Map
    let len = length input

    print $ next (2020-len) (last input) (len+1) map
    print $ next (30000000-len) (last input) (len+1) map

type Map = M.Map Int (Int,Int)

next :: Int -> Int -> Int -> Map -> Int
next 0 last ix map = last
next n last ix map = case M.lookup last map of
  Just (a,0) -> next (n-1) 0 (ix+1) $ M.alter f 0 map
  Just (a,b) -> next (n-1) (a-b) (ix+1) $ M.alter f (a-b) map
  Nothing -> error $ show last ++ " should be in the map..."
  where f (Just (a,b)) = Just (ix,a)
        f Nothing = Just (ix,0)


