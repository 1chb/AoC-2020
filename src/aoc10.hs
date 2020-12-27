{-# LANGUAGE TupleSections #-}
import qualified Data.Map as M
import Data.List (sort)

main = do
  dat <- readFile "aoc10.dat"
  let nums' = sort $ read <$> words dat
  let nums = nums' ++ [last nums' + 3]
  print $ uncurry (*) $ part1 nums
  print $ snd         $ part2 M.empty 0 nums
  print $ part2List $ nums
  print $ part2Map  $ nums

part1 :: [Int] -> (Int, Int)
part1 = snd . foldl acc (0,(0,0))
  where
    acc (k, (a,c)) x = (x, case x-k of 1->(a+1, c);   3->(a, c+1);   _->(a, c))

type Map = M.Map [Int] Int
part2 :: Map -> Int -> [Int] -> (Map, Int)
part2 m _ [] = (m, 1)
part2 m k xs =
  maybe (store $ foldr acc (m, 0) $ zip [1..] $ takeWhile (<=k+3) xs) (m,) $ M.lookup xs m
  where
    store (m, t) = (M.insert xs t m, t)
    acc (n, y) (m, t) = (+t) <$> part2 m y (drop n xs)

part2List :: [Int] -> Int -- Ordered [Jolt] -> Permutations
part2List js = last ps where -- Lazy memo:
  ps = 1 : [sum $ map snd $ takeWhile ((<j).fst) $ dropWhile ((<j-3).fst) $ zip (0 : js) ps | j <- js]

part2Map :: [Int] -> Int -- Ordered [Jolt] -> Permutations
part2Map js = snd $ M.findMax pm where
  pm = M.fromDistinctAscList $ (0,1) : zip js (perm <$> js)
  perm j = sum $ snd $ M.split (j-4) $ fst $ M.split j pm
