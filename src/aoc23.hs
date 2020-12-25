{-# LANGUAGE NumericUnderscores #-}
import Control.Monad (forM_)
import qualified Data.Array as A
import Data.Array.IO (IOArray, newArray_, readArray, writeArray)
import Data.List (elemIndex)
import qualified Data.Map as M
import System.IO (putStrLn)

main = do
  puzzle <- p "Puzzle" $ read . (:[]) <$> (if True then "643719258" else "389125467") :: IO [Int]
  let sz1 = 100
  p1L <- p "Part 1 (List)" $ concatMap show $ playList sz1 puzzle
  p1A <- p "Part 1 (Arr) " $ concatMap show $ arrCollect $ playArray sz1 puzzle
  p1M <- p "Part 1 (Map) " $ concatMap show $ mapCollect $ playMap sz1 puzzle
  p1U <- playMutableArray sz1 puzzle >>= mutCollect >>= p "Part 1 (mUt)" . concatMap show
  p "Verify algorithms" (p1L == p1A, p1L == p1M, p1L == p1U)

  let bigPuzzle = take 1_000_000 $ puzzle ++ [maximum puzzle+1 ..]
  
  -- myTwoStarsM <- playMutableArray 10_000_000 bigPuzzle >>= mutCollect >>= p "Play Big" . take 2
  -- Mutable array is not much faster than map!
  myTwoStarsU <- playMutableArray 10_000_000 bigPuzzle >>= mutCollect >>= p "Play Big" . take 2
  -- p "Verify algorithms" (myTwoStarsM == myTwoStarsU)
  p "Part 2" $ product myTwoStarsU

playList :: Int -> [Int] -> [Int]
playList n puzzle = go 1 puzzle where
  (mini, maxi) = (minimum puzzle, maximum puzzle)
  go k (current : circle) =
    let (crab, rest) = splitAt 3 circle
        dest : _ = [d | d <- dests current, d `notElem` crab]
        restCur = rest ++ [current]
        Just destIx = elemIndex dest restCur
        (before, after) = splitAt (destIx+1) restCur
    in if k < n
       then go (k+1) (before ++ crab ++ after)
       else let final = before ++ crab ++ after
                Just oneIx = elemIndex 1 final
                (back, 1:front) = splitAt oneIx final
            in front ++ back
  dests c = k2 : dests k2 where
    k = c - 1
    k2 = if k < mini then maxi else k

-- Simulate a circle of cups with an array of cups arranged so that a cup labelled c is at
-- index c and that location contains the label of the next cup in the circle, e.g.  the
-- circle (>3->8->9->1->2->5->4->6->7-) is represented as [2,5,8,6,4,7,3,9,1] because:
-- 1->2, 2->5, 3->8, 4->6, 5->4, 6->7, 7->3, 8->9, and 9->1. Now it is O(1) to: locate a cup,
-- remove the following three cups, insert them after the dest, and find the next current.
type Arr = A.Array Int Int
playArray :: Int -> [Int] -> Arr
playArray n puzzle = go 1 (head puzzle) arr where
  u = length puzzle
  arr = A.array (1,u) $ zip (last puzzle : puzzle) puzzle
  go :: Int -> Int -> Arr -> Arr
  go k c arr -- This is still not sufficient to solve Part 2.
    | k > n = arr
    | otherwise =
        go (k+1) (arr' A.! c) arr' where
        arr' = arr A.// [(c, arr A.! p2), (p2, arr A.! d), (d, p0)]
        (p0, p1, p2) = (arr A.! c, arr A.! p0, arr A.! p1)
        d = dest (c-1)
        dest d | d==p0 || d==p1 || d==p2 = dest (d-1)
               | d==0 = dest u
               | otherwise = d

arrCollect :: Arr -> [Int]
arrCollect arr = go (arr A.! 1) where
  go 1 = []
  go k = k : go (arr A.! k)

type Map = M.Map Int Int
playMap :: Int -> [Int] -> Map
playMap n puzzle = go 1 (head puzzle) map where
  u = length puzzle
  map = M.fromList $ zip (last puzzle : puzzle) puzzle
  go :: Int -> Int -> Map -> Map
  go k c map
    | k > n = map
    | otherwise = go (k+1) (map' M.! c) map' where
        map' = M.insert c (map M.! p2) $ M.insert p2 (map M.! d) $ M.insert d p0 map
        (p0, p1, p2) = (map M.! c, map M.! p0, map M.! p1)
        d = dest (c-1)
        dest d | d==p0 || d==p1 || d==p2 = dest (d-1)
               | d==0 = dest u
               | otherwise = d

mapCollect :: Map -> [Int]
mapCollect map = go (map M.! 1) where
  go 1 = []
  go k = k : go (map M.! k)

type Mut = IOArray Int Int
playMutableArray :: Int -> [Int] -> IO Mut
playMutableArray n puzzle = do
  ma <- newArray_ (1,u)
  forM_ (zip (last puzzle : puzzle) puzzle) $ uncurry (writeArray ma)
  replicateWith n (head puzzle) $ \c -> do
    p0 <- readArray ma c
    p1 <- readArray ma p0
    p2 <- readArray ma p1
    let d = dest (c-1)
        dest d | d==p0 || d==p1 || d==p2 = dest (d-1)
               | d==0 = dest u
               | otherwise = d
    readArray ma p2 >>= writeArray ma c
    readArray ma d >>= writeArray ma p2
    writeArray ma d p0
    readArray ma c
  return ma where
    u = length puzzle

replicateWith  :: (Monad m) => Int -> a -> (a -> m a) -> m ()
replicateWith cnt0 x f = loop cnt0 x where
  loop cnt x
    | cnt <= 0  = return ()
    | otherwise = f x >>= loop (cnt - 1)

mutCollect :: Mut -> IO [Int]
mutCollect arr = readArray arr 1 >>= go where
  go :: Int -> IO [Int]
  go 1 = return []
  go k = (k :) <$> (readArray arr k >>= go)

p :: (Show x) => String -> x -> IO x
p label x = do putStrLn $ label ++ ": " ++ show x; return x
