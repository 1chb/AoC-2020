{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
import System.Environment (getArgs)
import Data.List as L (delete)
import Data.Set as S (Set, (\\), filter, findMin, fromList, map, member, notMember, size, toList, union)
import System.IO

main = do
  [filename] <- getArgs
  withFile filename ReadMode $ \file ->
    parseInput <$> hGetContents file
      >>= part -- 2D
      >>= part . addDimension -- 3D
      >>= part . addDimension -- 4D
      >>= part . addDimension -- 5D
      >>= part . addDimension -- 6D
      >>= part . addDimension -- 7D

part :: (Dimension ix, Ord ix) => Set ix -> IO (Set ix)
part init = do
  putStrLn $ show (dim $ findMin init) ++ "D: " ++ show (size $ iterate next init !! 6)
  return init

next :: (Dimension ix, Ord ix) => Set ix -> Set ix
next set = S.filter stillActive set `union` S.filter becomeActive candidates
  where
    stillActive ix = let n = count $ neighbours ix in n >= 2 && n <= 3
    becomeActive ix = let n = count $ neighbours ix in n == 3
    candidates = fromList (concatMap neighbours $ toList set) \\ set
    count = length . optimize . Prelude.filter (`member` set)
    optimize = take 4 -- No need to count to more than 4
    neighbours ix = delete ix $ community ix

parseInput :: String -> Set (Int,Int)
parseInput = fromList . concat . zipWith (Prelude.map . (,)) [0..] .
  fmap (Prelude.map fst . Prelude.filter ((=='#') . snd) . zip [0..]) . lines
    
class Dimension ix where
  community :: ix -> [ix]
  dim :: ix -> Int

instance Dimension Int where
  community x = [x-1..x+1]
  dim = const 1

instance Dimension ix => Dimension (Int,ix) where
  community (y,x) = [(y',x') | y' <- community y, x' <- community x]
  dim (_,x) = 1 + dim x
  
addDimension :: Ord ix => Set ix -> Set (Int,ix)
addDimension = S.map (0,)
