import Control.Monad
import Data.Array
import Data.List (groupBy, intersect)
import Data.Maybe (maybeToList)
import qualified Data.Map as M
import qualified Data.Set as S
import System.Environment (getArgs)
import System.IO

import Debug.Trace (trace)
tr a b = trace (a ++ ": " ++ show b) b

main = do
  [fname] <- getArgs
  withFile fname ReadMode $ \file -> do
    dat <- hGetContents file
    let list' = ('|':) . (++"|") <$> lines dat
    let w = length $ head list'
    let outer = take w $ repeat '-'
    let list = outer : list' ++ [outer]
    forM list print
    let h = length list
    print (h,w)
    let a = array ((0,0), (h-1,w-1)) [((y,x), c) | (y,row) <- zip [0..] list, (x,c) <- zip [0..] row]
    printA a
    printA $ newgen a
    let b = iter newgen a
    printA b
    print $ countA b
    let c = iter newgen2 a
    printA c
    print $ countA c

iter new a | a == b = a
           | otherwise = iter new b
  where b = new a

newgen a =
  a // [(i,new i) | i <- pos]
  where
    ((yl,xl), (yh,xh)) = bounds a
    pos = [(y,x) | y <- [yl+1 .. yh-1], x <- [xl+1 .. xh-1]]
    new i = case a ! i of
      'L' | cnt i == 0 -> '#'
      '#' | cnt i >= 4 -> 'L'
      c -> c
    cnt (y,x) = count [a ! (y+dy,x+dx) | dy <- [(-1)..1], dx <- [(-1)..1], (dy,dx) /= (0,0)]
    
newgen2 a =
  a // [(i,new i) | i <- pos]
  where
    ((yl,xl), (yh,xh)) = bounds a
    pos = [(y,x) | y <- [yl+1 .. yh-1], x <- [xl+1 .. xh-1]]
    new i = case a ! i of
      'L' | cnt i == 0 -> '#'
      '#' | cnt i >= 5 -> 'L'
      c -> c
    cnt i = sum [countDir i d | d <- dirs]
    dirs = [(dy,dx) | dy <- [(-1)..1], dx <- [(-1)..1], (dy,dx) /= (0,0)]
    countDir (y,x) (dy,dx) = count $ take 1 $ dropWhile (=='.') poss
      where
        poss = [a ! i' | i' <- [(y',x') | k <- [1..max yh xh-2], let (y',x') = (y+k*dy, x+k*dx), y'>yl, y'<yh, x'>xl, x'<xh]]

printA = printA' False
printA' frame a = forM [yl+f..yh-f] $ \y -> print [a ! (y,x) | x <- [xl+f..xh-f]]
  where
    f = if frame then 0 else 1
    ((yl,xl), (yh,xh)) = bounds a

countA = count . elems

count = length . filter (=='#')
