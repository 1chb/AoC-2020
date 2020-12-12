import Control.Monad (forM, forM_)
import Data.Array
import Data.Tuple.Extra ((***), both)
import System.Environment (getArgs)
import System.IO

main = do
  [fname] <- getArgs
  withFile fname ReadMode $ \file -> do
    dat <- hGetContents file
    let list' = ('|':) . (++"|") <$> lines dat
    let w = length $ head list'
    let outer = '+' : replicate (w-2) '-' ++ ['+']
    let list = outer : list' ++ [outer]
    -- forM_ list print
    let h = length list
    print (h,w)
    let a = array ((1,1), (h,w)) [((y,x), c) | (y,row) <- zip [1..] list, (x,c) <- zip [1..] row]

    -- Part 1:
    printA "1: Init" a
    printA "1: Next" $ newgen repl1 a
    let bs = iter (newgen repl1) a
    printA "1: Final" $ last bs
    print $ countA $ last bs

    -- Part 2:
    let cs = iter (newgen repl2) a
    forM (zip [0..] cs) (\(k,a) -> printA ("2: Generation " ++ show k ++ ":") a)
    printA "2: Final" $ last cs
    print $ countA $ last cs

iter next a | a == b = [a] | otherwise = a : iter next b
  where b = next a

newgen repl a = a // [(i, repl a i) | i <- allpos $ bounds a]
  where
    allpos ((yl,xl), (yh,xh)) = [(y,x) | y <- [yl+1 .. yh-1], x <- [xl+1 .. xh-1]]

repl1 a i = case a ! i of
  'L' | cnt == 0 -> '#'
  '#' | cnt >= 4 -> 'L'
  c -> c
  where
    cnt = count $ (a !) . pairAdd i <$> neighbours
    
repl2 a i = case a ! i of
  'L' | cnt == 0 -> '#'
  '#' | cnt >= 5 -> 'L'
  c -> c
  where
    cnt = sum $ countDir <$> neighbours
    countDir di = count $ take 1 $ dropWhile (=='.') $ (a !) . pairAdd i <$> [both (k*) di | k <- [1..]]

neighbours = [(dy,dx) | dy <- [(-1)..1], dx <- [(-1)..1], (dy,dx) /= (0,0)]
pairAdd = uncurry (***) . both (+)
count = length . filter (=='#')

printA header a = putStrLn header >> printA' True a

printA' frame a = forM [yl+f..yh-f] $ \y -> putStrLn [a ! (y,x) | x <- [xl+f..xh-f]]
  where
    f = if frame then 0 else 1
    ((yl,xl), (yh,xh)) = bounds a

countA = count . elems
