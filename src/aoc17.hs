import Control.Monad (forM_, when)
import System.Environment (getArgs)
import Data.Array
import Data.List (delete, isPrefixOf, partition, span)
import Data.List.Split (wordsBy)
import qualified Data.Set as S
import System.IO

p,p' :: (Show x) => String -> x -> IO x
p label x = do putStrLn $ label ++ ": " ++ show x; return x
p' _label x = return x

main = do
  [filename] <- getArgs
  withFile filename ReadMode $ \file -> do
    list <- lines <$> hGetContents file :: IO [String]
    w <- p "W" $ length $ head list -- 3 or 8
    let n = 6
    g <- p "G" $ w `div` 2
    h <- p "H" $ w + n
    let a' = array ((-h,-h,-h), (h,h,h)) [((x,y,z), '.') | x <- [-h..h], y <- [-h..h], z <-[-h..h]]
    let a = a' // [((x,y,0),c) | (y,row) <- zip [-g..] list, (x,c) <- zip [-g..] row]
    let aa = nexts repl1 a
    p' "Bounds" $ bounds a
    -- forM_ [0..n] $ \k -> printA ("After generation " ++ show k) $ (aa !! k)
    p' "Part1" $ countA <$> take (n+1) aa
    p "Part1" $ countA $ aa !! n
    -- p' "Neighbours" $ (length neighbours, neighbours)

    -- Part2:
    let a2' = array ((-h,-h,-h,-h), (h,h,h,h)) [((x,y,z,w), '.') | x <- [-h..h], y <- [-h..h], z <-[-h..h], w <- [-h..h]]
    let a2 = a2' // [((x,y,0,0),c) | (y,row) <- zip [-g..] list, (x,c) <- zip [-g..] row]
    p' "Bounds" $ bounds a2
    p "Elems" $ countA a2'
    let a2a = next2s repl2 a2
    -- forM_ [0..2] $ \k -> printA2 ("After generation " ++ show k) $ (a2a !! k)
    p' "Part2" $ countA <$> take (n+1) a2a
    p "Part2" $ countA $ a2a !! n
    p' "Neighbours2" $ (length neighbours2, neighbours2)
    

type Arr = Array Ind Char
type Ind = (Int,Int,Int)

nexts :: (Arr -> Ind -> Char) -> Arr -> [Arr]
nexts repl a = a : go a where
  go :: Arr -> [Arr]
  go a = a' : go a' where
    a' = newgen repl a

newgen :: (Arr -> Ind -> Char) -> Arr -> Arr
newgen repl a = a // [(i, repl a i) | i <- allpos $ bounds a]
  where
    allpos ((xl,yl,zl), (xh,yh,zh)) = [(x,y,z) | z <- [zl+1..zh-1], y <- [yl+1..yh-1], x <- [xl+1..xh-1]]

repl1 :: Arr -> Ind -> Char
repl1 a i = case a ! i of
  '#' | cnt < 2 || cnt > 3 -> '.'
  '.' | cnt == 3 -> '#'
  c -> c
  where
    cnt = count $ (a !) . add3 i <$> neighbours
    
count = length . filter (=='#')
add3 (x,y,z) (x',y',z') = (x+x',y+y',z+z')
neighbours :: [Ind]
neighbours = [(dx,dy,dz) | dz <- [-1..1], dy <- [-1..1], dx <- [-1..1], (dx,dy,dz) /= (0,0,0)]

printA label a = do
  putStrLn $ "\n" ++ label ++ " (count = " ++ show (countA a) ++ ")"
  forM_ [zl..zh] $ \z ->
    when (count (map snd $ filter (\((_,_,z'),_) -> z'==z) $ assocs a) > 0) $ do
      putStrLn $ "\nz=" ++ show z
      forM_ [yl..yh] $ \y -> putStrLn [a ! (x,y,z) | x <- [xl..xh]]
  where
    ((xl,yl,zl), (xh,yh,zh)) = bounds a

countA = count . elems

type Arr2 = Array Ind2 Char
type Ind2 = (Int,Int,Int,Int)

next2s :: (Arr2 -> Ind2 -> Char) -> Arr2 -> [Arr2]
next2s repl a = a : go a where
  go :: Arr2 -> [Arr2]
  go a = a' : go a' where
    a' = newgen2 repl a

newgen2 :: (Arr2 -> Ind2 -> Char) -> Arr2 -> Arr2
newgen2 repl a = a // [(i, repl a i) | i <- allpos $ bounds a]
  where
    allpos ((xl,yl,zl,wl), (xh,yh,zh,wh)) = [(x,y,z,w) | w <- [wl+1..wh-1], z <- [zl+1..zh-1], y <- [yl+1..yh-1], x <- [xl+1..xh-1]]

repl2 :: Arr2 -> Ind2 -> Char
repl2 a i = case a ! i of
  '#' | cnt < 2 || cnt > 3 -> '.'
  '.' | cnt == 3 -> '#'
  c -> c
  where
    cnt = count $ (a !) . add4 i <$> neighbours2
    
add4 (x,y,z,w) (x',y',z',w') = (x+x',y+y',z+z',w+w')
neighbours2 :: [Ind2]
neighbours2 = [(dx,dy,dz,dw) | dw <- [-1..1], dz <- [-1..1], dy <- [-1..1], dx <- [-1..1], (dx,dy,dz,dw) /= (0,0,0,0)]

printA2 :: String -> Arr2 -> IO ()
printA2 label a = do
  putStrLn $ "\n" ++ label ++ " (count = " ++ show (countA a) ++ ")"
  forM_ [wl..wh] $ \w ->
    forM_ [zl..zh] $ \z ->
      when (count (map snd $ filter (\((_,_,z',w'),_) -> z'==z && w'==w) $ assocs a) > 0) $ do
        putStrLn $ "\nz=" ++ show z ++ ", w=" ++ show w
        forM_ [yl..yh] $ \y -> putStrLn [a ! (x,y,z,w) | x <- [xl..xh]]
  where
    ((xl,yl,zl,wl), (xh,yh,zh,wh)) = bounds a

