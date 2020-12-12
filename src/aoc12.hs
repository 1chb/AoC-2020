import System.Environment (getArgs)
import Data.Tuple.Extra (first, second)
import System.IO

data D = N Int | S Int | E Int | W Int | L Int | R Int | F Int deriving (Read)

main = do
  [filename] <- getArgs
  withFile filename ReadMode $ \file -> do
    ds <- fmap readD . lines <$> hGetContents file
    print $ collect $ navigateAll  first ((0,0),  (1,0)) ds -- Part 1
    print $ collect $ navigateAll second ((0,0), (10,1)) ds -- Part 2
  where
    readD (d:int) = read $ d:' ':int
    collect ((a,b),_) = abs a + abs b
    navigateAll which = foldl (flip $ navigate which)
        
navigate which d = case d of -- (pos, dir) -> (pos, dir)
  N x -> which  $ second (+  x )
  S x -> which  $ second (+(-x))
  E x -> which  $ first  (+  x )
  W x -> which  $ first  (+(-x))
  L a -> second $ rotate $ 360-a
  R a -> second $ rotate       a
  F x -> \((pn,pe),dir@(dn,de)) -> ((pn+x*dn,pe+x*de),dir)
  where
    rotate a pos@(e,n) 
      | a > 45 = rotate (a-90) (n, -e)
      | a == 0 = pos
      | otherwise = error $ "Unsupported angle: " ++ show a
