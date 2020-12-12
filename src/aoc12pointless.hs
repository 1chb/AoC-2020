import Control.Monad ((>=>)) -- The right fish
import Data.Tuple.Extra ((***), (&&&), both, first, second)
import System.Environment (getArgs)
import System.IO

data D = N Int | S Int | E Int | W Int | L Int | R Int | F Int deriving (Read)

main = head <$> getArgs -- Get filename as first argument
  >>= flip (`withFile` ReadMode) (hGetContents -- With the file, read its content
  >=> uncurry (>>) . both (print . uncurry (+) . both abs . fst) -- Sum abs and print both
  .  (foldl (flip $ navigate  first) ((0,0),  (1,0))        -- Part 1
  &&& foldl (flip $ navigate second) ((0,0), (10,1))     -- Part 2
     ) . fmap (read . (\(d:ds) -> d:' ':ds)) . lines) -- Parse content

navigate which d = case d of -- (pos, dir) -> (pos, dir)
  N x -> which  $ second (+  x )
  S x -> which  $ second (+(-x))
  E x -> which  $ first  (+  x )
  W x -> which  $ first  (+(-x))
  L a -> second $ rotate $ 360-a
  R a -> second $ rotate       a
  F x -> (uncurry . uncurry) (***) . (both (+) . fst &&& both (*x) . snd) &&& snd
  where
    rotate a pos@(e,n) 
      | a > 45 = rotate (a-90) (n, -e)
      | a == 0 = pos
      | otherwise = error $ "Unsupported angle: " ++ show a
