import Control.Monad (forM, forM_)
import Data.Array
import Data.Tuple.Extra ((***), both)
import System.Environment (getArgs)
import System.IO

import Debug.Trace (trace)

main = do
  [fname] <- getArgs
  withFile fname ReadMode $ \file -> do
    dat <- hGetContents file
    let list = read . space <$> lines dat :: [Dir]
    -- forM_ list print
    let (e,n) = navigate (10,1) (0,0) (E 0) list
    print ((e,n), abs e + abs n)

space (c:cs) = c : ' ' : cs
space cs = cs

data Dir = N Int | S Int | E Int | W Int | L Int | R Int | F Int deriving (Read, Show)

navigate :: (Int,Int) -> (Int,Int) -> Dir -> [Dir] -> (Int,Int)
navigate (e,n) pos@(pe,pn) cd (d:ds) = {- trace (show ((e,n),pos,cd,d)) $-} case d of
  N x -> navigate (e,n+x) pos cd ds
  S x -> navigate (e,n-x) pos cd ds
  E x -> navigate (e+x,n) pos cd ds
  W x -> navigate (e-x,n) pos cd ds
  L a -> navigate (newWP (360-a) (e,n)) pos cd ds
  R a -> navigate (newWP a (e,n)) pos cd ds
  F x -> navigate (e,n) (ffw x) cd ds
  where ffw x = (pe+x*e,pn+x*n)
          -- case cd of
          -- N _ -> (pe,pn+x*n)
          -- S _ -> (pe,pn-x*n)
          -- E _ -> (pe+x*e,pn)
          -- W _ -> (pe-x*e,pn)
          -- _   -> pos
        newWP 0 (e,n) = (e,n)
        newWP a (e,n) = newWP (a-90) ((0+n),(0-e))

        -- newdir a cd | a == 0 = cd
        --             | a >= 90 = newdir (a-90) $ next cd
        --             | a <= 90 = newdir (a+360) cd
        --             | otherwise = error $ "Bad angle: " ++ show a
        next d = case d of
          N x -> E x
          E x -> S x
          S x -> W x
          W x -> N x
          _   -> d
          
navigate _wp pos _cd [] = pos
