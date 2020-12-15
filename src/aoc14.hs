import Control.Monad (forM_)
import System.Environment (getArgs)
import Data.Bits ((.&.), (.|.))
import Data.List (partition)
import Data.List.Split (wordsBy)
import qualified Data.Map as M
import System.IO

main = do
  [filename] <- getArgs
  withFile filename ReadMode $ \file -> do
    ls <- lines <$> hGetContents file
    let map = part1 M.empty (0,0) ls
    print $ M.size map
    print $ sum $ M.elems map

    let map2 = part2 M.empty [] ls
    print $ M.size map2
    print $ sum $ M.elems map2

type Map = M.Map Int Int

part1 :: Map -> (Int,Int) -> [String] -> Map
part1 map mask (l:ls) = case l of
  'm':'a':'s':'k':' ':'=':' ':mask -> part1 map (parseMask $ reverse $ mask) ls
  'm':'e':'m':'[':asn -> part1 (uncurry M.insert (apply mask $ parseAsn asn) map) mask ls
  _ -> error $ "Bad line " ++ show l
part1 map _mask [] = map

parseMask :: String -> (Int,Int)
parseMask (x:xs) = case x of
  'X' -> (1+2*a,   2*o)
  '1' -> (  2*a, 1+2*o)
  '0' -> (  2*a,   2*o)
  where (a,o) = parseMask xs
parseMask [] = (0,0)

parseAsn asn = case reads asn of
  [(ix, ']':' ':'=':' ':val)] -> (ix, read val)
  _ -> error $ "Bad asn: " ++ asn

apply (a,o) (ix, val) = (ix, val .&. a .|. o)

part2 :: Map -> [(Int,Int)] -> [String] -> Map
part2 map masks (l:ls) = case l of
  'm':'a':'s':'k':' ':'=':' ':mask -> part2 map (parseMask2 $ reverse mask) ls
  'm':'e':'m':'[':asn -> part2 (foldr (insert2 val ix) map masks) masks ls where (ix,val) = parseAsn asn
  _ -> error $ "Bad line " ++ show l
part2 map _mask [] = map

parseMask2 :: String -> [(Int,Int)]
parseMask2 (x:xs) = case x of
  'X' -> [(x+2*a, x+2*o) | (a,o) <- masks, x <- [0,1]]
  '1' -> [(1+2*a, 1+2*o) | (a,o) <- masks]
  '0' -> [(1+2*a,   2*o) | (a,o) <- masks]
  where masks = parseMask2 xs
parseMask2 [] = [(0,0)]

insert2 :: Int -> Int -> (Int,Int) -> Map -> Map
insert2 val ix (a,o) map = M.insert (ix .&. a .|. o) val map
