import Control.Monad
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
    let list = parseLine <$> lines dat
    -- forM list print
    let cpm = belong list
    -- print $ cpm
    let tops = findAll cpm "shiny gold"
    print $ S.size tops
    let pcm = M.fromList list
    -- print pcm
    print $ count pcm "shiny gold" - 1 -- Don't count this

type Map = M.Map String [(Int, String)]
type Set = S.Set String

count :: Map -> String -> Int
count m name = go name
  where
    go :: String -> Int
    go name = 1 + maybe 0 (foldr acc 0) (M.lookup name m)
    acc (c,n) t = c * go n + t

findAll :: Map -> String -> Set
findAll m name = go S.empty name
  where go s name = case M.lookup name m of
          Nothing -> s
          Just cps -> foldr go2 s $ map snd cps
        go2 p s = if S.member p s then s else go (S.insert p s) p

belong :: [(String, [(Int, String)])] -> Map
belong = foldr parent M.empty
  where
    parent :: (String, [(Int, String)]) -> Map -> Map
    parent (p, contents) m = foldr (child p) m contents
    child :: String -> (Int, String) -> Map -> Map
    child p (count, c) m = M.alter (ack (count, p)) c m
    ack :: (Int, String) -> Maybe [(Int, String)] -> Maybe [(Int, String)]
    ack cp old = Just $ cp : concat (maybeToList old)

parseLine :: String -> (String, [(Int, String)])
parseLine = parse . words
  where
    parse (a : b : "bags" : "contain" : contents) = (name a b, go contents)
    go (count : a : b : bags : rest)
      | bags `elem` ["bag,", "bags,", "bag.", "bags."] = (read count, name a b) : go rest
    go ["no", "other", "bags."] = []
    go [] = []
    go other = error $ "Bad syntax: " ++ show other
    name a b = a ++ ' ' : b
