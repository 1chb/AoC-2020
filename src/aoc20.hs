{-# LANGUAGE TupleSections #-}
import Control.Applicative ((<|>), (<*))
import Control.Monad (forM_, void)
import Data.Char (isDigit)
import Data.Array (Array, (//), (!), array, elems)
import Data.List.Extra (delete, dropEnd1, find, findIndex, groupOn, intercalate, nub, partition, sort, transpose)
import qualified Data.Map as M
import Data.Maybe (fromJust, isJust)
import System.Environment (getArgs)
import System.IO (IOMode(ReadMode), hGetContents, putStrLn, withFile)
import qualified Text.ParserCombinators.ReadP as P
import Debug.Trace

(!!!) :: (Ord k, Show k) => M.Map k a -> k -> a
m !!! k = case M.lookup k m of
  Just a -> a
  Nothing -> error $ "Cannot find " ++ show k ++ " in map."
infixl 9 !!!

main = do
  [filename] <- getArgs
  withFile filename ReadMode $ \file -> do
    tiles <- map parseTile . filter (/=[""]) . groupOn (not . null) . lines <$> hGetContents file
    p' "tiles" tiles
    border4s <- p' "bordes" $ border4 <$> tiles
    matches <- p' "mathes" $ matchN border4s <$> border4s
    zips <- p' "zip" $ sort $ zip matches border4s
    (corners', rest) <- p' "corners" $ splitAt 4 zips
    p "Part 1" $ product $ fst . snd <$> corners'

    sz <- p "sz" $ round $ sqrt $ fromIntegral $ length tiles
    map <- p' "map" $ M.adjust flipV 1951 $ M.fromList tiles
    map2 <- p' "map2" $ M.mapWithKey (matchB map) $ map
    -- p "1951" $ map !!! 1951
    -- p' "1951 rots" $ rot4 (map !!! 1951)
    -- p' "1951 edges" $ edges4 (map !!! 1951)
    (mapC, map3) <- p' "mapC" $ M.partition ((==2) . length . filter null) map2
    (mapE, map4) <- p' "mapE" $ M.partition ((==1) . length . filter null) map3
    (mapI, map5) <- p' "mapI" $ M.partition ((==0) . length . filter null) map4
    -- p "ambC" $ M.filter amb mapC
    -- p "ambE" $ M.filter amb mapE
    -- p "ambI" $ M.filter amb mapI
    -- p "mapCZ" $ M.fromList $ zipWith (\n (k,v) -> (k,(n,v))) [0..] $ M.toList mapC
    -- (tlK, tlT) <- p "TopLeft" $ (1951, mapC !!! 1951)
    (tlK, tlT) <- p "Corner" $ head $ M.toList mapC
    p "TL4" $ find isTopLeft $ iterate rotateL tlT
    map3 <- p' "map3" $ M.fromList $ zipWith (\tile (k,links) -> (k,(tile,links))) (M.elems map) $ M.toList map2
    -- p "1951-3" $ map3 !!! 1951
    map4 <- p "rot" $ rotAll tlK map3
    -- p "T: ROW 1" $ top    . fst . (map4 !!!) <$> [1951, 2311, 3079]
    -- p "B: ROW 1" $ bottom . fst . (map4 !!!) <$> [1951, 2311, 3079]
    -- p "T: ROW 1" $ top    . fst . (map4 !!!) <$> [2729, 1427, 2473]
    -- p "B: ROW 1" $ bottom . fst . (map4 !!!) <$> [2729, 1427, 2473]
    -- p "T: ROW 1" $ top    . fst . (map4 !!!) <$> [2971, 1489, 1171]
    -- p "B: ROW 1" $ bottom . fst . (map4 !!!) <$> [2971, 1489, 1171]

    -- p "R: 1951-4" $ right $ fst $ map4 !!! 1951
    -- p "L: 2311-4" $ left  $ fst $ map4 !!! 2311

    tiless <- p "Collect" $ collect map4 tlK
    tiles' <- p "Concat" $ concat $ fmap concat . transpose <$> tiless
    forM_ tiles' putStrLn
    (th,tw) <- p "TileSize" $ (length tiles', length $ head tiles')
    a <- p' "Array" $ array ((0,0),(th-1,tw-1)) $ zip [(r,c) | r <- [0..th-1], c <- [0..tw-1]] $ concat tiles'

    monster <- p "Monster"
      [ "                  # " -- 1
      , "#    ##    ##    ###" -- 2
      , " #  #  #  #  #  #   " -- 3
      ]--12345678901234567890
    (mh,mw) <- p "Monster size" $ (length monster, length $ head monster)
    szMonsters <- p' "szMonsters" $ zip (cycle [(mh,mw),(mw,mh)]) $ rf8 monster
    szIxMonsters <- p "ixMonsters" $
      [ ((mh,mw), ixs) | ((mh,mw), monster) <- szMonsters,
        let ixs = [rc | (rc, '#') <- zip [(r,c) | r <- [0..mh-1], c <- [0..mw-1]] $ concat monster]]
        
    forM_ szMonsters $ \(sz, monster) -> putStrLn $ "--- " ++ show sz ++ " ---\n" ++ intercalate "\n" monster
    found <- p "Monsters" $ [((r,c),m) | ((mh,mw),m) <- szIxMonsters, r <- [0..th-mh], c <- [0..tw-mw],
                             isMonster a (r,c) m]
    found2 <- p "Found" $ concat $ (\(rc,m) -> fmap (addOffs rc) m) <$> found
    print $ (length found2, length $ nub found2)
    upds <- p "Update" $ (,'O') <$> found2
    elms <- p "Array-O" $ elems $ a // upds
    p "Part 2" $ length $ filter (=='#') elms
    return ()

isMonster :: Array (Int,Int) Char -> (Int,Int) -> [(Int,Int)] -> Bool
isMonster a rc monster = all ((=='#') . (a !) . addOffs rc) monster where

addOffs :: (Int,Int) -> (Int,Int) -> (Int,Int)
addOffs (r,c) (mr,mc) = (mr+r, mc+c)

removeEdges :: Tile -> Tile
removeEdges = map (dropEnd1 . tail) . dropEnd1 . tail

collect :: M.Map Int (Tile,[[Int]]) -> Int -> [[Tile]]
collect map k = rows k where
  rows k = cols k : case map !!! k of
    (_, [_t, _l, b, _r]) -> concatMap rows b
  cols k = case map !!! k of
    (tile, [_t, _l, _b, r]) -> removeEdges tile : concatMap cols r
                     

amb :: [[Int]] -> Bool
amb ll = any ((>1) . length) ll

isTopLeft :: [[Int]] -> Bool
isTopLeft [t,l,_b,_r] = null t && null l
isTopLeft _ = False

rotAll :: Int -> M.Map Int (Tile, [[Int]]) -> M.Map Int (Tile, [[Int]])
rotAll start map = (\map -> trace (show $ "END: " ++ show (map !!! start)) map) $ goDown start $ (\map -> trace (show $ "START: " ++ show (map !!! start)) map) $ M.adjust rotToTopLeft start map where
  goDown :: Int -> M.Map Int (Tile, [[Int]]) -> M.Map Int (Tile, [[Int]])
  goDown link map =
    case map !!! link of
      (tile, [_t,[],[],_r]) -> goRight link map
      (tile, [t,[],[bLink],_r]) -> goDown bLink
        $ (\map -> trace (show $ "START-A: " ++ show (link, map !!! link, bLink, map !!! bLink, start, map !!! start)) map) $ goRight link $ M.adjust (rotOrFlipUntilGoodLR [bottom tile] []) (trace ("goDown from " ++ show link ++ " to " ++ show bLink ++ " top: " ++ show t) bLink) $ (\map -> trace (show $ "START-B: " ++ show (link, map !!! link, bLink, map !!! bLink, start, map !!! start)) map) map
      otherTile -> error $ "GO DOWN: " ++ show otherTile
    
  goRight link map = case map !!! link of
    (leftTile, [t,_l,_b,[rLink]]) -> goRight rLink $
      M.adjust (rotOrFlipUntilGoodLR (bottom . fst <$> concatMap findAboveRight t) [right leftTile]) (trace ("goRight from " ++ show link ++ " to " ++ show rLink ++ " top: " ++ show t) rLink) map where
      findAboveRight :: Int -> [(Tile, [[Int]])]
      findAboveRight t = case map !!! t of
        (_, [_t, _l, _b, r]) -> (map !!!) <$> r
        other -> error $ "findAboveRight faild with " ++ show other ++ " on " ++ show t
    (leftTile, [_t,_l,_b,[]]) -> map
    otherTile -> error $ "GO RIGHT: " ++ show otherTile
    
rotOrFlipUntilGoodLR :: [String] -> [String] -> (Tile, [[Int]]) -> (Tile, [[Int]])
rotOrFlipUntilGoodLR tops lefts (tile, links) = case find goodLR alts of
  Just x -> trace ("Result: " ++ show (snd x, tops, top $ fst x, lefts, left $ fst x)) x
  Nothing -> error $ "rotOrFlipUntilGoodLR: No good tile: for " ++ show (tops, lefts, tile, links, alts)
  where
    alts = zip (rf8 tile) (lk8 links)
    goodLR :: (Tile, [[Int]]) -> Bool
    goodLR (tile, [t, l, _b, _r]) = (top tile <$ t) == tops && (left tile <$ l) == lefts

rotToTopLeft :: (Tile, [[Int]]) -> (Tile, [[Int]])
rotToTopLeft (tile, links) = case find  topLeft alts of
  Just x -> trace ("TOP LEFT: " ++ show x) x
  Nothing -> error $ "rotToTopLeft: No TL found for " ++ show (tile, links, alts)
  where
    alts = take 4 $ zip (rf8 tile) (lk8 links)
    topLeft (_tile, links) = isTopLeft links

type Tile = [String]
type Map = M.Map Int Tile

-- For the map and a given tile number, return a list of length 4, each containing a list of
-- matching tile numbers.
matchB :: Map -> Int -> Tile -> [[Int]]
matchB map1 n1 t1 = (\e -> M.keys $ M.filter (match e) map) <$> edges4 t1
  where
    match :: String -> Tile -> Bool
    match e t = e `elem` edges4 t || reverse e `elem` edges4 t
    map = M.delete n1 map1

flipV, rotL :: Tile -> Tile
flipV = reverse
rotL =  transpose . map reverse
rotR =  map reverse . transpose
rot4,rf8 :: Tile -> [Tile]
rot4 = take 4 . iterate rotR
rf8 t = r4 ++ map flipV r4 where r4 = rot4 t
top, right, left :: Tile -> String
top = head
right = head . rotL
left = reverse . head . rotR
bottom = last
rotM :: String -> Tile -> (Tile,Bool)
rotM r t = maybe (t,False) (,True) $ find ((==r) . left) $ rot4 t
edges4 :: Tile -> [String] -- [T,L,B,R]
edges4 = map top . rot4

-- links:
rotateL :: [a] -> [a]
rotateL (x:xs) = xs ++ [x]
rotateL [] = []

lk4 = take 4 . iterate rotateL

lk8 links = lk4 links ++ ((\[t,l,b,r] -> [b,l,t,r]) <$> lk4 links)

matchSide :: String -> [Tile] -> ([(Tile,Bool)], [(Tile,Bool)])
matchSide side edges = partition snd $ rotM side <$> edges
  
matchN :: [(Int, Tile)] -> (Int, Tile) -> Int
matchN tBorder4s tBorder4@(_num, bord4) = length $ filter match bord4 where
  match :: String -> Bool
  match bord1 = bord1 `elem` border4s' || reverse bord1 `elem` border4s'
  border4s' :: Tile
  border4s' = concatMap snd $ delete tBorder4 tBorder4s

border4 :: (Int, Tile) -> (Int, Tile)
border4 (num, lns) = (num, head lns : last lns : head lns' : last lns' : []) where
  lns' = transpose lns

parseTile :: [String] -> (Int, Tile)
parseTile (('T':'i':'l':'e':' ':rest):lines) = case reads rest of
        [(num, ":")] -> (num, lines)
        _ -> error $ "Cannot parse id: " ++ show rest
parseTile other = error $ "Cannot parse tile: " ++ show other

p,p' :: (Show x) => String -> x -> IO x
p label x = do putStrLn $ label ++ ": " ++ show x; return x
p' _label = return
