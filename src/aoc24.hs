import Control.Applicative ((<|>), (<$))
import Data.List (group, sort)
import qualified Data.Set as S
import System.Environment (getArgs)
import qualified Text.ParserCombinators.ReadP as P

main = do
  p' "esew" $ foldr1 pAdd $ parse lineP "esew"
  p' "nwwswee" $ foldr1 pAdd $ parse lineP "nwwswee"
  p' "Sum of neighbours" $ foldr1 pAdd $ neighbours (0,0)
  tiles <- fmap (foldr1 pAdd . parse lineP) . lines <$> (getArgs >>= readFile . head)
  blackTiles <- p' "Black tiles" $ map head $ filter (odd . length) $ group $ sort tiles
  p "Part-1" $ length blackTiles
  p "Part-2" $ last $ zip [0..] $ take 101 $ S.size <$> iterate next (S.fromList blackTiles)

next :: S.Set (Int, Int) -> S.Set (Int, Int)
next set = S.filter stillBlack set `S.union` S.filter becomeBlack candidates
  where
    stillBlack ix = let n = count $ neighbours ix in n >= 1 && n <= 2
    becomeBlack ix = let n = count $ neighbours ix in n == 2
    candidates = S.fromList (concatMap neighbours $ S.toList set) S.\\ set
    count = length . Prelude.filter (`S.member` set)

neighbours :: (Int, Int) -> [(Int, Int)]
neighbours (y,x) = [(y+dy, x+dx) | (dy,dx) <- [(0,2), (-1,1), (-1,-1), (0,-2), (1,-1), (1,1)]]

pAdd :: (Int, Int) -> (Int, Int) -> (Int, Int)
pAdd (a,b) (c,d) = (a+c, b+d)

parse :: P.ReadP a -> String -> a
parse p = head . fmap fst . P.readP_to_S (p <* P.eof)

lineP :: P.ReadP [(Int, Int)]
lineP = P.many1 dirP

dirP :: P.ReadP (Int, Int)
dirP = eP <|> seP <|> swP <|> wP <|> nwP <|> neP

eP, seP, swP, wP, nwP, neP :: P.ReadP (Int, Int)
eP  = ( 0, 2) <$ P.string "e"
seP = (-1, 1) <$ P.string "se"
swP = (-1,-1) <$ P.string "sw"
wP  = ( 0,-2) <$ P.string "w"
neP = ( 1, 1) <$ P.string "ne"
nwP = ( 1,-1) <$ P.string "nw"

p,p' :: (Show x) => String -> x -> IO x
p label x = do putStrLn $ pLim $ label ++ ": " ++ show x; return x where
                 pLim s = if length s' < 1000 then s' else s' ++ "..." where s' = take 1000 s
p' _label = return
