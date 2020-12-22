import Control.Applicative ((<|>), (<*))
import Data.Char (isLower)
import Data.List (intercalate)
import qualified Data.Map as M
import qualified Data.Set as S
import System.Environment (getArgs)
import System.IO
import qualified Text.ParserCombinators.ReadP as P

main = do
  [filename] <- getArgs
  withFile filename ReadMode $ \file -> do
    input <- hGetContents file
    menu <- p "menu" $ parse dish <$> lines input
    aiMapL <- p' "aiMapL" $ (allergsM <$> menu :: [M.Map String (S.Set String)])
    aiMap <- p "aiMap" $ foldr1 (M.unionWith S.intersection) aiMapL

    allIngreds <- p "allIngreds" $ concatMap fst menu
    ingridS <- p "ingridS" $ S.fromList allIngreds
    allergIngreds <- p "allergIngreds" $ M.foldr S.union S.empty aiMap
    safeIngreds <- p "safeIngreds" $ filter (`S.notMember` allergIngreds) allIngreds
    p "Part-1" $ length safeIngreds

    (aiMap1, aiMapS) <- p "partition" $ M.partition ((<=1) . S.size) aiMap
    ingreds1 <- p "ingreds1" $ S.unions $ M.elems aiMap1 :: IO (S.Set String)

    aiMap2 <- p "aiMap2" $ part2 aiMap
    p "Part2" $ intercalate "," $ M.elems aiMap2

part2 :: M.Map String (S.Set String) -> M.Map String String
part2 aiMap = go aiMap M.empty where
  go aiMap aiRes = case M.partition ((<=1) . S.size) aiMap of
    (aiMap1, aiMapS)
      | M.null aiMap1 -> aiRes
      | otherwise -> go aiMapS1 $ M.union aiRes $ M.map (head . S.elems) aiMap1
      where
        aiMapS1 = M.map (S.\\ singles) aiMapS :: M.Map String (S.Set String)
        singles = S.unions $ M.elems aiMap1 :: S.Set String

allergsM :: ([String], [String]) -> M.Map String (S.Set String)
allergsM (ingreds, allergs) = M.fromList $ zip allergs $ repeat $ S.fromList ingreds

parse :: P.ReadP a -> String -> a
parse p = fst . head . P.readP_to_S (p <* P.eof)

dish :: P.ReadP ([String], [String])
dish = (,) <$> ingredsP <* P.skipSpaces >>= (<$> alergsP)

ingredsP :: P.ReadP [String]
ingredsP = P.sepBy1 wordP (P.char ' ')

alergsP :: P.ReadP [String]
alergsP = P.between (P.string "(contains ") (P.string ")") (P.sepBy1 wordP (P.string ", "))

wordP :: P.ReadP String
wordP = P.many1 (P.satisfy isLower)

trim :: P.ReadP a -> P.ReadP a
trim = P.between P.skipSpaces P.skipSpaces

p,p' :: (Show x) => String -> x -> IO x
p label x = do putStrLn $ pLim $ label ++ ": " ++ show x; return x
p' _label = return
pLim :: String -> String
pLim s = if length s' < 1000 then s' else s' ++ "..." where s' = take 1000 s
