import Control.Applicative ((<|>), (<*))
import Control.Monad (void)
import Data.Char (isDigit)
import qualified Data.Map as M
import System.Environment (getArgs)
import System.IO (IOMode(ReadMode), hGetContents, putStrLn, withFile)
import qualified Text.ParserCombinators.ReadP as P

main :: IO Int
main = do
  [filename] <- getArgs
  withFile filename ReadMode $ \file -> do
    (ruleLines, "" : messages) <- break null . lines <$> hGetContents file
    map <- p' "map" $ M.fromList $ head . parse ruleLineP <$> ruleLines
    p "Matches" $ length $ filter (==[()]) $ parse (makeRuleP map) <$> messages

parse :: P.ReadP a -> String -> [a]
parse p = fmap fst . P.readP_to_S (p <* P.eof)

ruleLineP :: P.ReadP (Int, String)
ruleLineP = (,) <$> numberP <* P.string ": " >>= (<$> P.many1 P.get)

makeRuleP :: M.Map Int String -> P.ReadP ()
makeRuleP map = go 0 where
  go :: Int -> P.ReadP ()
  go k = maybe (error $ "Cannot find rule #" ++ show k) pLine $ M.lookup k map
    where pLine line = case parse rulePP line of
            [ruleP] -> ruleP
            _BU -> error $ "Cannot parse rule #" ++ show k ++ ": " ++ show line
  rulePP, termPP, factorPP :: P.ReadP (P.ReadP ())
  rulePP = P.choice <$> termPP `P.sepBy1` P.string " | "
  termPP = sequence_ <$> factorPP `P.sepBy1` P.char ' '
  factorPP = go <$> numberP <|> void . P.char <$> letterP
  letterP :: P.ReadP Char
  letterP = P.between (P.char '"') (P.char '"') P.get

numberP :: P.ReadP Int
numberP = read <$> P.many1 (P.satisfy isDigit)

p,p' :: (Show x) => String -> x -> IO x
p label x = do putStrLn $ label ++ ": " ++ show x; return x
p' _label = return
