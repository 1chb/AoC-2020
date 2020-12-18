import Control.Applicative ((<|>), (<*))
import Data.Char (isDigit)
import System.Environment (getArgs)
import System.IO
import qualified Text.ParserCombinators.ReadP as P

main = do
  [filename] <- getArgs
  withFile filename ReadMode $ \file -> do
    input <- hGetContents file
    p "Part-1" $ sum $ parse expr1 input
    p "Part-2" $ sum $ parse expr2 input

parse :: P.ReadP a -> String -> [a]
parse p = fst . head . P.readP_to_S (P.many (trim p) <* P.eof)

expr1 :: P.ReadP Int
expr1 = P.chainl1 (factor expr1) op1

op1 :: P.ReadP (Int -> Int -> Int)
op1 = (+) <$ trim (P.char '+') <|> (*) <$ trim (P.char '*')

expr2 :: P.ReadP Int
expr2 = P.chainl1 term2 $ (*) <$ trim (P.char '*')

term2 :: P.ReadP Int
term2 = P.chainl1 (factor expr2) $ (+) <$ trim (P.char '+')

factor :: P.ReadP Int -> P.ReadP Int
factor expr = number <|> P.between (P.char '(') (P.char ')') expr

number :: P.ReadP Int
number = read <$> P.many1 (P.satisfy isDigit)

trim :: P.ReadP a -> P.ReadP a
trim = P.between P.skipSpaces P.skipSpaces

p,p' :: (Show x) => String -> x -> IO x
p label x = do putStrLn $ label ++ ": " ++ show x; return x
p' _label = return
