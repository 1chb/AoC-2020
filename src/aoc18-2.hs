import Control.Monad (mapM_)
import System.Environment (getArgs)
import Data.List as L (delete)
import System.IO

p,p' :: (Show x) => String -> x -> IO x
p label x = do putStrLn $ label ++ ": " ++ show x; return x
p' _label x = return x

main = do
  [filename] <- getArgs
  withFile filename ReadMode $ \file -> do
    exprs <- map parseExpr . lines <$> hGetContents file
    mapM_ (p "expr") exprs
    mapM_ (p "res") $ eval . fst <$> exprs
    p "Part2" $ sum $ eval . fst <$> exprs

data Expr
  = Num Int
  | Plus Expr Expr
  | Mult Expr Expr
  deriving (Show)

eval :: Expr -> Int
eval (Num num) = num
eval (Plus l r) = eval l + eval r
eval (Mult l r) = eval l * eval r

parseExpr :: String -> (Expr,String)
parseExpr line = case parseTerm line of
  (left, ' ':'*':' ':rest) -> let (right, rest2) = parseExpr rest
                              in (Mult left right, rest2)
  (term, rest) -> (term, rest)

parseTerm :: String -> (Expr,String)
parseTerm line = case parseFact line of
  (left, ' ':'+':' ':rest) -> let (right, rest2) = parseTerm rest
                              in (Plus left right, rest2)
  (term, rest) -> (term, rest)

parseFact :: String -> (Expr,String)
parseFact ('(':line) = let (expr, ')':rest) = parseExpr line
                       in (expr, rest)
parseFact line = case reads line of
  [(num, rest)] -> (Num num, rest)
  _ -> error $ "parseFact " ++ show line
  
