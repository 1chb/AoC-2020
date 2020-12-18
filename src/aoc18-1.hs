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
    p "Part1" $ sum $ eval . fst <$> exprs

data Expr = Expr Fact [Op] deriving (Show)
data Fact = Num Int | Par Expr deriving (Show)
data Op = Plus Fact | Mult Fact deriving (Show)

eval :: Expr -> Int
eval (Expr fact ops) = go (evalFact fact) ops where
  go acc [] = acc
  go acc (Plus fact:rest) = go (acc + evalFact fact) rest
  go acc (Mult fact:rest) = go (acc * evalFact fact) rest

evalFact :: Fact -> Int
evalFact (Num num) = num
evalFact (Par expr) = eval expr

parseExpr :: String -> (Expr,String)
parseExpr line = let (fact, rest) = parseFact line
                     (ops, rest2) = parseOps rest
                     in (Expr fact ops, rest2)

parseFact :: String -> (Fact, String)
parseFact ('(':line) = case parseExpr line of
  (expr, ')':rest) -> (Par expr, rest)
  other -> error $ "parseFact " ++ show other
parseFact line = case reads line of
  [(num, rest)] -> (Num num, rest)
  _ -> error $ "parseExpr " ++ show line

parseOps :: String -> ([Op],String)
parseOps (' ':c:' ':rest) =
  let (fact, rest1) = parseFact rest
      (ops, rest2) = parseOps rest1 in (op c fact : ops, rest2)
  where
    op '+' e = Plus e
    op '*' e = Mult e
    op c   e = error $ "Op " ++ show (c, e, rest)
parseOps rest = ([],rest)
