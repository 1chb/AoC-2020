import qualified Data.Set as S
import System.Environment (getArgs)
import System.IO (IOMode(ReadMode), hGetContents, putStrLn, withFile)

main = do
  [filename] <- getArgs
  withFile filename ReadMode $ \file -> do
    ("Player 1:":player1, "":"Player 2:":player2) <- break null . lines <$> hGetContents file
    p1 <- p' "P1" $ read <$> player1 :: IO [Int]
    p2 <- p' "P2" $ read <$> player2 :: IO [Int]
    game <- p' "Play" $ play1 p1 p2
    p "Part-1" $ sum $ zipWith (*) [1..] $ reverse game

    (d1,d2) <- p' "Play2" $ play2 p1 p2
    p "Part-2" $ sum $ zipWith (*) [1..] $ reverse $ d1++d2

play1 (a:as) (b:bs)
  | a > b = play1 (as ++ [a,b]) bs
  | b > a = play1 as (bs ++ [b,a])
  | otherwise = error "Equal"
play1 as bs = as ++ bs

play2 :: [Int] -> [Int] -> ([Int], [Int])
play2 as bs = go S.empty (as, bs) where
  go prevS g@(a:as, b:bs)
    | g `S.member` prevS = ([1], [])
    | a <= length as && b <= length bs =
      case play2 (take a as) (take b bs) of
        (_as, []) -> go currS (as ++ [a,b], bs)
        ([], _bs) -> go currS (as, bs ++ [b,a])
        other -> error $ "No subwinner: " ++ show other
    | a > b = go currS (as ++ [a,b], bs)
    | b > a = go currS (as, bs ++ [b,a])
    | otherwise = error "Equal"
    where currS = S.insert g prevS
  go _ g = g

p,p' :: (Show x) => String -> x -> IO x
p label x = do putStrLn $ pLim $ label ++ ": " ++ show x; return x where
                 pLim s = if length s' < 1000 then s' else s' ++ "..." where s' = take 1000 s
p' _label = return
