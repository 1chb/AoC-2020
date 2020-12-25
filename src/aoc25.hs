main = do
  let (cardPubl, doorPubl) = if True then (19241437, 17346587) else (5764801, 17807724)

  cardSize <- p "Card size" $ loopSize cardPubl
  doorSize <- p "Door size" $ loopSize doorPubl

  encKey1 <- p "Encryption key 1" $ transforms doorPubl !! cardSize
  encKey2 <- p "Encryption key 2" $ transforms cardPubl !! doorSize
  p "Equal keys" $ encKey1 == encKey2

transforms :: Int -> [Int]
transforms subjectMatter = iterate ((`mod` 20201227) . (*subjectMatter)) 1

loopSize :: Int -> Int
loopSize pk = fst $ head $ dropWhile ((/=pk) . snd) $ zip [0..] $ transforms 7

p :: (Show x) => String -> x -> IO x
p label x = do putStrLn $ label ++ ": " ++ show x; return x
