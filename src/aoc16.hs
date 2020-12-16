import System.Environment (getArgs)
import Data.List (delete, isPrefixOf, partition, span)
import Data.List.Split (wordsBy)
import qualified Data.Set as S
import System.IO

p,p' :: (Show x) => String -> x -> IO x
p label x = do putStrLn $ label ++ ": " ++ show x; return x
p' _label x = return x

main = do
  [filename] <- getArgs
  withFile filename ReadMode $ \file -> do
    input <- lines <$> hGetContents file :: IO [String]
    let (fieldLines, "":"your ticket:":myTicketLine:"":"nearby tickets:":otherTicketLines) = span (not . null) input
    p' "Field lines" fieldLines
    p' "My ticket line  " myTicketLine
    p' "Last ticket line" $ last otherTicketLines
    fields <- p' "Fields" $ parseField <$> fieldLines
    otherTickets <- p' "Other tickes" $ parseTicket <$> otherTicketLines
    p "Part1" $ sum $ concat $ notInFields fields <$> otherTickets

    -- Part2:
    validTickets <- p' "Valid tickets" $  filter (null . notInFields fields) otherTickets
    sets <- p' "Sets" $ zip fields $ repeat $ S.fromList [0..length fields-1]
    reducedSets <- p' "Reduced sets" $ reduceAll validTickets <$> sets
    (singles, []) <- p' "Singles" $ partition isSingle $ reduceSingles reducedSets
    myTicket <- p' "My ticket" $ parseTicket myTicketLine
    depIxs <- p' "Departure indexes'" $ map snd $ filter (\(name, _val) -> "departure" `isPrefixOf` name) $ extract <$> singles
    p "Part2" $ product $ (myTicket !!) <$> depIxs

type Field = (String, Range, Range)
type Range = (Int,Int)
type Set = (Field, S.Set Int) -- indexes

extract :: Set -> (String, Int)
extract ((name, _, _), set) = (name, head $ S.toList set)

reduceSingles :: [Set] -> [Set]
reduceSingles sets
  | null singles = sets
  | otherwise    = singles ++ reduceSingles (remove singles <$> multiples) where
      (singles, multiples) = partition isSingle sets

remove :: [Set] -> Set -> Set
remove singles (name, set) = (name, rm singles set) where
  rm ((_name, single):singles) set = rm singles $ set S.\\ single
  rm [] set = set

isSingle :: Set -> Bool
isSingle (_field, set) = S.size set == 1

reduceAll :: [[Int]] -> Set -> Set
reduceAll (t:ts) set = reduce t $ reduceAll ts set
reduceAll [] set = set

reduce :: [Int] -> Set -> Set
reduce = go 0 where
  go ix (val:vals) (field, ixs) = go (ix+1) vals (field, if inField val field then ixs else S.delete ix ixs)
  go _ _ set = set

parseField :: String -> Field
parseField line = (name, r1, r2) where
  (name, ':':' ':nums) = span (/=':') line
  (r1', ' ':'o':'r':' ':r2') = span (/=' ') nums
  (r1, r2) = (range r1', range r2')
  range r = (read a, read b) :: Range where
    (a,'-':b) = span (/='-') r

parseTicket :: String -> [Int]
parseTicket ticket = read <$> wordsBy (==',') ticket

notInFields :: [Field] -> [Int] -> [Int]
notInFields fields vals = filter bad vals where
  bad val = not $ any (inField val) fields

inField val (_name, r1, r2) = inRange val r1 || inRange val r2

inRange val (a,b) = a <= val && val <= b
