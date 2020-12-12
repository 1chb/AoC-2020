import Control.Monad
import Data.Char
import Data.List ((\\), delete)
import Data.Tuple (swap)
import System.Environment (getArgs)
import System.IO

main = do
  [fname] <- getArgs
  withFile fname ReadMode $ \file -> do
    dat <- hGetContents file
    let pwdLines = group $ lines dat
    forM pwdLines print
    let pwds = parse <$> pwdLines
    forM pwds print
    let tags = fmap fst <$> pwds
    let missings = ([minBound..] \\) <$> tags
    print missings
    print $ length $ filter valid pwds
    let pwds' = filter check <$> pwds
    -- forM pwds' $ putStrLn . show
    print $ length $ filter valid pwds'

valid :: [(Tag, String)] -> Bool
valid pwd = null $ delete CID [minBound..] \\ map fst pwd

check :: (Tag, String) -> Bool
check (tag, ':':val) = case (tag, val) of
  (BYR, year) -> checkYear 1920 2002 year -- (Birth Year) - four digits; at least 1920 and at most 2002.
  (IYR, year) -> checkYear 2010 2020 year -- (Issue Year) - four digits; at least 2010 and at most 2020.
  (EYR, year) -> checkYear 2020 2030 year -- (Expiration Year) - four digits; at least 2020 and at most 2030.
  (HGT,  hgt) -> checkHgt hgt -- (Height) - a number followed by either cm or in:
                       -- If cm, the number must be at least 150 and at most 193.
                       -- If in, the number must be at least 59 and at most 76.
  (HCL, hair) -> checkHair hair -- (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
  (ECL,  eye) -> checkEye eye -- (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
  (PID,  pid) -> checkPid pid -- (Passport ID) - a nine-digit number, including leading zeroes.
  (CID, _cid) -> True -- (Country ID) - ignored, missing or not.
  where
    checkYear from to str = case reads str of
      [(year, "")] -> between from to year
      _ -> False
    checkHgt hgt = case reads hgt of
      [(cm, "cm")] -> between 150 193 cm
      [(inch, "in")] -> between 59 76 inch
      _ -> False
    checkHair ('#':hair) = all (\c -> between '0' '9' c || between 'a' 'f' c) hair && length hair == 6
    checkHair _ = False
    checkEye eye = eye `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
    checkPid pid = length pid == 9 && all isDigit pid
    between from to x = from <= x && x <= to
  

group :: [String] -> [String]
group xss = go [] xss where
  go ys (xs:xss) | null xs = ys : go [] xss
                 | otherwise = go (ys ++ ' ':xs) xss
  go ys [] | null ys = []
           | otherwise = [ys]

parse :: String -> [(Tag, String)]
parse pwd = swap . fmap (read . map toUpper) . swap . break (==':') <$> words pwd

data Tag = BYR -- (Birth Year)
         | IYR -- (Issue Year)
         | EYR -- (Expiration Year)
         | HGT -- (Height)
         | HCL -- (Hair Color)
         | ECL -- (Eye Color)
         | PID -- (Passport ID)
         | CID -- (Country ID)
         deriving (Bounded, Enum, Eq, Read, Show)
