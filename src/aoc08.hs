import Control.Monad (forM)
import Data.Array
import Data.Char (toUpper)
import qualified Data.Set as S
import System.Environment (getArgs)
import System.IO (IOMode(ReadMode), hGetContents, withFile)

main = do
  [fname] <- getArgs
  withFile fname ReadMode $ \file -> do
    dat <- hGetContents file
    let list = read . filter (/='+') . map toUpper <$> lines dat
    -- forM list print
    let pgm = listArray (0, length list) (list ++ [END])
    print $ execute pgm
    print $ filter fst $ execute . flopAt pgm <$> [0..length list]
    
data Instruction = NOP Int | ACC Int | JMP Int | END deriving (Read, Show)
type Program = Array Int Instruction

flopAt :: Program -> Int -> Program
flopAt pgm k = accum flop pgm [(k, undefined)]
  where
    flop (NOP d) _ = JMP d
    flop (JMP d) _ = NOP d
    flop other   _ = other

execute :: Program -> (Bool, Int) -- (terminates, acc)
execute pgm = go S.empty 0 0
  where
    go s acc pc = if pc `S.member` s then (False, acc) else
      case pgm ! pc of
        NOP _ -> go s'  acc    (pc+1)
        ACC d -> go s' (acc+d) (pc+1)
        JMP d -> go s'  acc    (pc+d)
        END   -> (True, acc)
        where s' = S.insert pc s
