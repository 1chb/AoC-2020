import Control.Monad (forM_)
import System.Environment (getArgs)
import Data.List (partition)
import Data.List.Split (wordsBy)
import System.IO

main = do
  [filename] <- getArgs
  withFile filename ReadMode $ \file -> do
    line1:line2:_ <- lines <$> hGetContents file
    let time = read line1 :: Int
    let mixed = wordsBy (==',') line2
    let fix "x" = "0"; fix bus = bus
    let buses = read <$> map fix mixed :: [Int]

    putStrLn "Part1:"
    -- print $ (time,buses)
    let (dep,bus) = minimum $ (\bus -> ((time `div` bus+1)*bus,bus)) <$> filter (/=0) buses
    print (dep,bus,(dep-time)*bus)
    putStrLn ""

    
    putStrLn "Part2:"
    let buses2 = filter ((/=0) . fst) $ zip buses [0..]
    -- print buses2
    let (bus0,0):_ = buses2 -- It seems the first bus is special..
    -- print bus0

    -- The bus ID with offset d: bus!d
    -- x divides t             : x|t

    -- Task: Find an Int t such that:
    --   (*1) bus!j|t+j, for all buses j <- buses2.
    let condOne t (busJ,j) = (t-bus0+j) `mod` busJ == 0
    let condAll busJs t = and $ condOne t <$> busJs

    -- From the preamble: 7|t and 19|t+7, I found a similar example in my input data:
    -- 41|t and 431|t+41, but the product of 41*431 seems not to be big enough for fast
    -- calculation. But note, 19|t+7 ==> 19|t+7+n*19 for all Int n, which is a weaker
    -- condition. Maybe more buses fulfill this:
    -- If:    bus!k|t + k, and k = bus!0 + n * bus!k,
    -- then:  bus!k|t +           (bus!0 + n * bus!k)  ==>   bus!k|t + bus!0   !!!
    -- This is also trivial true for bus!0:  bus!0|t   ==>   bus!0|t + bus!0
    -- k = bus!0 + n*bus!k   <==> k - bus!0 = n*bus!k, i.e. find all k, such that:
    --   (*2) bus!k|k - bus!0
    -- When we're at it, partion the buses that fulfills (*2) and the ones that don't:
    let (busKs, busJs) = partition (\(busK,k) -> (k-bus0) `mod` busK == 0) buses2
    print (busKs, busJs)

    -- Assuming all bus!k are relative prime, in fact, they seem all to be primes, take the
    -- product of all bus!k for all the above k. If they weren't divide by their GCD...
    let pBusKs = product $ fst <$> busKs
    forM_ (fst <$> busKs) $ putStr . (' ':) . show; putStrLn ""
    print pBusKs

    -- Now, just loop t over all positive multiples of pBusKs and find the smallest one that
    -- satisfies the above task (*1). In fact it is not necessary to check all buses, just
    -- the ones that don't fullfil (*2).
    let search busJs = head (filter (condAll busJs) [pBusKs, 2*pBusKs..]) - bus0
    let t = search busJs
    print t

    -- We can verify t by checking (*1) for all busKs as well, or by searching again
    -- checking all buses, or why not both:
    print ("Verify", condAll busKs t, t == search buses2, pBusKs == search busKs + bus0)

    -- Result: t = 556100168221141
    -- (t + bus0) = pBusKs * 7494558 (number of iterations)
