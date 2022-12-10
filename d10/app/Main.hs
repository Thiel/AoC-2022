import System.IO
import Data.List
import Data.List.Split

sample = [
  "addx 15",
  "addx -11",
  "addx 6",
  "addx -3",
  "addx 5",
  "addx -1",
  "addx -8",
  "addx 13",
  "addx 4",
  "noop",
  "addx -1",
  "addx 5",
  "addx -1",
  "addx 5",
  "addx -1",
  "addx 5",
  "addx -1",
  "addx 5",
  "addx -1",
  "addx -35",
  "addx 1",
  "addx 24",
  "addx -19",
  "addx 1",
  "addx 16",
  "addx -11",
  "noop",
  "noop",
  "addx 21",
  "addx -15",
  "noop",
  "noop",
  "addx -3",
  "addx 9",
  "addx 1",
  "addx -3",
  "addx 8",
  "addx 1",
  "addx 5",
  "noop",
  "noop",
  "noop",
  "noop",
  "noop",
  "addx -36",
  "noop",
  "addx 1",
  "addx 7",
  "noop",
  "noop",
  "noop",
  "addx 2",
  "addx 6",
  "noop",
  "noop",
  "noop",
  "noop",
  "noop",
  "addx 1",
  "noop",
  "noop",
  "addx 7",
  "addx 1",
  "noop",
  "addx -13",
  "addx 13",
  "addx 7",
  "noop",
  "addx 1",
  "addx -33",
  "noop",
  "noop",
  "noop",
  "addx 2",
  "noop",
  "noop",
  "noop",
  "addx 8",
  "noop",
  "addx -1",
  "addx 2",
  "addx 1",
  "noop",
  "addx 17",
  "addx -9",
  "addx 1",
  "addx 1",
  "addx -3",
  "addx 11",
  "noop",
  "noop",
  "addx 1",
  "noop",
  "addx 1",
  "noop",
  "noop",
  "addx -13",
  "addx -19",
  "addx 1",
  "addx 3",
  "addx 26",
  "addx -30",
  "addx 12",
  "addx -1",
  "addx 3",
  "addx 1",
  "noop",
  "noop",
  "noop",
  "addx -9",
  "addx 18",
  "addx 1",
  "addx 2",
  "noop",
  "noop",
  "addx 9",
  "noop",
  "noop",
  "noop",
  "addx -1",
  "addx 2",
  "addx -37",
  "addx 1",
  "addx 3",
  "noop",
  "addx 15",
  "addx -21",
  "addx 22",
  "addx -6",
  "addx 1",
  "noop",
  "addx 2",
  "addx 1",
  "noop",
  "addx -10",
  "noop",
  "noop",
  "addx 20",
  "addx 1",
  "addx 2",
  "addx 2",
  "addx -6",
  "addx -11",
  "noop",
  "noop",
  "noop"]

data Ops = Add Int | Noop
  deriving Show

parse :: [String] -> [Ops]
parse input = do
  entry <- input
  case entry of
    "noop" -> [Noop]
    s | "addx" `isPrefixOf` s -> [Add $ read $ words s !! 1]

command2cycle_update :: Ops -> [Int]
command2cycle_update Noop = [0]
command2cycle_update (Add x) = [0, x]

type Signal = [Int]
signal :: [Ops] -> Signal
signal ops = scanl (+) 1 $ ops >>= command2cycle_update

input :: IO [String]
input = do
  h <- openFile "input" ReadMode
  c <- hGetContents h
  return $ lines c

signal_strength :: [Ops] -> [Int]
signal_strength ops = zipWith (*) ([20+i*40 | i <-[ 0..]]) $ map head $ chunksOf 40 $ drop 19 $ signal ops

solve :: [String] -> Int
solve input = sum $ take 6 $ signal_strength $ parse input

type Cycle = Int
pixelpos :: Cycle -> Signal -> [Int]
pixelpos cycle s = do
  i <- [-1..1]
  [i+middle]
  where
    middle = s !! (cycle - 1)

type Screen = [String]
render :: Signal -> Screen
render signal = chunksOf 40 (do cycle <- [1..240]
                                let crt_pos = cycle - 1 in
                                  if (crt_pos `mod` 40) `elem` pixelpos cycle signal then
                                    ['#']
                                  else
                                    ['.'])

solve2 :: [String] -> IO ()
solve2 input = mapM_ putStrLn $ render (signal $ parse input)
