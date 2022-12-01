import Data.List.Split
import System.IO
import Data.List

sample = [
  "1000",
  "2000",
  "3000",
  "",
  "4000",
  "",
  "5000",
  "6000",
  "",
  "7000",
  "8000",
  "9000",
  "",
  "10000"
  ]

parse :: [String] -> [[Int]]
parse input = map (map read) (splitOn [""] input) :: [[Int]]

solve :: [String] -> Int
solve input = maximum $ map sum $ parse input

solve2 :: [String] -> Int
solve2 input = sum $ take 3 $ reverse $ sort $ map sum $ parse input

apply_solve :: ([String] -> Int) -> IO Int
apply_solve s = do
  handle <- openFile "input" ReadMode
  content <- hGetContents handle
  return $ s $ lines content
