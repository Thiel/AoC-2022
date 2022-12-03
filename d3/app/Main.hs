import Data.List
import Data.Char
import System.IO
import Data.Tuple


sample :: [String]
sample = [
  "vJrwpWtwJgWrhcsFMMfFFhFp",
  "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL",
  "PmmdzqPrVvPwwTWBwg",
  "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn",
  "ttgJtRGJQctTZtZT",
  "CrZsJsPPZsGzwwsLwLmpwMDw"
  ]

type Rucksack = ([Item], [Item])
type Item = Char

parse :: [String] -> [Rucksack]
parse input = map p_line input
  where p_line s = (take half s, drop half s)
          where half = length s `div` 2

priority :: Item -> Int
priority i | isLower i = ord i - ord 'a' + 1
priority i | isUpper i = ord i - ord 'A' + 27

solve :: [String] -> Int
solve input = sum $ map (priority . head . uncurry intersect) $ parse input

apply :: ([String] -> Int) -> IO Int
apply s = do
  h <- openFile "input" ReadMode
  c <- hGetContents h
  return $ s (lines c)

type Group = [[Item]]
type Badge = Char

parse2 :: [String] -> [Group]
parse2 [] = []
parse2 input = take 3 input : parse2 (drop 3 input)

get_badge :: Group -> Badge
get_badge g = head $ foldr1 intersect g

solve2 input = sum $ map (priority . get_badge) $ parse2 input
