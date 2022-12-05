import Data.List.Split
import Data.List
import System.IO
import Text.Parsec
import Data.Either

sample = [
  "2-4,6-8",
  "2-3,4-5",
  "5-7,7-9",
  "2-8,3-7",
  "6-6,4-6",
  "2-6,4-8"
  ]

type Section = (Int, Int)
type Assignment = (Section, Section)

parse_entry :: Parsec String st Assignment
parse_entry = let
  number = read <$> many1 digit
  in
    do x1 <- number 
       char '-'
       y1 <- number
       char ','
       x2 <- number
       char '-'
       y2 <- number
       return ((x1,y1),(x2,y2))

parse_input :: [String] -> [Assignment]
parse_input i = rights $ map (parse parse_entry "") i


overlap_fully :: Assignment -> Bool
overlap_fully ((x1,y1),(x2,y2)) | x1 >= x2 && y1 <= y2 = True
                                | x1 <= x2 && y1 >= y2 = True
                                | otherwise = False

solve :: [String] -> Int
solve input = sum $ map (fromEnum . overlap_fully) $ parse_input input

apply :: ([String] -> Int) -> IO Int
apply s = do
  h <- openFile "input" ReadMode
  c <- hGetContents h
  return $ s (lines c)

overlap :: Assignment -> Bool
overlap ((x1,y1),(x2,y2)) = intersect [x1..y1] [x2..y2] /= []

solve2 :: [String] -> Int
solve2 input = sum $ map (fromEnum . overlap) $ parse_input input
