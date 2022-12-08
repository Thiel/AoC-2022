import System.IO
import Data.Char
import Data.Maybe
import Data.List
import Data.List.Split

sample :: [String]
sample = [
  "30373",
  "25512",
  "65332",
  "33549",
  "35390"
  ]

type Height = Int
type Pos = (Int, Int)
type Tree = (Pos, Height)
type Map = ((Int, Int), [Tree])

parse :: [String] -> Map
parse input = ((w,h),zip coords (map digitToInt $ concat input))
  where h = length input
        w = length $ head input
        coords = [(x,y) | y <- [0..h-1], x <- [0..w-1]]

data Dir = North | South | West | East

neigh_height :: Dir -> Map -> Tree -> [Height]
neigh_height North ((_,h),ts) ((x,y),_) = map (maybe (-1) id) $ map (\p -> lookup p ts) [(x,y-i) | i <- [1..h]]
neigh_height South ((_,h),ts) ((x,y),_) = map (maybe (-1) id) $ map (\p -> lookup p ts) [(x,y+i) | i <- [1..h]]
neigh_height East ((w,_),ts) ((x,y),_) = map (maybe (-1) id) $ map (\p -> lookup p ts) [(x+i,y) | i <- [1..w]]
neigh_height West ((w,_),ts) ((x,y),_) = map (maybe (-1) id) $ map (\p -> lookup p ts) [(x-i,y) | i <- [1..w]]

all_direction :: [Dir]
all_direction = [North, South, East, West]

is_hidden :: Map -> Tree -> Bool
is_hidden m@(_,ts) t@(_,h) = all (any (>=h)) [ neigh_height d m t | d <- all_direction ]

is_visible :: Map -> Tree -> Bool
is_visible m t = not $ is_hidden m t

solve input = sum $ map (fromEnum . (is_visible m)) ts
  where m@(_,ts) = parse input

input :: IO [String]
input = do
  h <- openFile "input" ReadMode
  c <- hGetContents h
  return (lines c)

viewing_distance :: Map -> Dir -> Tree -> Int
viewing_distance m d t@(_,h) = min (distance_to_edge m d t) (1 + length viewable_l)
  where heights = filter (>=0) $ neigh_height d m t
        viewable_l = takeWhile (<h) heights

distance_to_edge :: Map -> Dir -> Tree -> Int
distance_to_edge _ North ((_,y),_) = y
distance_to_edge ((_,h),_) South ((_,y),_) = h - y - 1
distance_to_edge _ West ((x,_),_) = x
distance_to_edge ((w,_),_) East ((x,_),_) = w - x - 1

scenic_score :: Map -> Tree -> Int
scenic_score m t = product [viewing_distance m d t | d <- all_direction]

solve2 :: [String] -> Int
solve2 input = maximum $ map (scenic_score m) ts
  where m@(_,ts) = parse input
