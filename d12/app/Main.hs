import Algorithm.Search
import Data.Maybe
import Data.Tuple
import Data.Char
import System.IO
import Debug.Trace

sample = [
  "Sabqponm",
  "abcryxxl",
  "accszExk",
  "acctuvwj",
  "abdefghi"]

type Elevation = Char
type Pos = (Int, Int)
type MapElt = (Pos, Elevation)
type Map = [MapElt]

parse :: [String] -> Map
parse input = do
  y <- [0 .. length input - 1]
  x <- [0 .. (length.head) input - 1]
  return ((x,y), input !! y !! x)

neighbors :: Map -> Pos -> [Pos]
neighbors m (x,y) = filter is_valid $ filter (\p -> isJust $ lookup p m)  [(x,y+1), (x,y-1), (x+1,y), (x-1,y)]
  where is_valid pos = cost m (x,y) pos == 1

cost :: Map -> Pos -> Pos -> Float
cost m from_pos to_pos = if ord from_elevation + 1 >= ord to_elevation then
                           1
                         else
                           10000000
  where from_elevation = get_elevation m from_pos
        to_elevation = get_elevation m to_pos

reverse_cost m from_pos to_pos = cost m to_pos from_pos
reverse_neighbors m (x,y) = filter is_valid $ filter (\p -> isJust $ lookup p m)  [(x,y+1), (x,y-1), (x+1,y), (x-1,y)]
  where is_valid pos = reverse_cost m (x,y) pos == 1

get_elevation :: Map -> Pos -> Elevation
get_elevation m pos = case maybe_e of
                        Nothing -> '~'
                        Just 'S' -> 'a'
                        Just 'E' -> 'z'
                        Just e -> e
  where maybe_e = lookup pos m


get_start_pos :: Map -> Pos
get_start_pos m = fromJust $ lookup 'S' $ map swap m

get_end_pos :: Map -> Pos
get_end_pos m = fromJust $ lookup 'E' $ map swap m

solve input start = trace ("Sloving " ++ show start) $ length $ fromJust $ snd <$> dijkstra neighbors' cost' (==get_end_pos m) start
  where m = parse input
        neighbors' = neighbors m
        cost' = cost m

input :: IO [String]
input = do
  h <- openFile "input" ReadMode
  c <- hGetContents h
  return (lines c)
s = parse sample

possible_starts :: Map -> [Pos]
possible_starts m = map fst $ filter (\(_,e) -> e=='a' || e=='S') m

solve2 input = dijkstra neighbors' cost' (\x -> get_elevation m x == 'a') (get_end_pos m)
  --length $ fromJust $ snd <$>
  where m = parse input
        neighbors' = reverse_neighbors m
        cost' = reverse_cost m

main = do
  i <- input
  print $ solve2 i
