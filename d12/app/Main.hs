import Data.Tuple
import Data.Maybe
import Data.Char
import Data.List
import System.IO

sample = [
  "Sabqponm",
  "abcryxxl",
  "accszExk",
  "acctuvwj",
  "abdefghi"]

type Elevation = Char
type Pos = (Int, Int)
type Map = [(Pos, Elevation)]

data Move = U | D | L | R
  deriving Show

parse :: [String] -> Map
parse input = do
  y <- [0 .. length input - 1]
  x <- [0 .. (length.head) input - 1]
  return ((x,y), input !! y !! x)

s = parse sample

get_start_pos :: Map -> Pos
get_start_pos m = fromJust $ lookup 'S' $ map swap m

get_end_pos :: Map -> Pos
get_end_pos m = fromJust $ lookup 'E' $ map swap m

s_pos = get_start_pos s
e_pos = get_end_pos s

get_elevation :: Map -> Pos -> Elevation
get_elevation m pos = case maybe_e of
                        Nothing -> '~'
                        Just 'S' -> 'a'
                        Just 'E' -> 'z'
                        Just e -> e
  where maybe_e = lookup pos m

pos_after_move :: Pos -> Move -> Pos
pos_after_move (x,y) U = (x,y-1)
pos_after_move (x,y) D = (x,y+1)
pos_after_move (x,y) L = (x-1,y)
pos_after_move (x,y) R = (x+1,y)

all_dirs :: [Move]
all_dirs = [U, D, L, R]

is_valid_move :: Map -> Pos -> Move -> Bool
is_valid_move m pos mv = ord after_e <= ord before_e + 1
  where before_e = get_elevation m pos
        after_e = get_elevation m (pos_after_move pos mv)

neighbors :: Pos -> [Pos]
neighbors p = map (pos_after_move p) [U, D, L, R]

paths :: Map -> Pos -> Pos -> [Pos] -> [[Pos]]
paths _ start stop _ | start == stop = [[start]]
paths m start stop visited = map (start:) $ concat $ map (\new_pos -> paths m new_pos stop (start:visited)) not_visited_poss
  where valid_move = filter (is_valid_move m start) all_dirs
        not_visited_poss = map (pos_after_move start) valid_move \\ visited

get_move :: Pos -> Pos -> Move
get_move from to = fromJust $ lookup to neigh
  where neigh = zip (map (pos_after_move from) all_dirs) all_dirs

ps =  paths s s_pos e_pos []
shortest_path = fromJust $ lookup 26 $ zip (map length $ ps ) ps

solve input = (minimum $ map length $ paths m start stop []) - 1
  where m = parse input
        start = get_start_pos m
        stop = get_end_pos m


input :: IO [String]
input = do
  h <- openFile "input" ReadMode
  c <- hGetContents h
  return (lines c)

main :: IO ()
main = do
  i <- input
  print $ solve i
