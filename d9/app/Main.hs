import Control.Monad.State.Strict
import Debug.Trace
import Data.List
import System.IO

type Pos = (Int, Int)

data Dir = U | D | L | R
  deriving (Show, Eq)

sample = [
  "R 4",
  "U 4",
  "L 3",
  "D 1",
  "R 4",
  "D 1",
  "L 5",
  "R 2"
  ]

type Move = (Dir,Int)

parse :: [String] -> [Move]
parse input = map ((\(d:l:[])->(str2dir d, read l)) . words) input

str2dir :: String -> Dir
str2dir "U" = U
str2dir "D" = D
str2dir "L" = L
str2dir "R" = R

type BoardState = ([Pos], [Move])
apply_move_head :: State BoardState ()
apply_move_head = do
  (ps, ms) <- get
  case (ps,ms) of
    ((x,y):ts,(_,0):ms') -> do
      put ((x,y):ts, ms')
      apply_move_head
    ((x,y):ts,(U,l):ms') -> put ((x,y+1):ts, (U,l-1):ms')
    ((x,y):ts,(D,l):ms') -> put ((x,y-1):ts, (D,l-1):ms')
    ((x,y):ts,(R,l):ms') -> put ((x+1,y):ts, (R,l-1):ms')
    ((x,y):ts,(L,l):ms') -> put ((x-1,y):ts, (L,l-1):ms')
    (_,[]) -> return ()
--  get >>= traceShowM
  return ()

get_tail_pos :: State BoardState Pos
get_tail_pos = do
  (ts,_) <- get
  return $ last ts

update_tail :: State BoardState ()
update_tail = do
  (ts, ms) <- get
  put (new_tail_pos ts, ms)
  return ()

new_tail_pos :: [Pos] -> [Pos]
new_tail_pos (h@(hx,hy):t@(tx,ty):ts) | hx == tx && abs(hy - ty) >= 2 = h : (new_tail_pos $ (tx, ty + signum (hy - ty)):ts)
                                      | hy == ty && abs(hx - tx) >= 2 = h : (new_tail_pos $ (tx + signum (hx - tx), ty):ts)
                                      | dist h t >= 3 = h : (new_tail_pos $ (tx + signum (hx - tx), ty + signum (hy - ty)):ts)
                                      | otherwise = h : (new_tail_pos $ t:ts)
new_tail_pos (x:[]) = [x]
new_tail_pos [] = []
dist :: Pos -> Pos -> Int
dist (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

is_done :: State BoardState Bool
is_done = do
  (_,ms) <- get
  return (ms == [])

resolve_tail_pos :: State BoardState [Pos]
resolve_tail_pos = do
  is_end <- is_done
  if is_end then
    return []
    else do
    apply_move_head
    update_tail
    p <- get_tail_pos
    ps <- resolve_tail_pos
    return $ p:ps

solve :: State BoardState Int
solve = do
  t_pos <- resolve_tail_pos
  return $ length $ group $ sort t_pos

input :: IO [String]
input = do
  h <- openFile "input" ReadMode
  c <- hGetContents h
  return $ lines c

solve_input :: IO Int
solve_input = do
  i <- input
  return $ evalState solve ([(0,0), (0,0)], parse i)

---

sample2 = [
  "R 4",
  "U 4",
  "L 3",
  "D 1",
  "R 4",
  "D 1",
  "L 5",
  "R 2"]

sample3 = [
  "R 5",
  "U 8",
  "L 8",
  "D 3",
  "R 17",
  "D 10",
  "L 25",
  "U 20"]

start_state :: BoardState
start_state = (replicate 10 (0,0), parse sample3)

solve2_input :: IO Int
solve2_input = do
  i <- input
  return $ evalState solve (replicate 10 (0,0), parse i)
