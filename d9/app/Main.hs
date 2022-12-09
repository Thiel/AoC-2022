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

type BoardState = (Pos, Pos, [Move])
apply_move_head :: State BoardState ()
apply_move_head = do
  ((x,y), t, ms) <- get
  case ms of
    (_,0):ms' -> do
      put ((x,y), t, ms')
      apply_move_head
    (U,l):ms' -> put ((x,y+1), t, (U,l-1):ms')
    (D,l):ms' -> put ((x,y-1), t, (D,l-1):ms')
    (R,l):ms' -> put ((x+1,y), t, (R,l-1):ms')
    (L,l):ms' -> put ((x-1,y), t, (L,l-1):ms')
    [] -> return ()
--  get >>= traceShowM
  return ()

get_tail_pos :: State BoardState Pos
get_tail_pos = do
  (_,t,_) <- get
  return t

update_tail :: State BoardState ()
update_tail = do
  (h, t, ms) <- get
  put (h, new_tail_pos h t, ms)
  return ()

new_tail_pos :: Pos -> Pos -> Pos
new_tail_pos h@(hx,hy) t@(tx,ty) | hx == tx && abs(hy - ty) >= 2 = (tx, ty + signum (hy - ty))
                                 | hy == ty && abs(hx - tx) >= 2 = (tx + signum (hx - tx), ty)
                                 | dist h t >= 3 = (tx + signum (hx - tx), ty + signum (hy - ty))
                                 | otherwise = t

dist :: Pos -> Pos -> Int
dist (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

is_done :: State BoardState Bool
is_done = do
  (_,_,ms) <- get
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
  return $ evalState solve ((0,0), (0,0), parse i)

start_state :: BoardState
start_state = ((0,0), (0,0), parse sample)
