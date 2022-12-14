import System.IO
import Data.List
import Data.Maybe

data Move = Rock | Paper | Scissors
  deriving (Show, Eq)
type Round = (Move, Move)

sample = [
  "A Y",
  "B X",
  "C Z"
  ]

interpret_move :: String -> Move
interpret_move "A" = Rock
interpret_move "B" = Paper
interpret_move "C" = Scissors
interpret_move "X" = Rock
interpret_move "Y" = Paper
interpret_move "Z" = Scissors

parse :: [String] -> [Round]
parse input = map (\(their:our:[]) -> (their, our)) $ map (map interpret_move) $ map words input

point_shape :: Round -> Int
point_shape (_, Rock) = 1
point_shape (_, Paper) = 2
point_shape (_, Scissors) = 3

data Outcome = Win | Draw | Lose
  deriving (Show, Eq)

win_against Rock = Paper
win_against Paper = Scissors
win_against Scissors = Rock

outcome :: Round -> Outcome
outcome (their, our) | win_against their == our = Win
outcome (their, our) | their == our = Draw
outcome _ | otherwise = Lose

point_outcome :: Outcome -> Int
point_outcome Win = 6
point_outcome Draw = 3
point_outcome Lose = 0

point_round :: Round -> Int
point_round r = point_shape r + (point_outcome . outcome) r

solve :: [String] -> Int
solve input = sum $ map point_round $ parse input

apply :: ([String] -> Int) -> IO Int
apply s = do
  h <- openFile "app/input" ReadMode
  c <- hGetContents h
  return $ s (lines c)

-- --

type Strategy = (Move, Outcome)
fix_interpretation :: Round -> Strategy
fix_interpretation (a, Rock) = (a, Lose)
fix_interpretation (a, Paper) = (a, Draw)
fix_interpretation (a, Scissors) = (a, Win)

parse2 :: [String] -> [Strategy]
parse2 input = map fix_interpretation $ parse input

my_move :: Strategy -> Move
my_move (their_move, expected_outcome) = possible_move !! idx_meet_expectation
  where possible_move = [Rock, Paper, Scissors]
        idx_meet_expectation = fromJust $ findIndex (==expected_outcome) $
                               map outcome $ zip (repeat their_move) possible_move

plan :: Strategy -> Round
plan s = (fst s, my_move s)

solve2 :: [String] -> Int
solve2 input = sum $ map (point_round . plan) $ parse2 input
