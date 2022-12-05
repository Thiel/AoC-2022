import System.IO
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Token
import Data.List.Split
import Data.List
import Data.Maybe
import Data.Either

type Crate = Char
type StackId = Int

parse_crate :: Parsec String st (Maybe Crate)
parse_crate = do
    x <- choice [
      string "   ",
      between (char '[') (char ']') (do c <- letter
                                        return [c])
      ]
    return $ if x == "   " then Nothing else Just (head x)

parse_crates :: Parsec String st [Maybe Crate]
parse_crates = do
  c <- parse_crate
  space
  cs <- try parse_crates <|> return []
  return (c:cs)

parse_crates_count :: Parsec String st Int
parse_crates_count = do
  ids <- many1 (do spaces
                   id <- digit
                   spaces
                   return id)
  return $ length ids

type Move = (Int, StackId, StackId)
parse_move :: Parsec String st Move
parse_move =
  let number = read <$> many digit
  in do
    string "move "
    n <- number
    string " from "
    from <- number
    string " to "
    to <- number
    spaces
    return (n, from, to)

type Stack = [Crate]

parse_input :: Parsec String st ([Stack], [Move])
parse_input = do
  crates <- try parse_crates
  stack_count <- parse_crates_count
  moves <- many parse_move
  let stacks = map catMaybes $ transpose $ chunksOf stack_count crates
    in return (stacks, moves)

sample :: IO ([Stack], [Move])
sample = fromRight ([],[]) <$> parseFromFile parse_input "sample"

apply_move :: Move -> [Stack] -> [Stack]
apply_move (n,from,to) ss = ss''
  where from_stack = ss !! (from-1)
        to_stack = ss !! (to-1)
        from_stack' = drop n from_stack
        to_stack' = (reverse . take n) from_stack ++ to_stack
        ss' = let (lss, rss) = splitAt (from-1) ss
              in lss ++ [from_stack'] ++ tail rss
        ss'' = let (lss, rss) = splitAt (to-1) ss'
               in lss ++ [to_stack'] ++ tail rss

solve :: ([Stack], [Move]) -> String
solve (stacks, moves) = map head $ foldl (flip apply_move) stacks moves

input :: IO ([Stack], [Move])
input = fromRight ([],[]) <$> parseFromFile parse_input "input"

apply :: (([Stack],[Move]) -> String) -> IO String
apply s = s <$> input

apply_move_9001 :: Move -> [Stack] -> [Stack]
apply_move_9001 (n,from,to) ss = ss''
  where from_stack = ss !! (from-1)
        to_stack = ss !! (to-1)
        from_stack' = drop n from_stack
        to_stack' = take n from_stack ++ to_stack
        ss' = let (lss, rss) = splitAt (from-1) ss
              in lss ++ [from_stack'] ++ tail rss
        ss'' = let (lss, rss) = splitAt (to-1) ss'
               in lss ++ [to_stack'] ++ tail rss

solve2 :: ([Stack], [Move]) -> String
solve2 (stacks, moves) = map head $ foldl (flip apply_move_9001) stacks moves
