{-# LANGUAGE BangPatterns #-}
import System.IO
import Text.Parsec (parse, Parsec, many1, spaces, string, try, sepBy, char, (<|>), space)
import Text.Parsec.Number
import Control.Monad.State.Strict
import Data.List
import Control.Lens
import Data.Either
import Debug.Trace

sample = unlines [
  "Monkey 0:",
  "  Starting items: 79, 98",
  "  Operation: new = old * 19",
  "  Test: divisible by 23",
  "    If true: throw to monkey 2",
  "    If false: throw to monkey 3",
  "",
  "Monkey 1:",
  "  Starting items: 54, 65, 75, 74",
  "  Operation: new = old + 6",
  "  Test: divisible by 19",
  "    If true: throw to monkey 2",
  "    If false: throw to monkey 0",
  "",
  "Monkey 2:",
  "  Starting items: 79, 60, 97",
  "  Operation: new = old * old",
  "  Test: divisible by 13",
  "    If true: throw to monkey 1",
  "    If false: throw to monkey 3",
  "",
  "Monkey 3:",
  "  Starting items: 74",
  "  Operation: new = old + 3",
  "  Test: divisible by 17",
  "    If true: throw to monkey 0",
  "    If false: throw to monkey 1"]

type WorryLevel = Int
type Operation = Int -> Int
type Test = ((Int -> Bool), Int, Int)
data Monkey = Monkey
  [WorryLevel]
  Operation
  Test

instance Show Monkey where
  show (Monkey wl _ (_,t, f)) = "Monkey " ++ show wl ++ " (pred) " ++ show t ++ " " ++ show f

parse_monkey_id :: Parsec String st Int
parse_monkey_id = string "Monkey " >> do
  i <-int
  string ":"
  return i
parse_items :: Parsec String st [Int]
parse_items = do
  spaces
  string "Starting items: "
  xs <- sepBy int $ char ',' >> spaces
  return $ xs
parse_operand :: Parsec String st (Maybe Int)
parse_operand = try (string "old" >> return Nothing) <|> do
  i <- int
  return $ Just i
parse_operator :: Parsec String st (Int -> Int -> Int)
parse_operator = do
  op <- char '+' <|> char '*'
  return $ case op of
    '+' -> (+)
    '*' -> (*)
parse_operation :: Parsec String st Operation
parse_operation = spaces >> string "Operation: new = " >> do
  lo <- parse_operand
  space
  op <- parse_operator
  space
  ro <- parse_operand
  return $ case (lo, ro) of
    (Nothing, Nothing) -> (\x -> x `op` x)
    (Nothing, Just x) -> (`op` x)
    (Just x, Nothing) -> (x `op`)
parse_test :: Parsec String st (Int->Bool)
parse_test = spaces >> string "Test: divisible by " >> do
  divisible_by <- int
  return $ (\x-> x `mod` divisible_by == 0)
parse_test_stmt :: Parsec String st Int
parse_test_stmt = spaces >> string "If " >> (try $ string "true" <|> string "false")
                  >> string": throw to monkey " >> int

parse_monkey :: Parsec String st Monkey
parse_monkey = do
  parse_monkey_id
  items <- parse_items
  operation <- parse_operation
  test_predicate <- parse_test
  test_true <- parse_test_stmt
  test_false <- parse_test_stmt
  spaces
  return $ Monkey items operation (test_predicate, test_true, test_false)

type MonkeyBoard = [Monkey]
parse_monkey_board :: Parsec String st MonkeyBoard
parse_monkey_board = many1 $ parse_monkey

sub_round :: Int -> State MonkeyBoard Int
sub_round monkey_idx = do
  monkey_board <- get
  let monkey@(Monkey items op (test, true_monkey_idx, false_monkey_idx)) = monkey_board !! monkey_idx
      true_monkey@(Monkey tm_items tm_op tm_test) = monkey_board !! true_monkey_idx
      false_monkey@(Monkey fm_items fm_op fm_test) = monkey_board !! false_monkey_idx
      new_items = eval_items monkey
      (t_items, f_items) = partition test new_items
      updated_monkey = Monkey [] op (test, true_monkey_idx, false_monkey_idx)
      updated_true_monkey = Monkey (tm_items ++ t_items) tm_op tm_test
      updated_false_monkey = Monkey (fm_items ++ f_items) fm_op fm_test
    in
    do put $ monkey_board & element monkey_idx .~ updated_monkey
       monkey_board <- get
       put $ monkey_board & element true_monkey_idx .~ updated_true_monkey
       monkey_board <- get
       put $ monkey_board & element false_monkey_idx .~ updated_false_monkey
       return $ length items

type InspectCount = Int
full_round :: State MonkeyBoard [InspectCount]
full_round = do
  monkey_board <- get
  inspected_counts <- mapM sub_round [0..length monkey_board - 1]
  return $ inspected_counts

eval_items :: Monkey -> [WorryLevel]
eval_items (Monkey items op _) = do
  item <- items
  return $ op item `mod` (11*7*13*3*17*2*5*19) -- (23*19*13*17)

-- `div` 3

round_n :: Int -> State MonkeyBoard [[InspectCount]]
round_n n = do
  replicateM n full_round

solve input = product . take 2 . reverse . sort $ foldr1 (zipWith (+)) $ evalState (round_n 10000) $ is
  where Right is = parse parse_monkey_board "" input

puzzle_input :: IO String
puzzle_input = do
  h <- openFile "input" ReadMode
  c <- hGetContents h
  return $ c

main :: IO ()
main = do
  i <- puzzle_input
  let s = solve i in
    print s
