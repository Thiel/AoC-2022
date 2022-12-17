import Text.Parsec
import Text.Parsec.Number
import System.IO
import Debug.Trace
import Data.List

sample = unlines [
  "[1,1,3,1,1]",
  "[1,1,5,1,1]",
  "",
  "[[1],[2,3,4]]",
  "[[1],4]",
  "",
  "[9]",
  "[[8,7,6]]",
  "",
  "[[4,4],4,4]",
  "[[4,4],4,4,4]",
  "",
  "[7,7,7,7]",
  "[7,7,7]",
  "",
  "[]",
  "[3]",
  "",
  "[[[]]]",
  "[[]]",
  "",
  "[1,[2,[3,[4,[5,6,7]]]],8,9]",
  "[1,[2,[3,[4,[5,6,0]]]],8,9]"]

data Packet = Integer Int | List [Packet]
  deriving (Eq)

parse_entry :: Parsec String st Packet
parse_entry = do
  x <- parse_list
       <|> parse_int
  return x

parse_list :: Parsec String st Packet
parse_list = between (string "[") (string "]") parse_list_elt

parse_list_elt :: Parsec String st Packet
parse_list_elt = do
  entries <- parse_entry `sepBy` (string ",")
  return $ List entries

parse_int :: Parsec String st Packet
parse_int = do
  i <- int
  return $ Integer i

parse_problem :: Parsec String st [(Packet, Packet)]
parse_problem = (do left <- parse_entry
                    newline
                    right <- parse_entry
                    spaces
                    rest <- parse_problem
                    return $ (left, right) : rest)
                <|> return []

solve input = sum [ idx | (idx, ord) <- zip [1..] pair_ord, ord]
  where Right i = parse parse_problem "" input
        pair_ord = map (\(a,b) -> a <= b) i

instance Ord Packet where
  Integer a `compare` Integer b = a `compare` b
  as@List{} `compare` b@Integer{} = as `compare` (List [b])
  a@Integer{} `compare` bs@List{} = List [a] `compare` bs
  List as `compare` List bs = as `compare` bs

instance Show Packet where
  show (Integer a) = show a
  show (List as) = show as

input :: IO String
input = do
  h <- openFile "input" ReadMode
  hGetContents h

debug input = unlines $ map custom_show $ zip i pair_ord
  where Right i = parse parse_problem "" input
        pair_ord = map (\(a,b) -> a <= b) i

custom_show ((l,r), i) = show l ++ "\n" ++ show r ++ "\n" ++ show i ++ "\n"

divider_packet :: [Packet]
divider_packet = [List [List [Integer 2]], List [List [Integer 6]]]

flatten_input :: [(Packet, Packet)] -> [Packet]
flatten_input i = map fst i ++ map snd i

Right s = parse parse_problem "" sample

solve2 :: String -> Int
solve2 input = product $ map fst $ filter (snd) $ zip [1..] $ map (\x -> x `elem` divider_packet) $ sort $ flatten_input i ++ divider_packet
  where Right i = parse parse_problem "" input
