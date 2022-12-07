import Text.Parsec
import Data.Maybe
import Text.Parsec.String
import Data.Either

sample :: String
sample = unlines [
  "$ cd /",
  "$ ls",
  "dir a",
  "14848514 b.txt",
  "8504156 c.dat",
  "dir d",
  "$ cd a",
  "$ ls",
  "dir e",
  "29116 f",
  "2557 g",
  "62596 h.lst",
  "$ cd e",
  "$ ls",
  "584 i",
  "$ cd ..",
  "$ cd ..",
  "$ cd d",
  "$ ls",
  "4060174 j",
  "8033020 d.log",
  "5626152 d.ext",
  "7214296 k"
  ]

data FS = File String Int | Dir String [FS]
  deriving Show

parse_cd_in :: Parsec String st [FS]
parse_cd_in = do
  string "$ cd "
  ss <- many1 letter <|> string "/"
  newline
  fs <- parse_ls
  nested_dirs <- many (try parse_cd_in) <|> return []
  try parse_cd_out <|> return ()
  return $ [Dir ss (fs++concat nested_dirs)]

parse_ls :: Parsec String st [FS]
parse_ls = do
  string "$ ls" >> newline
  fs <- many1 $ choice [parse_ls_file, parse_ls_dir]
  return $ catMaybes fs

parse_ls_file :: Parsec String st (Maybe FS)
parse_ls_file = do
  size <- read <$> many1 digit
  space
  name <- many1 (letter <|> char '.')
  newline
  return $ Just (File name size)

parse_ls_dir :: Parsec String st (Maybe FS)
parse_ls_dir = string "dir " >> many1 letter >> newline >> return Nothing

parse_cd_out :: Parsec String st ()
parse_cd_out = string "$ cd .." >> newline >> return ()

parse_input :: Parsec String st [FS]
parse_input = parse_cd_in

input :: IO [FS]
input = fromRight [] <$> parseFromFile parse_input "input"

weird_dir_size :: FS -> Int
weird_dir_size (File _ s) = s
weird_dir_size (Dir _ f_or_d) = sum $ map weird_dir_size f_or_d

parse_sample = [Dir "/" [File "b.txt" 14848514,File "c.dat" 8504156,Dir "a" [File "f" 29116,File "g" 2557,File "h.lst" 62596,Dir "e" [File "i" 584]],Dir "d" [File "j" 4060174,File "d.log" 8033020,File "d.ext" 5626152,File "k" 7214296]]]

solve :: [FS] -> Int
solve (d@(Dir _ fs):xs) | weird_dir_size d <= 100000 = weird_dir_size d + solve fs + solve xs
                        | otherwise = solve fs + solve xs
solve (_:xs) = solve xs
solve [] = 0

filter_gt :: Int -> [FS] -> [FS]
filter_gt freeup_space (d@(Dir _ fs):xs) | weird_dir_size d >= freeup_space = d : filter_gt freeup_space fs ++ filter_gt freeup_space xs
                                    | otherwise = filter_gt freeup_space fs ++ filter_gt freeup_space xs
filter_gt freeup_space (_:xs) = filter_gt freeup_space xs
filter_gt _ [] = []

solve2 :: [FS] -> Int
solve2 fs = minimum $ map weird_dir_size $ filter_gt freeup_space fs
  where freeup_space = 30000000 - (70000000 - (head $ map weird_dir_size fs))
