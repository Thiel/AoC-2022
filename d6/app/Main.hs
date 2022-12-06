import Data.List

sample :: String
sample = "bvwbjplbgvbhsrlpgdmjqwftvncz"

solve :: String -> Maybe Int
solve input = (+4) <$> (elemIndex 4 $ map (length . group . sort) $ (transpose . take 4 . tails) input)

sample2 :: String
sample2 = "mjqjpqmgbljsphdztnvjfqwrcgsmlb"

solve2 :: String -> Maybe Int
solve2 input = (+14) <$> (elemIndex 14 $ map (length . group . sort) $ (transpose . take 14 . tails) input)
