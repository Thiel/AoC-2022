import Data.List

sample :: String
sample = "bvwbjplbgvbhsrlpgdmjqwftvncz"

search_uniq_seq :: Int -> String -> Maybe Int
search_uniq_seq size ss = (+size) <$> (elemIndex size
                                    $ map (length . group . sort)
                                    $ (transpose . take size . tails) ss)

solve :: String -> Maybe Int
solve = search_uniq_seq 4

sample2 :: String
sample2 = "mjqjpqmgbljsphdztnvjfqwrcgsmlb"

solve2 :: String -> Maybe Int
solve2 = search_uniq_seq 14
