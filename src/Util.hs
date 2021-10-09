module Util where

splitEvery :: Int -> [a] -> [[a]]
splitEvery 0 xs = [xs]
splitEvery n xs = map (take n) $ go xs
  where
    go :: [a] -> [[a]]
    go [] = []
    go ys = ys : go (drop n ys)
