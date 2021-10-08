module Util where
import GHC.Arr (accum)

splitEvery :: Int -> [a] -> [[a]]
splitEvery 0 xs = [xs]
splitEvery n xs = reverse $ go xs []
  where
    go :: [a] -> [[a]] -> [[a]]
    go [] acc = acc
    go ys acc = go (drop n ys) (take n ys : acc)
