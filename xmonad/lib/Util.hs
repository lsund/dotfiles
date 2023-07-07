module Util where

roundN :: Double -> Int -> Double
roundN x n = fromInteger (round $ x * (10 ^ n)) / (10.0 ^^ n)

every n xs =
  case drop (n - 1) xs of
    (y:ys) -> y : every n ys
    [] -> []
