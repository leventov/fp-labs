module Stat () where

import Data.List (sort)

median xs
    | odd len   = mid
    | otherwise = realToFrac((mid + (xs !! (midI - 1)))) / 2 
    where len = length xs
          midI = len `div` 2
          mid = xs !! midI

percentile percent xs = realToFrac $ (!!) (sort xs) $ round $ (fromIntegral $ length xs) * (percent / 100)

q1 :: (Real a, Fractional b) => [a] -> b
q1 = percentile 25

q3 :: (Real a, Fractional b) => [a] -> b
q3 = percentile 75

iqr xs = q3 xs - q1 xs

lowOutlinerBound xs = q1 xs - 1.5 * iqr xs
highOutlinerBound xs = q3 xs + 1.5 * iqr xs


isOutliner xs x =
    fx < lowBound || fx > highBound
    where lowBound = lowOutlinerBound xs
          highBound = highOutlinerBound xs
          fx = realToFrac x

regular xs = filter (not . isOutliner xs) xs
outliners xs = filter (isOutliner xs) xs
