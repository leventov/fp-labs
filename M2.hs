module M2 () where


import qualified M0


slice sq start count = take (count) . drop (start - 1) $ sq

fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
fibSlice = slice fibs


primes = filter M0.prime [1..]
primeSlice = slice primes


range :: (Integral a) => a -> a -> a -> [a]  
range s e d = [s, s + d .. e]


lt a b = (a - 0.000001) < b

rangeF :: Float -> Float -> Float -> [Float]
rangeF s e d = takeWhile (`lt` e) [s, s + d ..]


qsort cmp []   = []
qsort cmp [x]  = [x]
qsort cmp (x:ys) =
    lower ++ [x] ++ higher
    where part p = qsort cmp $ filter p ys
          by op = \y -> (y `cmp` x) `op` 0
          lower = part $ by (<=)
          higher = part $ by (>)
          
sortByLength :: [[a]] -> [[a]]
sortByLength = qsort (\s1 s2 -> length s1 - length s2)
        



