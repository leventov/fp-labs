module M0 (prime, median) where


import Data.Bits
import Data.List


s :: Double -> Double
s r = 4 * pi * r^2


fact n = product [1..n]


abs' x | x >= 0    = x
       | otherwise = -x


sign x | x < 0     = 1
       | x == 0    = 0
       | otherwise = -1


solve a b c
    | a == 0    = [simple]
    | d > 0     = [r1, r2]
    | d == 0    = [r1]
    | otherwise = error ("!")
    where simple = -c / b
          d = b ** 2 - 4 * a * c
          r1 = (-b + sqrt d) / (2 * a)
          r2 = (-b - sqrt d) / (2 * a)


fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)


gcd' 0 n = n
gcd' n 0 = n
gcd' a b = gcd' b (a `rem` b)


divides x n = n `rem` x == 0

prime 0 = False
prime 1 = False
prime 2 = True
prime n = not $ any (`divides` n) possibleDivs
          where possibleDivs = takeWhile (\x -> x * x <= n) [2..n]


lcm' a b = a * b `quot` gcd' a b


mul n m | n < 0     = - mul (-n) m
        | otherwise = (iterate (+ m) 0) !! n 


div2 n = n `shiftR` 1 -- n should be even
mul2 n = n `shiftL` 1

logMulAccum 0 n a = a
logMulAccum n 0 a = a
logMulAccum n m a
    | odd n     = logMulAccum (n - 1) m (a + m)
    | otherwise = logMulAccum (div2 n) (mul2 m) a   

logMul n m
    | n < 0     = - logMul (-n) m
    | m < 0     = - logMul n (-m)
    | otherwise = logMulAccum n m 0


φ = 2 / (1 + sqrt 5)

localMinIter f eps a b x1
    | abs (a - b) < eps = a
    | f x2 > f x1       = rec x2 a x1
    | otherwise         = rec x1 b x2
    where rec = localMinIter f eps
          x2 = a + (b - a) * φ

localMin f a b eps = localMinIter f eps a b (b - (b - a) * φ)

testFun x = - exp (-x) * log (x)


median xs
    | odd len   = mid
    | otherwise = (mid + (xs !! (midI - 1))) / 2 
    where len = length xs
          midI = len `div` 2
          mid = xs !! midI




