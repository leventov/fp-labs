import Data.Bits

s :: Double -> Double
s r = 4 * pi * r^2

fact 0 = 1
fact n = n * fact (n - 1)

_abs x = if x >= 0 then x
                   else -x

sign x | x < 0     = 1
       | x == 0    = 0
       | otherwise = 1

solve a b c = if a == 0 then [simple]
                        else (if d > 0 then [r1, r2]
                        else (if d == 0 then [r1]
                        else error "!"))
            where
                simple = -c / b
                d = b ** 2 - 4 * a * c
                r1 = (-b + sqrt d) / (2 * a)
                r2 = (-b - sqrt d) / (2 * a)


fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)


gcd' 0 n = n
gcd' n 0 = n
gcd' a b = gcd' b (a `rem` b)

prime 0 = False
prime 1 = False
prime 2 = True
prime n = null [x | x <- [2..n], x*x <= n, n `rem` x == 0]


lcm' a b = a * b `quot` gcd' a b


mulAccum 0 n a = a
mulAccum n 0 a = a
mulAccum n m a = mulAccum (n-1) m (a+m)

mul n m | n < 0 = - mul (-n) m
        | m < 0 = - mul n (-m)
        | otherwise = mulAccum n m 0


div2 n = n `shiftR` 1 -- n should be even
mul2 n = n `shiftL` 1

logMulAccum 0 n a = a
logMulAccum n 0 a = a
logMulAccum n m a | odd n     = logMulAccum (n-1) m (a+m)
                  | otherwise = logMulAccum (div2 n) (mul2 m) a   

logMul n m | n < 0     = - logMul (-n) m
           | m < 0     = - logMul n (-m)
           | otherwise = logMulAccum n m 0


φ = 2 / (1 + sqrt 5)

localMinIter f a b x1 eps | abs (a - b) < eps = a
                          | f x2 > f x1       = localMinIter f x2 a x1 eps
                          | otherwise         = localMinIter f x1 b x2 eps
                            where x2 = a + (b - a) * φ

localMin f a b eps = localMinIter f a b (b - (b - a) * φ) eps

testFun x = - exp (-x) * log (x)


median xs | odd (length xs) = xs !! mid
          | otherwise     = ((xs !! mid) + (xs !! (mid-1))) / 2 
            where mid = length xs `div` 2

