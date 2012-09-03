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


gcd1 :: Integral a => a -> a -> a
gcd1 a b | a == 0    = b
         | b == 0    = a
         | otherwise = gcd1 b (a rem b)