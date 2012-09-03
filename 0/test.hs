s :: Double -> Double
s r = 4 * pi * r^2

fact 0 = 1
fact n = n * fact (n - 1)

_abs x = if x >= 0 then x
                   else -x

sign x | x < 0     = 1
       | x == 0    = 0
       | otherwise = 1