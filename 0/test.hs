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

