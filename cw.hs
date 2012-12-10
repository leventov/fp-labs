import Control.Applicative

localMin :: ([Double] -> Double) -> Int -> [Double]
localMin f n = minIter f 1 $ replicate n 0

minH = 0.0000001

minIter f h b = if h <= minH then b else
    let nb = research f b h
    in if nb == b then minIter f (h * 0.1) b
        else let [(.-.), (.+.), (.*.)] = map liftA2 [(-), (+), (*)]
                 vb = ZipList b
                 vnb = ZipList nb
                 db = vnb .-. vb
                 pack = ZipList . (replicate $ length b) . fromIntegral 
                 zz li = map (getZipList .
                    (\i -> vb .+. (db .*. (pack i)))) li
                 nnb = snd $ last $ (:) (b, nb) $
                    takeWhile (\(v1, v2) -> f v1 > f v2) $
                        zip (zz [1..]) (zz [2..])
             in minIter f (h * 0.5) nnb

research f b h = foldl choose [] b
    where choose computed bi =
            let cf x =
                    let (p1, px:p2) = splitAt (length computed) b
                    in f $ computed ++ x:p2
                steps = [bi, bi - h, bi + h]
                stepValues = map cf steps
                m = minimum stepValues
                minI = head $ filter ((== m) . (stepValues !!)) [0..]
                minStep = steps !! minI
            in computed ++ [minStep]

f1 (x1:x2:[]) = 100 * ((x2 - (x1 ^ 2)) ^ 2) + ((1 - x1) ^ 2)

f2 (x:y:z:[]) = (x - 2) ^ 2 + (y - 5) ^ 2 + (z + 2) ^ 4

f3 (a:b:c:d:[]) = (a + 10 * b) ^ 2 + 5 * ((c - d) ^ 2) +
                  (b - 2 * c) ^ 4 + 10 * ((a - d) ^ 4)

f4 (x:y:[]) = sum [((exp (a * x) - exp (a * y)) -
                    (exp a - exp (10 * a))) ^ 2 | a <- [-1, 0]] 