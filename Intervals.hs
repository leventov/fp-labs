import Data.List
import Data.Function

import Control.Applicative
import Control.Monad

imap2 mins maxs f a b =
	let pairs = f <$> a <*> b
	in join $ [mins, maxs] <*> [pairs]

-- real interval 
rimap2 = imap2 (pure . minimum) (pure . maximum)

add = rimap2 (+)
sub = rimap2 (-)
mul = rimap2 (*)
di  = rimap2 (/)

-- "pr" for parallel resistance
pr1 = rimap2 $ \x y -> x * y / (x + y)
pr2 = rimap2 $ \x y -> 1 / (1/x + 1/y)

-- vectors
vexts f xs =
    let n = minimum $ length <$> xs
    in [f (compare `on` (!! i)) xs | i <- [0..n - 1]]
vmins = vexts minimumBy
vmaxs = vexts maximumBy
vimap2 = imap2 vmins vmaxs

vadd = vimap2 $ zipWith (+)

