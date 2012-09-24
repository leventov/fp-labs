import Control.Applicative

list (a, b) = [a, b]
pair [a, b] = (a, b)

imap :: (Fractional a, Ord a) => (a -> a -> a) -> (a, a) -> (a, a) -> (a, a)
imap f a b =
	let pairs = pure $ f <$> (list a) <*> (list b)
	in pair $ [minimum, maximum] <*> pairs

add = imap (+)
sub = imap (-)
mul = imap (*)

intervalDivisionError i =
	error ("Division by zeroing interval " ++ show i)
_ `di` i@(_, 0) = intervalDivisionError i
_ `di` i@(0, _) = intervalDivisionError i
a `di` b        = imap (/) a b

-- "pr" for parallel resistance
pr1 = imap $ \x y -> x * y / (x + y)
pr2 = imap $ \x y -> 1 / (1/x + 1/y)

