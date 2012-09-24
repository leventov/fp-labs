import Data.Map

count Nothing  = Just 1
count (Just f) = Just $ f + 1

frequencyList = foldl (flip $ alter count) empty