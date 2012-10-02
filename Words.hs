import Data.Map (alter, empty, assocs)
import Data.Char (isPunctuation, toLower)
import System.Environment (getArgs)
import GHC.Exts (sortWith)

perfectiveGerund rw = 

stem :: String -> String
stem = id

count Nothing  = Just 1
count (Just f) = Just $ f + 1

frequencyList = foldl (flip $ alter count) empty

main = do
    (fn:_) <- getArgs
    content <- readFile fn
    let adoptedContent = map toLower . filter (not . isPunctuation) $ content
        adoptedWords = map stem $ words adoptedContent
        fql = sortWith snd . assocs . frequencyList $ adoptedWords
        pprint (a, b) = a ++ " -> " ++ show b ++ "\n"
    putStr . concat . map pprint $ fql