import Data.Functor ((<$>))
import Data.List (intersperse, findIndex)
import Data.Maybe (fromMaybe)
import Data.Either (either)
import Data.Map (alter, empty, assocs)
import Data.Char (isPunctuation, toLower)
import System.Environment (getArgs)
import GHC.Exts (sortWith)

isVowel c = c `elem` "аеиоуыэюя"

type StemWord = (String, (Int, Int))

toStemWord s =
    let boundProcess p rs =
            let mbRI = (+ 1) <$> findIndex p rs
            in fromMaybe (length rs) mbRI

        rvI = boundProcess isVowel s

        rvS = drop rvI s
        r1I = boundProcess (not . isVowel) rvS
    
        r1S = drop r1I rvS
        r2I = boundProcess (not . isVowel) r1S

        l = length s
    in (reverse s, (l - rvI, l - rvI - r1I - r2I))

fromStemWord (s, _) = reverse s

type EndBound = (Int, Int) -> Int
rv = snd
r2 = fst


type RemoveTry = Either StemWord StemWord

unwrap :: RemoveTry -> StemWord
unwrap = either id id

continueAnyway tr = Right $ unwrap tr

continueIfRemoved (Left sw) = Right sw
continueIfRemoved (Right sw) = Left sw

type Endings = (EndBound, [String], [String], [Char])

perfectiveGerund = (rv, ["ив", "ивши", "ившись", "ыв", "ывши", "ывшись"],
    ["в", "вши", "вшись"], "ая")

adjective = (rv, ["ее", "ие", "ые", "ое", "ими", "ыми", "ей", "ий", "ый", "ой",
    "ем", "им", "ым", "ом", "его", "ого", "ему", "ому", "их", "ых", "ую", "юю",
    "ая", "яя", "ою", "ею"], [], "")

participle = (rv, ["ивш", "ывш", "ующ"], ["ем", "нн", "вш", "ющ", "щ"], "ая")

reflexive = (rv, ["ся", "сь"], [], "")

verb = (rv, ["ила", "ыла", "ена", "ейте", "уйте", "ите", "или", "ыли", "ей",
    "уй", "ил", "ыл", "им", "ым", "ен", "ило", "ыло", "ено", "ят", "ует", "уют",
    "ит", "ыт", "ены", "ить", "ыть", "ишь", "ую", "ю"],
    ["ла", "на", "ете", "йте", "ли", "й", "л", "ем", "н", "ло", "но", "ет",
     "ют", "ны", "ть", "ешь", "нно"], "ая")

noun :: Endings
noun = undefined
superlative :: Endings
superlative = undefined
derivational :: Endings
derivational = undefined
adjectival :: Endings
adjectival = undefined

i :: Endings
i = undefined
softSign :: Endings
softSign = undefined

doubleN :: Endings
doubleN = undefined

tryRemove :: Endings -> StemWord -> RemoveTry
tryRemove = undefined


step1 :: StemWord -> RemoveTry
step1 sw = do
    sw2 <- tryRemove perfectiveGerund sw
    let way2 sw3 = do
            sw4 <- continueAnyway $ tryRemove reflexive sw3
            sw5 <- tryRemove adjectival sw4
            sw6 <- tryRemove verb sw5
            tryRemove noun sw6
    way2 sw2

step2 :: StemWord -> RemoveTry
step2 = tryRemove i

step3 :: StemWord -> RemoveTry
step3 = tryRemove derivational

step4 :: StemWord -> RemoveTry
step4 sw = do
    sw2 <- tryRemove doubleN sw
    let way2 sw3 = do
        sw4 <- continueIfRemoved $ tryRemove superlative sw3
        tryRemove doubleN sw4
    sw5 <- way2 sw2
    tryRemove softSign sw5


stem :: String -> String
stem = fromStemWord . steps . toStemWord
    where steps = foldr (.) id $ map (unwrap .) [step4, step3, step2, step1]


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