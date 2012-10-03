import Data.Functor ((<$>))
import Control.Applicative ((<*>))
import Control.Monad ((>=>), foldM)

import Data.Maybe (fromMaybe)
import Data.Either (either)
import Data.Char (isPunctuation, toLower)
import Data.List (intersperse, findIndex, isPrefixOf)
import Data.Map (alter, empty, assocs)

import System.Environment (getArgs)

import GHC.Exts (sortWith)
import Text.Printf (printf)

isVowel c = c `elem` "аеиоуыэюя"

data StemWord = StemWord {reversedString :: String,
                          rvBound :: Int, r2Bound :: Int}

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
    in StemWord (reverse s) (l - rvI) (l - rvI - r1I - r2I)

fromStemWord (StemWord s _ _) = reverse s


type RemoveTry = Either StemWord StemWord

unwrap :: RemoveTry -> StemWord
unwrap = either id id

continueAnyway tr = Right $ unwrap tr

continueIfRemoved (Left sw) = Right sw
continueIfRemoved (Right sw) = Left sw


type SuffixBound = (StemWord -> Int)

data Endings = Endings {endings :: [String], preceded :: [Char],
                        bound :: SuffixBound}

instance Show Endings where
    show (Endings es p _) = printf "Endings [%s %s someFun]" (show es) (show p)


perfectiveGerund = [
    Endings ["ив", "ивши", "ившись", "ыв", "ывши", "ывшись"] "" rvBound,
    Endings ["в", "вши", "вшись"] "ая" rvBound]

adjective = [Endings ["ее", "ие", "ые", "ое", "ими", "ыми", "ей", "ий",
    "ый", "ой", "ем", "им", "ым", "ом", "его", "ого", "ему", "ому", "их", "ых",
    "ую", "юю", "ая", "яя", "ою", "ею"] "" rvBound]

participle = [Endings ["ивш", "ывш", "ующ"] "" rvBound,
              Endings ["ем", "нн", "вш", "ющ", "щ"] "ая" rvBound]

reflexive = [Endings ["ся", "сь"] "" rvBound]

verb = [Endings ["ила", "ыла", "ена", "ейте", "уйте", "ите", "или", "ыли", "ей",
                 "уй", "ил", "ыл", "им", "ым", "ен", "ило", "ыло", "ено", "ят",
                 "ует", "уют", "ит", "ыт", "ены", "ить", "ыть", "ишь", "ую", "ю"
                 ] "" rvBound,
        Endings ["ла", "на", "ете", "йте", "ли", "й", "л", "ем", "н", "ло",
                 "но", "ет", "ют", "ны", "ть", "ешь", "нно"] "ая" rvBound]

noun = [Endings ["а", "ев", "ов", "ие", "ье", "е", "иями", "ями", "ами",
    "еи", "ии", "и", "ией", "ей", "ой", "ий", "й", "иям", "ям", "ием", "ем",
    "ам", "ом", "о", "у", "ах", "иях", "ях", "ы", "ь", "ию", "ью", "ю", "ия",
    "ья", "я"] "" rvBound]

superlative = [Endings ["ейш", "ейше"] "" rvBound]

derivational = [Endings ["ост", "ость"] "" r2Bound]

i = [Endings ["и"] "" rvBound]

softSign = [Endings ["ь"] "" rvBound]

doubleN = [Endings ["н"] "н" rvBound]

tryRemove :: [Endings] -> StemWord -> RemoveTry
tryRemove es = foldl1 (>=>) $ map tryRemove' es

tryRemove' :: Endings -> StemWord -> RemoveTry
tryRemove' (Endings es ps bound) sWord = foldM tryRemoveEnding sWord rEndings
    where
        rEndings = map reverse es
        withP = (length ps) == 0
        endingL e = length (e) + (if withP then 0 else 1)
        
        tryRemoveEnding sw@(StemWord rString rv r2) rEnding = 
            let el = endingL rEnding
                endingIsSuffix = isPrefixOf rEnding rString
                endingInBound = el <= bound sw
                properlyPreceded = not withP || rString !! (el - 1) `elem` ps
                reminder = StemWord (drop el rString) (rv - el) (r2 - el)
            in if endingInBound && endingIsSuffix && properlyPreceded
                    then Left reminder
                    else Right sw
         


tryRemoveAdjectival sw = do
    sw1 <- continueAnyway $ tryRemove adjective sw
    tryRemove participle sw1


step1 :: StemWord -> RemoveTry
step1 sw = do
    sw2 <- tryRemove perfectiveGerund sw
    let way2 sw3 = do
            sw4 <- continueAnyway $ tryRemove reflexive sw3
            sw5 <- tryRemoveAdjectival sw4
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