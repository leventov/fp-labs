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

-- stemming: exact implementation of
-- http://snowball.tartarus.org/algorithms/russian/stemmer.html
isVowel c = c `elem` "аеиоуыэюя"

data StemWord = StemWord {reversedString :: String,
                          rvBound :: Int, r2Bound :: Int} deriving (Show)

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

-- "Right" means fail with current remove rule, to continue with the next.
-- "Left" means success with removing, => pass the rest of rules. 
type RemoveTry = Either StemWord StemWord

unwrap :: RemoveTry -> StemWord
unwrap = either id id

continueAnyway :: RemoveTry -> RemoveTry
continueAnyway tr = Right $ unwrap tr

-- v. v. of default RemoveTry semantics
continueIfRemoved :: RemoveTry -> RemoveTry
continueIfRemoved (Left sw) = Right sw
continueIfRemoved (Right sw) = Left sw


type SelectBound = (StemWord -> Int)

data Endings = Endings {endings :: [String], preceded :: [Char],
                        bound :: SelectBound}

-- kind :: [Endings]
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
-- / kinds

tryRemove :: [Endings] -> StemWord -> RemoveTry
tryRemove es = foldl1 (>=>) $ map tryRemove' es

tryRemove' :: Endings -> StemWord -> RemoveTry
tryRemove' (Endings es ps selectBound) sWord =
    foldM tryRemoveEnding sWord rEndings
    where
        rEndings = map reverse es
        withP = length ps /= 0
        endingBoundL e = length e + if withP then 0 else 1

        tryRemoveEnding sw@(StemWord rString rv r2) rEnding =

            if endingInBound && endingIsSuffix && properlyPreceded
                then Left reminder
                else Right sw
            where
                ebl = endingBoundL rEnding
                endingInBound = ebl <= selectBound sw

                endingIsSuffix = isPrefixOf rEnding rString

                el = length rEnding
                properlyPreceded = not withP || rString !! el `elem` ps
                
                reminder = StemWord (drop el rString) (rv - el) (r2 - el)
         

tryRemoveAdjectival sw = do
    s1 <- continueAnyway $ tryRemove adjective sw
    tryRemove participle s1

-- step* :: StemWord -> RemoveTry
step1 = tryRemove perfectiveGerund >=> way2
    where way2 sw = do
            s1 <- continueAnyway $ tryRemove reflexive sw
            tryRemoveAdjectival >=> tryRemove verb >=> tryRemove noun $ s1

step2 = tryRemove i

step3 = tryRemove derivational

step4 = tryRemove doubleN >=> way2 >=> tryRemove softSign
    where way2 sw = do
            s1 <- continueIfRemoved $ tryRemove superlative sw
            tryRemove doubleN s1


stem :: String -> String
stem = fromStemWord . steps . toStemWord
    where steps = foldr1 (.) $ map (unwrap .) [step4, step3, step2, step1]
    -- the TRUE way would be
    -- steps = unwrap $ foldl1 (>=>)
    --                $ map (continueAnyway .) [step1, step2, step3, step4]

-- / stemming

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

-- P. S. this program processes War and Peace text in ~2 minutes