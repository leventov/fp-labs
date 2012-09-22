import Data.List
import Data.Maybe
import Data.Char

import Data.Function
import Data.Functor
import Control.Monad
import Control.Monad.Instances

import Text.Printf

import Control.Exception (tryJust, ArithException)
import System.IO.Unsafe (unsafePerformIO)


both = on (&&)
both' a b p = both p a b

(.||.) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(.||.) = liftM2 (||)

(.&&.) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(.&&.) = liftM2 (&&)

isNumeric = isDigit .||. (== '.')
acceptable = isSpace .||. isNumeric .||. isLetter .||. (`elem` ",()^*/+-")

processChar [] c = [[c]]
processChar ts@(lt@(tt:_):restTs) c
    | unary || sameNeededType c tt = (lt ++ [c]):restTs
    | otherwise                    = [c]:ts
    where sameNeededType a b =
              let eitherChars = (||) `on` (both' a b)
              in eitherChars isNumeric isLetter
          unary = not (null restTs) && head restTs == "("
                  && tt == '-' && isNumeric c

type Error = String

tokenize :: String -> Either Error [String]
tokenize s =
    case findIndex (not . acceptable) s of
        Nothing -> let tokenizeClear = reverse . foldl processChar []
                   in Right $ concat . map tokenizeClear . words $ s
        Just i  -> Left $ printf "Systax error: illegal symbol %c \
                                  \at position %d." (s !! i) i


shuntError = Left "Syntax error: some problem with parentheses, \
                   \or functions args, or operator order."

data Yard = Yard [String] [String] deriving (Show)
getOps (Yard _ ops) = ops
getResult (Yard r _) = r

pushRes t (Yard result ops) = Right $ Yard (t:result) ops
pushOp op (Yard result ops) = Right $ Yard result (op:ops)

popOp (Yard _ []) = shuntError
popOp (Yard result (op:ops)) = Right $ Yard result ops

forwardOp (Yard _ []) = shuntError
forwardOp (Yard result (op:ops)) = Right $ Yard (op:result) ops

forward p y@(Yard r ops) = Yard (foldl (flip (:)) r toMove) restOps
    where (toMove, restOps) = break (not . p) ops

data Assoc = LeftA | RightA deriving (Eq)

operations = [
    ("^", (10, RightA)),
    ("*", (8, LeftA)),
    ("/", (8, LeftA)),
    ("+", (6, LeftA)),
    ("-", (6, LeftA))]

getOpInfo op = fromMaybe (error "There is a bug!") $ lookup op operations

isOp [c] = c `elem` "^*/+-"
isOp _   = False

isFun = all isLetter
isNum = all isNumeric .||.
    (((> 1) . length) .&&. ((== '-') . head) .&&. (all isNumeric . tail))

getProirity t | isOp t  = fst $ getOpInfo t
              | isFun t = 12

processToken y t
    | isFun t || t == "(" = pushOp t y
    | isOp t   = let (priority, assoc) = getOpInfo t
                     lp = if assoc == LeftA then (<=) else (<)
                     weaker = lp priority . getProirity
                     y' = forward ((isOp .||. isFun) .&&. weaker) y
                 in pushOp t y'
    | isNum t  = pushRes t y
    | t == ")" = let isFunOnTop = (not . null) .&&. (isFun . head)
                     forwardFun x = if isFunOnTop $ getOps x
                                        then forwardOp x else Right x
                 in forwardFun <=< popOp $ forward (/= "(") y

shunt = forwardRest <=< (foldM processToken (Yard [] []))
    where forwardRest y
            | all (/= "(") $ getOps y =
                Right $ reverse . getResult . forward (const True) $ y
            | otherwise = Left "Syntax error: there is unclosed opening parenthesis"


safeArith comp = catchArith $ return $! comp
    where showArithE =
            (\e -> "Arithmetic Error: " ++ show e) :: ArithException -> String
          catchArith = unsafePerformIO . tryJust (\e -> Just $ showArithE e)

apply st f = (safeArith =<< fst <$> c) .:. (snd <$> c)
    where (.:.) = liftM2 (:)
          comp fs "pi"        = Right $ (pi, fs)
          comp fs "e"         = Right $ (exp 1, fs)

          comp (x:fs) "sqrt"  = Right $ (sqrt x, fs)
          comp (x:fs) "sin"   = Right $ (sin x, fs)
          comp (x:fs) "cos"   = Right $ (cos x, fs)
          comp (x:fs) "tan"   = Right $ (tan x, fs)
          comp (x:y:fs) "log" = Right $ (logBase y x, fs)

          comp (x:y:fs) "+"   = Right $ (x + y, fs)
          comp (x:y:fs) "*"   = Right $ (x * y, fs)
          comp (x:y:fs) "-"   = Right $ (y - x, fs)
          comp (x:y:fs) "/"   = Right $ (y / x, fs)
          comp (x:y:fs) "^"   = Right $ (y ** x, fs)

          comp _ f = Left $ "Syntax error: unknown function(op) or \
                             \not enough arguments: " ++ f
          c = comp st f

maybeRead = fmap fst . listToMaybe . reads

processSymbol :: [Double] -> String -> Either Error [Double]
processSymbol st s
    | isNum s   = case maybeRead s of
        Just value -> Right (value:st)
        Nothing    -> Left $ printf "Syntax error: can't parse %s \
                                     \as a number" s
    | otherwise = apply st $ s 


evaluate = unbox . (output <=< foldM processSymbol [] <=< shunt <=< tokenize)
    where output [x] = Right $ show x
          output []  = Left "Nothing to calculate"
          output _   = Left "Syntax error: too much numbers"
          unbox = either id id


greeting = "Hello, I am calculator. These operations are supported:\n\
            \+, -, *, /, ^. Association and priorities are standard.\n\
            \Functions: 0ry - pi, e, unary - sqrt, sin, cos, tan, \n\
            \binary - log base arg. Comma-free style: sin 1 * log 2 10.\n\
            \All calculations are performed with floating point.\n\
            \Ctrl-C (within GHCI) or Ctrl-D (within Bash) to exit.\n"

main = interact $ (greeting ++) . unlines . map evaluate . lines
