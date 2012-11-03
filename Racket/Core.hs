module Racket.Core where

import Data.Data

import Control.Monad.State
import Control.Monad.Error
import Control.Monad.Identity
import Text.Printf
import Control.Applicative ((<*), (<$), (<$>), liftA2)

import Data.List (intercalate)
import Debug.Trace (trace)

import qualified Data.Map as M

type Env = M.Map String Expr
type ErrorMessage = String
type ErrorState s a = ErrorT ErrorMessage (State s) a
type Computation a = ErrorState Env a
type Evaluation = Computation Expr

run :: Computation a -> Env -> (Either ErrorMessage a, Env)
run comp env = runIdentity $ runStateT (runErrorT comp) env

putResult :: Either ErrorMessage a -> Computation a
putResult = either fail (lift . return)

getEnv :: Computation Env
getEnv = lift get

putEnv :: Env -> Computation ()
putEnv = lift . put

keepEnv :: Computation a -> Computation a
keepEnv comp = do
    env <- getEnv
    let (res, _) = run comp env
    putResult res

envLookup :: String -> Evaluation
envLookup = \k -> do
    env <- getEnv
    let failWithMsg = fail $
            printf "No function bound to the name %s, current env:\n%s" k $ show $ M.keys env
    maybe failWithMsg return $ M.lookup k env

check :: Bool -> String -> Computation ()
check p errorMsg = if p then return () else fail errorMsg


data Expr =
    QuotedExpr {getExpr :: Expr} |
    ListExpr [Expr] |
    IdentifierExpr {getIdentifier :: String} |
    FuncExpr {getFunc :: Func} |
    IntegerExpr {getInteger :: Integer} |
    DoubleExpr {getDouble :: Double} |
    StringExpr String |
    BoolExpr {getBool :: Bool} |
    Void

getList (ListExpr es) = es
getList (QuotedExpr (ListExpr es)) = QuotedExpr <$> es

getNumericValue :: Expr -> Double
getNumericValue (IntegerExpr i) = realToFrac i
getNumericValue (DoubleExpr d) = realToFrac d

exprOrd (QuotedExpr e) = exprOrd e
exprOrd (ListExpr _) = 1
exprOrd (IdentifierExpr _) = 2
exprOrd (FuncExpr _) = 3
exprOrd (IntegerExpr _) = 4
exprOrd (DoubleExpr _) = 5
exprOrd (StringExpr _) = 6
exprOrd (BoolExpr _) = 7
exprOrd Void = 8

isList = (== 1) . exprOrd
isIdentifier = (== 2) . exprOrd
isFunc = (== 3) . exprOrd
isInteger = (== 4) . exprOrd
isDouble = (== 5) . exprOrd
isBool = (== 7) . exprOrd

isQuotable = (<= 2) . exprOrd
isNumeric = (`elem` [4,5]) . exprOrd
isOrderable = (`elem` [4..7]) . exprOrd

allOfOneType es = all (== exprOrd (head es)) $ exprOrd <$> es

instance Show Expr where
    show (QuotedExpr e)
        | isQuotable e      = "'" ++ show e
        | otherwise         = show e
    show (ListExpr es)      = "(" ++ (intercalate " " $ map show es) ++ ")"
    show (IdentifierExpr i) = i
    show (FuncExpr f)       = "#<function:" ++ (getName f) ++ ">"
    show (IntegerExpr i)    = show i
    show (DoubleExpr d)     = show d
    show (StringExpr s)     = show s
    show (BoolExpr v)       = if v then "#t" else "#f"
    show (Void)             = "#<void>"

instance Eq Expr where
    (QuotedExpr e1) == (QuotedExpr e2) = e1 == e2
    (ListExpr es1) == (ListExpr es2) = es1 == es2
    (IdentifierExpr i1) == (IdentifierExpr i2) = i1 == i2
    (FuncExpr f1) == (FuncExpr f2) = getName f1 == getName f2
    (StringExpr s1) == (StringExpr s2) = s1 == s2
    (BoolExpr b1) == (BoolExpr b2) = b1 == b2
    Void == Void = True
    e1 == e2 | all isNumeric [e1, e2] = getNumericValue e1 == getNumericValue e2
    _ == _ = False

instance Ord Expr where
    (BoolExpr b1) <= (BoolExpr b2) = b1 <= b2
    (StringExpr s1) <= (StringExpr s2) = s1 <= s2
    e1 <= e2 | all isNumeric [e1, e2] = getNumericValue e1 <= getNumericValue e2


type Builtin = [Expr] -> Evaluation
data Func =
    Func {getName :: String, getArgs :: [String], getBody :: Expr} |
    VarargFunc {getName :: String, getArg :: String, getBody :: Expr} |
    BuiltinFunc {getName :: String, getBuiltin :: Builtin}

printEnv :: Builtin
printEnv = \args -> do
    checkArgs 0 args
    env <- getEnv
    return . ListExpr $ IdentifierExpr <$> M.keys env

checkArgs c args =
    let al = length args
    in if al == c then return args else fail $ printf "function accepts %d args, %d given: %s" c al (intercalate ", " $ show <$> args)

instance Show Func where
    show = getName

class Show f => Function f where
    apply :: f -> [Expr] -> Evaluation
    apply' :: f -> [Expr] -> Evaluation
    apply' f args = (apply f args) `catchError` insertName
        where insertName msg = fail $ printf "Error while applying function %s:\n%s" (show f) msg

eval :: Expr -> Evaluation
eval (ListExpr (fn:args)) = do {f <- eval fn; apply' f args;}
eval (ListExpr empty)     = return $ ListExpr []
eval (IdentifierExpr i)   = envLookup i
eval simpleExpr           = return simpleExpr

evalArgs :: [Expr] -> Computation [Expr]
evalArgs = sequence . map eval



instance Function Func where  
    apply (Func _ argNames bodyExpr) args = keepEnv $ do
        checkArgs (length argNames) args 
        env <- getEnv
        eArgs <- evalArgs args
        --let ids = filter isFunc args
        --    values = map ((\i -> show i ++ " = " ++ (show $ M.lookup i env)) . getIdentifier) ids
        --    tr b | trace (show env) False = undefined
        --    tr b = show b ++ "\n"
        --putResult $! Right $! tr values
        
        putEnv $ foldl (\e (k, v) -> M.insert k v e) env $ zip argNames eArgs
        eval bodyExpr

    apply (VarargFunc name argName bodyExpr) args = apply (Func name [argName] bodyExpr) [ListExpr args]
    apply (BuiltinFunc _ bf) args = bf args

instance Function Expr where
    apply (FuncExpr f) args = apply f args
    apply e args = checkArgs 0 args >> return e
