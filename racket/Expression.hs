module Expression where

import Control.Monad.State
import Control.Monad.Error
import Control.Monad.Identity

import Data.List (intercalate)

import qualified Data.Map as M

type Env = M.Map String Expr

type ErrorState s a = ErrorT String (State s) a

type Computation = ErrorState Env Expr

runComputation comp env = runIdentity $ runStateT (runErrorT comp) env

data Func =
    Func {getName :: String, getArgs :: [String], getBody :: Expr} |
    VarargFunc {getName :: String, getArg :: String, getBody :: Expr} |
    BuiltinFunc {getName :: String, getBuiltinFunc :: [Expr] -> Computation}

getEnv :: ErrorState Env Env
getEnv = lift get

putEnv :: Env -> ErrorState Env ()
putEnv = lift . put

wrap :: Either String a -> ErrorState Env a
wrap = either fail (lift . return)

nonModifying :: Computation -> Computation
nonModifying comp = do
    env <- getEnv
    let (res, _) = runComputation comp env
    wrap res

envLookup :: String -> Computation
envLookup = \k -> do
    env <- getEnv
    let failWithMsg = fail $ "No function bound to the name " ++ k
    maybe failWithMsg return $ M.lookup k env


data Expr =
    List [Expr] |
    FuncExpr Func |
    Identifier String |
    IntegerExpr Integer |
    DoubleExpr Double |
    StringExpr String |
    BoolExpr Bool |
    Void

instance Show Expr where
    show (List es)       = "(" ++ (intercalate " " $ map show es) ++ ")"
    show (FuncExpr f)    = "#<function:" ++ (getName f) ++ ">"
    show (Identifier i)  = i
    show (IntegerExpr i) = show i
    show (DoubleExpr d)  = show d
    show (StringExpr s)  = show s
    show (BoolExpr v)    = if v then "#t" else "#f"
    show (Void)          = "#<void>"

isIdentifier (Identifier _) = True
isIdentifier _              = False

check :: Bool -> String -> ErrorState Env ()
check p errorMsg = if p then return () else fail errorMsg

