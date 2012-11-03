module Racket.Library where

import Racket.Core

import Control.Monad.State
import Control.Monad.Error
import qualified Data.Map as M
import Control.Applicative ((<*), (<$), (<$>), liftA2)
import Text.Printf

import Data.List (intercalate)

library :: Builtin -> Builtin
library builtin = \args -> do
    eArgs <- evalArgs args
    keepEnv $ builtin eArgs


builtinOf2Args builtin = \args -> do
    (fa:sa:_) <- checkArgs 2 args
    builtin fa sa

checkDef :: Expr -> Computation [String]
checkDef expr = putResult $ case expr of
    (IdentifierExpr i) -> Right [i]
    (ListExpr es)      ->
        if all isIdentifier es && length es >= 1
            then Right $ getIdentifier <$> es
            else defError
    _                  -> defError
    where defError = Left "first arg should be either identifier or list of identifiers"

define = builtinOf2Args $ \defExpr body -> do
    (name:args) <- checkDef defExpr
    env <- getEnv
    let funcExpr = FuncExpr $ Func name args body
        newEnv = M.insert name funcExpr env
    putEnv newEnv
    return Void

lambda = builtinOf2Args $ \defExpr body -> do
    args@(first:rest) <- checkDef defExpr
    return $ FuncExpr $ if null rest
        then VarargFunc "anonymous" first body
        else Func "anonymous" args body

funcListBuiltin :: (Func -> Builtin) -> Builtin
funcListBuiltin builtin = library . builtinOf2Args $ \fe le -> do
    let ms1 = if isFunc fe then []
                else [printf "first arg should be function, %s given" (show fe)]
        ms2 = if isList le then ms1
                else (printf "second arg should be list, %s given" $ show le):ms1
    check (null ms2) (intercalate "\n" ms2)
    builtin (getFunc fe) (getList le)

mapBuiltin = funcListBuiltin mapFunc
    where mapFunc f es = do
            check (all isList es) $
                printf "second arg should be list of lists, %s given" (show $ ListExpr es)
            res <- sequence $ map (apply' f) $ getList <$> es
            return $ ListExpr res

list :: Builtin
list = library $ \es -> return $ ListExpr es

quote :: Builtin
quote es = return . QuotedExpr . ListExpr $ es

-- supports QuotedExpr!!!
listBuiltin :: ([Expr] -> Expr) -> Builtin
listBuiltin builtin = library $ \args -> do
    (le:_) <- checkArgs 1 args
    check (isList le) $ printf "first arg should be list, %s given" (show le)
    let es = getList le
    check (not $ null es) $ printf "function accepts non-empty lists"
    return $ builtin es

simpleArith op = library $ \es -> do
    let el = length es
    check (el >= 2) "function accepts at least 2 args"
    check (all isNumeric es) "function args should be numeric"
    if all isInteger es
        -- аргумент - оператор должен быть над одним конкретным типом
        then let res = foldl1 op $ getNumericValue <$> es
             in return $ IntegerExpr $ round res -- dirty hack

        else let res = foldl1 op $ getNumericValue <$> es
             in return $ DoubleExpr res

builtinOf2Numerics builtin = library $ builtinOf2Args $ \a1 a2 -> do
    let both = [a1, a2]
    check (all isNumeric both) $
        printf "function args should be numeric, %s and %s given" (show a1) (show a2)
    let a1V = getNumericValue a1
        a2V = getNumericValue a2
    builtin a1V a2V

division = builtinOf2Numerics $ \num dem -> do
    check (dem == 0.0) "division by zero"
    return . DoubleExpr $ num / dem

eq :: Builtin
eq = library $ builtinOf2Args $ \e1 e2 -> return $ BoolExpr $ e1 == e2

orderBuiltin op = library $ builtinOf2Args $ \e1 e2 -> do
    let both = [e1, e2]
    check (all isOrderable both) $
        printf "function args should be either numeric, bools or strings, %s and %s given" (show e1) (show e2)
    return $ BoolExpr $ op e1 e2

-- shouldn't evaluate branches greedy !!!
ifBuiltin = \args -> keepEnv $ do
    (p:e1:e2:_) <- checkArgs 3 args
    pV <- eval p
    check (isBool pV) $ printf "first arg should be bool, %s given" (show pV)
    if (getBool pV) then eval e1 else eval e2

range = library $ builtinOf2Args $ \s e -> do
    let both = [s, e]
    check (all isInteger both) $
        printf "function args should be integer, %s and %s given" (show s) (show e)
    return . ListExpr $ IntegerExpr <$> [(getInteger s)..(getInteger e)]

errorBuiltin :: Builtin
errorBuiltin = \es -> do
    (msg:_) <- checkArgs 1 es 
    fail $ show msg

cons = library $ builtinOf2Args $ \e l -> do
    check (isList l) $ printf "second arg should be list, %s given" $ show l
    return . ListExpr $ e:(getList l)