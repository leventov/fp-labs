import Parser
import Expression

import Control.Monad.State
import Control.Monad.Error
import qualified Data.Map as M
import Control.Applicative ((<*), (<$), (<$>), liftA2)
import Text.Printf

eval :: Expr -> Computation
eval = undefined


apply :: Func -> [Expr] -> Computation
apply (Func name argNames bodyExpr) args = nonModifying $ do
    let actualL = length args
        expectedL = length argNames
    check (actualL == expectedL) $
        printf "Function %s accepts %d args, %d given" name expectedL actualL
    env <- getEnv
    putEnv $ foldl (\e (k, v) -> M.insert k v e) env $ zip argNames args
    eval bodyExpr

apply (VarargFunc name argName bodyExpr) args = apply (Func name [argName] bodyExpr) [List args]
apply (BuiltinFunc _ bf) args = bf args

beginBuiltin = BuiltinFunc "begin" $ \es -> nonModifying $ foldl1 (>>) $ eval <$> es

builtin2args :: String -> (Expr -> Expr -> Computation) -> Func
builtin2args name b = BuiltinFunc name $ \es -> do
    let el = length es
    check (el == 2) $ printf "Function %s accepts 2 args, %d given" name el
    b (es !! 0) (es !! 1)

defCheck :: Expr -> Either String [String]
defCheck expr = case expr of
    (Identifier i) -> Right [i]
    (List es)      ->
        if all isIdentifier es
            then Right $ show <$> es
            else defError
    _              -> defError
    where defError = Left "First argument of define should be either identifier or list of identifiers"


defineBuiltin = builtin2args "define" define
    where define defExpr body = do
            (name:args) <- wrap $ defCheck defExpr
            env <- getEnv
            let funcExpr = FuncExpr $ Func name args body
                newEnv = M.insert name funcExpr env
            putEnv newEnv
            return Void

lambdaBuiltin = builtin2args "lambda" lambda
    where lambda defExpr body = do
            args@(first:rest) <- wrap $ defCheck defExpr
            return $ FuncExpr $ if null rest
                then VarargFunc "lambda" first body
                else Func "lambda" args body

