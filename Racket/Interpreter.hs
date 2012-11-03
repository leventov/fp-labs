import Racket.Parser
import Racket.Core
import Racket.Library

import Control.Applicative ((<$>))
import Data.List (intercalate)

import Control.Monad.Error (catchError)

import qualified Data.Map as M

builtins = let bi n b = (n, FuncExpr $ BuiltinFunc n b) in M.fromList [
    bi "env" printEnv,

    bi "error" errorBuiltin,
    bi "define" define,
    bi "if" ifBuiltin,

    bi "lambda" lambda,
    bi "begin" begin,
    bi "apply" (funcListBuiltin apply'),
    bi "map" mapBuiltin,
    bi "list" list,
    bi "quote" quote,

    bi "range" range,
    bi "cons" cons,
    bi "head" (listBuiltin head),
    bi "tail" (listBuiltin ListExpr . tail),
    bi "init" (listBuiltin ListExpr . init),
    bi "last" (listBuiltin last),
    
    bi "+" (simpleArith (+)),
    bi "*" (simpleArith (*)),
    bi "-" (simpleArith (-)),
    bi "/" division,
    
    bi "==" eq,
    bi "<" (orderBuiltin (<)),
    bi "<=" (orderBuiltin (<=)),
    bi ">" (orderBuiltin (>)),
    bi ">=" (orderBuiltin (>=)),
    
    bi "and" (boolBuiltin and),
    bi "or" (boolBuiltin or)]

runExpr :: Expr -> Evaluation
runExpr e = (eval e) `catchError` (return . IdentifierExpr)

mainLoop env es = 
    let comp = sequence $ runExpr <$> es
        showRight rs = intercalate "\n" $ show <$> rs
    in either show showRight $ fst $ run comp env

main = do
    putStr "Racket interpreter. Type `(env)` to print available functions.\n"
    interact $ \s -> (either show (mainLoop builtins) $ parseExprs s) ++ "\n"
