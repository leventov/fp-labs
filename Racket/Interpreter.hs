import Racket.Parser
import Racket.Core
import Racket.Library

import qualified Data.Map as M
import Control.Monad.Error (catchError)
import Data.List (intercalate)
import Control.Applicative ((<*), (<$), (<$>), liftA2)
import Debug.Trace (trace)

import System.IO (BufferMode (NoBuffering), stdin, stdout, hSetBuffering)

builtins = let bi n b = (n, FuncExpr $ BuiltinFunc n b) in M.fromList [
    bi "define" define,
    bi "lambda" lambda,
    bi "apply" (funcListBuiltin apply'),
    bi "map" mapBuiltin,
    bi "list" list,
    bi "quote" quote,
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
    bi "if" ifBuiltin,
    bi "range" range,
    bi "error" errorBuiltin,
    bi "cons" cons,
    bi "env" printEnv]

greeting = "Racket interpreter.\n" --  Type `(env)` to print available functions.\n"

runExpr :: Expr -> Evaluation
runExpr e = (eval e) `catchError` (return . IdentifierExpr)


mainLoop env es = 
    let comp = sequence $ runExpr <$> es
        showRight rs = intercalate "\n" $ show <$> rs
    in either show showRight $ fst $ run comp env

setupBuffering = do
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering

main = setupBuffering >> putStr greeting >> (interact $ \s -> (either show (mainLoop builtins) $ parseExprs s) ++ "\n")
