module Parser (Expr, parseExprs) where
    
import Expression

import Control.Applicative ((<*), (<$), (<$>), liftA2)
import Text.ParserCombinators.Parsec hiding (Parser)
import qualified Text.ParserCombinators.Parsec.Token as P

type Parser t = CharParser () t



identifierLetter :: Parser Char
identifierLetter = noneOf "()[]{}\",'`;#|\\"

racketLanguage = P.LanguageDef
    { P.commentStart = "@;{"
    , P.commentEnd = "}"
    , P.commentLine = "@;"
    , P.nestedComments = True
    , P.identStart = identifierLetter
    , P.identLetter = identifierLetter
    , P.opStart = identifierLetter
    , P.opLetter = identifierLetter
    , P.reservedOpNames = []
    , P.reservedNames = []
    , P.caseSensitive = True
    }

lexer = P.makeTokenParser racketLanguage

whiteSpace = P.whiteSpace lexer

braces = P.braces lexer <||> P.brackets lexer
    where (<||>) = liftA2 (<|>)

listExpr :: Parser Expr
listExpr = char '\'' >> List <$> (braces $ expr `endBy` whiteSpace)

identifier = P.identifier lexer
identifierExpr = Identifier <$> identifier

integer = P.integer lexer
integerExpr = IntegerExpr <$> integer

double = P.float lexer
doubleExpr = DoubleExpr <$> double

stringLiteral = P.stringLiteral lexer
stringExpr = StringExpr <$> stringLiteral

boolExpr :: Parser Expr
boolExpr = char '#' >> BoolExpr <$> (== 't') <$> oneOf "tf"

voidExpr :: Parser Expr
voidExpr = string "#<void>" >> return Void

simpleExpr :: Parser Expr
simpleExpr = choice $ map try $
    [listExpr, identifierExpr, doubleExpr, integerExpr, stringExpr, boolExpr, voidExpr]

functionExpr :: Parser Expr
functionExpr = braces $ do
    i <- whiteSpace >> identifierExpr <* whiteSpace
    args <- expr `endBy` whiteSpace
    return $ List $ i:args

expr :: Parser Expr
expr = simpleExpr <|> functionExpr

type Program = [Expr]

parseExprs :: String -> Either ParseError Program
parseExprs = parse (whiteSpace >> (expr `endBy` whiteSpace)) "(unknown)"
