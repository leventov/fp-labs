module Racket.Parser (parseExprs) where
    
import Racket.Core

import Control.Applicative ((<$>), liftA2)
import Text.ParserCombinators.Parsec hiding (Parser)
import qualified Text.ParserCombinators.Parsec.Token as P

type Parser t = CharParser () t

identifierLetter :: Parser Char
identifierLetter = noneOf "()[]{}\",'`;#|\\ \n1234567890"

racketLanguage = P.LanguageDef
    { P.commentStart = "/*"
    , P.commentEnd = "*/"
    , P.commentLine = "//"
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

braces = P.parens lexer <||> P.brackets lexer
    where (<||>) = liftA2 (<|>)

lexeme = P.lexeme lexer

quotedExpr :: Parser Expr
quotedExpr = QuotedExpr <$> do {char '\''; expr;}

listExpr :: Parser Expr
listExpr = ListExpr <$> braces exprs

identifier = P.identifier lexer
identifierExpr = IdentifierExpr <$> identifier

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


expr :: Parser Expr
expr = choice $ map try $
    [quotedExpr, listExpr, identifierExpr, doubleExpr, integerExpr, stringExpr,
    boolExpr, voidExpr]

exprs :: Parser [Expr]
exprs = whiteSpace >> (many $ lexeme expr)

type Program = [Expr]

parseExprs :: String -> Either ParseError Program
parseExprs = parse exprs "(unknown)"
