import Text.ParserCombinators.Parsec hiding (Parser)
import Control.Applicative hiding (many, (<|>))

type Parser t = CharParser () t


eol :: Parser String
-- RWH :)
eol =   try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"
    <|> fail "Couldn't find EOL"


eols = "\r\n"

quotedValue :: Char -> Parser String
quotedValue delim = many $ doubleQuote <|> nonQuoteAcceptable
    where doubleQuote = try $ '"' <$ string "\"\""
          nonQuoteAcceptable = noneOf $ delim:'\"':eols


value :: Char -> Parser String
value delim = quotedValue delim <|> many acceptable
    where acceptable = noneOf $ delim:eols


line :: Char -> Parser [String]
line delim = sepBy (value delim) (char delim)


csv :: Char -> Parser [[String]]
csv delim = endBy (line delim) eol


parseCSV :: Char -> String -> Either ParseError [[String]]
parseCSV delim = parse (csv delim) ""