import Control.Applicative ((<*), (<$))

import Text.ParserCombinators.Parsec

-- according to http://en.wikipedia.org/wiki/Comma-separated_values

eol = try (string "\n\r") <|>
      try (string "\r\n") <|>
      string "\n" <|>
      string "\r" <?> "end of line"

quotes = between (char '"') (char '"')

eols = "\r\n"

quotedValue delim = quotes $ many $ doubleQuote <|> noneOf "\""
    where doubleQuote = try $ '"' <$ string "\"\""

value delim = quotedValue delim <|> many acceptable
    where acceptable = noneOf $ delim:eols

line delim = sepBy (value delim) (char delim)

csv delim False = endBy (line delim) eol
csv delim True = [] <$ eof <|> do
    firstRow <- (line delim) <* eol
    let colCount = length firstRow
        val = value delim
        row = do
            firstValue <- val
            restValues <- count (colCount - 1) $ char delim >> val
            return $ firstValue:restValues

    restRows <- endBy row eol
    -- without this csv consumes only 1st line of non-table input.
    eof -- leads to error
    return $ firstRow:restRows


parseCSV :: Char -> Bool -> String -> Either ParseError [[String]]
parseCSV delim tableMode = parse (csv delim tableMode) ""

simpleCSV = parseCSV ',' False

main = putStrLn . either show show . simpleCSV =<< getContents