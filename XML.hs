import Control.Applicative ((<*), (<$), (<$>))

import Text.ParserCombinators.Parsec hiding (Parser)
import Numeric (readHex, readDec)
import Data.Maybe (fromMaybe)

type Parser t = CharParser () t

--quotes = between (char '"') (char '"')

type Attr = (String, String)
data Node = Tag {getName :: String, getAttrs :: [Attr], getChildren :: [Node]} |
            Content {getContent :: String} |
            Comment

type Tree = Node

charRef :: Parser Char
charRef = do
    char '#'
    (char 'x' >> chR hexDigit readHex) <|> chR digit readDec
    where chR pf rf = do
            digits <- many pf
            let ((d, _):_) = rf digits
            return $ toEnum d

reference p = char '&' >> p <* char ';'

replacements = [('\'', "apos"), ('"', "quot"), ('<', "lt"), ('>', "gt")]
replace :: String -> Parser Char
replace cs = noneOf cs <|>
             choice $ map (reference . string) repls
    where repls = fromMaybe [] .sequence . map (`lookup` replacements) $ cs


tagName :: Parser String
tagName = undefined

attr :: Parser Attr
attr = undefined

attrName :: Parser String
attrName = undefined

attrValue :: Parser String
attrValue = undefined

content :: Parser String
content = many $ replace "<&"

comment :: Parser Node
comment = do string "<!--"
             many $ noneOf "-" <|> (anyChar >> noneOf "-")
             string "-->"
             return Comment

tag :: Parser Node
tag = do
    char '<'
    name <- tagName
    spaces
    attrs <- attr `endBy` spaces

    let endTag = char '<' >> string name >> spaces >> string "/>"
    allChildren <- [] <$ string "/>" <|> many node <* endTag
    
    let notComment Comment = False
        notComment _       = True
        children = filter notComment allChildren
    
    return $ Tag name attrs children


node :: Parser Node
node = try comment <|> tag <|> Content <$> content


parseXML :: Parser Tree
parseXML = undefined

