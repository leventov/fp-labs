import Control.Applicative ((<*), (<$), (<$>))
import Control.Monad ((<=<))

import Data.List (intercalate)
import Data.Char (isSpace)
import Numeric (readHex, readDec)
import Data.String.Utils (strip)
import Text.ParserCombinators.Parsec hiding (Parser)

type Parser t = CharParser () t

-- Simplified XML parser
-- Mainly according to http://www.w3.org/TR/2006/REC-xml11-20060816/
-- Without declarations and CDATA
-- With empty tags, comments, entities and hexidecimal char replacements,
-- commentless and spaceless modes

enquote c s = [c] ++ s ++ [c]

newtype Attr = Attr {getAttr :: (String, String)}
instance Show Attr where
    show (Attr (n, v))
        | '"' `elem` v = n ++ "=" ++ enquote '\'' v
        | otherwise    = n ++ "=" ++ enquote '"' v

data Node =
    Tag {getName :: String, getAttrs :: [Attr], getChildren :: [Node]} |
    Content {getContent :: String} |
    Comment {getComment :: String}

indented :: Int -> Node -> String
indented i node = (replicate i '\t') ++ case node of
    (Tag n as ch) -> let desc = intercalate " " (n:(map show as))
                     in intercalate "\n" (desc:(map (indented (i + 1)) ch))
    (Content c)   -> c
    (Comment c)   -> "Comment " ++ enquote '"' c

instance Show Node where
    show = indented 0

-- A Tag. Not content or comment! May turn to "tag with declarations, comments"
type Document = Node

charRef :: Parser Char
charRef = do
    char '#'
    (char 'x' >> chR hexDigit readHex) <|> chR digit readDec
    where chR pf rf = do
            digits <- many pf
            let ((d, _):_) = rf digits
            return $ toEnum d


reference p = char '&' >> p <* char ';'
entities = [('\'', "apos"), ('"', "quot"), ('<', "lt"), ('>', "gt"), ('&', "amp")]

xmlReplace :: String -> Parser Char
xmlReplace cs = reference $ (<|>) charRef $ choice $ flip map cs $ \c -> do
    let repl = lookup c entities
    maybe (fail "No such named entity") ((<$) c . try . string) repl

replaced :: String -> Parser Char
replaced rs = noneOf xmlRs <|> xmlReplace xmlRs
    where xmlRs = '<':'&':rs


anyName :: Parser String
anyName = do
    let startChar = letter <|> oneOf ":_"
    sChar <- startChar
    restChars <- many $ startChar <|> digit <|> oneOf "-."
    return $ sChar:restChars

tagName = anyName
attrName = anyName

attrValue :: Parser String
attrValue = quoted '"' <|> quoted '\''
    where quoted q = between (char q) (char q) $ many $ replaced [q]

content :: Parser String
content = many1 $ replaced ""


attr :: Parser Attr
attr = do
    n <- attrName
    spaces >> char '=' >> spaces
    v <- attrValue
    return $ Attr (n, v)


comment :: Parser Node
comment = do
    string "<!--"
    c <- many $ noneOf "-" <|> (try $ anyChar >> noneOf "-")
    string "-->"
    return $ Comment c

tag :: Parser Node
tag = do
    name <- char '<' >> tagName <* spaces
    attrs <- attr `endBy` spaces
    let endTag = string "</" >> string name >> spaces >> char '>'

    children <-
        [] <$ string "/>" <|> -- empty tag
        (char '>' >> manyTill node (try endTag))

    return $ Tag name attrs children

node :: Parser Node
node = try comment <|> try tag <|> Content <$> content


parseXML :: String -> Either ParseError Document
parseXML = parse (spaces >> tag <* spaces <* eof) "(unknown)"


spaceless :: Node -> Node
spaceless (Tag n as ch) =
    let emptyNode (Content c) = all isSpace c
        emptyNode _           = False
    in Tag n as $ map spaceless . filter (not . emptyNode) $ ch
spaceless (Content c)   = Content $ strip c
spaceless (Comment c)   = Comment $ strip c


commentless :: Document -> Document
commentless (Tag n as ch) = Tag n as $ map applyCh $ filter notC ch
    where notC (Comment _) = False
          notC _           = True
          applyCh c@(Content _) = c
          applyCh t@(Tag _ _ _) = commentless t
-- Document should NOT be content or comment


main = do
    xmlS <- getContents
    let unwrap = either show show
    putStrLn . unwrap $ return . commentless . spaceless =<< parseXML xmlS