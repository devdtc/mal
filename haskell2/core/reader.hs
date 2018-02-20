
module Reader (
    readStr
)
where

import Types

import Data.Text (Text, pack, unpack)

import Text.Parsec
import Text.Parsec.Error

type CharParser   = Parsec Text () Char
type StringParser = Parsec Text () String
type MalParser    = Parsec Text () MalValue


delim :: CharParser
delim =
        space
    <|> char ','

delims :: StringParser
delims = many delim

delims1 :: StringParser
delims1 = many1 delim

malInt :: MalParser
malInt = do
    intStr <- pos <|> neg

    pure $ MalInt $ read intStr
  where
    pos = many1 digit
    neg = (:) <$> (char '-') <*> pos

malSymbol :: MalParser
malSymbol = do
    symStr <- (:) <$> leadingChar <*> many trailingChar

    pure $ MalSym $ pack symStr
  where
    underscore = char '_'
    specials = oneOf "-+*/"

    leadingChar = letter <|> underscore <|> specials
    trailingChar = leadingChar <|> digit

malString :: MalParser
malString = do
    char '"'
    strs <- many $ fmap pure nonEscape <|> escape
    char '"'

    pure $ MalStr $ pack $ concat $ strs
  where
    escape = do
        e <- char '\\'
        c <- oneOf "\\\""
        pure [e, c]

    nonEscape = noneOf "\\\""

malAtom :: MalParser
malAtom =
        malInt
    <|> malSymbol
    <|> malString

malList :: MalParser
malList = do
    char '('
    delims

    vals <- malValue `sepEndBy` delims -- sepEndBy needed if trailing delims allowed

    -- trailing delims handled by above
    char ')'

    pure $ MalList vals

malValue :: MalParser
malValue = malList <|> malAtom


readStr :: Text -> Either String MalValue
readStr str =
    case result of
        Left err -> Left $ concatMap ((++"\n") . messageString) (errorMessages err)
        Right malVal -> Right malVal
  where
    tokenizer :: MalParser
    tokenizer = do
        delims
        val <- malValue
        delims

        pure val

    result = parse (tokenizer <* eof) "repl" str

