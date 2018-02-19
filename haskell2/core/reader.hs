
module Reader (
    readStr
)
where

import Types

import Control.Applicative
import Data.Attoparsec.Text
import Data.Text as T


--readStr :: Text -> MalValue
--readStr str =
--    case tokenize str of
--        Left err ->  error err
--        Right result -> result

readStr :: Text -> Either String MalValue
readStr = tokenize

tokenize :: Text -> Either String MalValue
tokenize str =
    case result of
        Left s -> Left s
        right@(Right _) -> right
  where
    delim =
            space
        <|> char ','

    delims = many' delim
    delims1 = many1' delim

    --int = do
    --    (intStr, _) <- match $ pos <|> neg

    --    return $ MalInt $ read $ unpack intStr
    --  where
    --    pos = many1' digit
    --    neg = do
    --        char '-'
    --        pos

    int = fmap (MalInt . fromIntegral) (signed decimal)

    symbol = do
        (symStr, _) <- match $ do
            letter <|> underscore <|> specials
            many' $ letter <|> underscore <|> digit <|> specials

        pure $ MalSym symStr
      where
        underscore = char '_'
        specials = satisfy $ inClass "-+*/"

    strLit = do
        char '"'
        --str <- manyTill' anyChar (notChar '\\' <* char '"')
        --str <- many' $ do
        --    next <- peekChar

        --    satisfy (\ch -> ch == '\\' || next /= (Just '"'))
        str <- many' $ qut <|> nc
        char '"'

        pure $ MalStr $ T.filter (/= '\\') $ T.concat $ str
      where
        qut = (string $ pack "\\\"")
        nc = notChar '"' >>= (\ch -> pure $ singleton ch)
       

    atom =
            int
        <|> symbol
        <|> strLit

    list = do
        char '('
        delims
        atoms <- value `sepBy` delims1
        delims
        char ')'

        pure $ MalList atoms

    value = list <|> atom

    tokenizer :: Parser MalValue
    tokenizer = do
        delims
        val <- value
        delims

        pure val

    result = parseOnly (tokenizer <* endOfInput) str

