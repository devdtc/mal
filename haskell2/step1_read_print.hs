
import Types
import Reader
import Printer

import Data.Text
import qualified Data.Text.IO as TIO

import System.IO


malRead :: Text -> Either String MalValue
malRead = readStr

malEval :: MalValue -> MalValue
malEval = id

malPrint :: MalValue -> Text
malPrint = printStr

rep :: Text -> Text
rep str =
    case malRead str of
        Left err -> pack err
        Right mv -> (malPrint . malEval) mv

repl :: IO ()
repl = do
    putStr "user> "
    hFlush stdout

    TIO.getLine >>= (TIO.putStrLn . rep)

    repl


main :: IO ()
main = repl

