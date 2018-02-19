
import System.IO

malRead :: String -> String
malRead = id

malEval :: String -> String
malEval = id

malPrint :: String -> String
malPrint = id

rep :: String -> String
rep = malPrint . malEval . malRead

repl :: IO ()
repl = do
    putStr "user> "
    hFlush stdout

    getLine >>= (putStrLn . rep)

    repl


main :: IO ()
main = repl

