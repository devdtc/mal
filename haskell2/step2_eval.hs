
import Types
import Reader
import Printer

import Data.Either
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import System.IO


env :: M.Map T.Text MalFunc
env = M.fromList
  [ (T.pack "+", \(MalList [MalInt x, MalInt y]) -> MalInt (x + y))
  , (T.pack "-", \(MalList [MalInt x, MalInt y]) -> MalInt (x - y))
  , (T.pack "*", \(MalList [MalInt x, MalInt y]) -> MalInt (x * y))
  , (T.pack "/", \(MalList [MalInt x, MalInt y]) -> MalInt (x `div` y))
  ]

flattenEithers :: [Either a b] -> Either (NonEmpty a) [b]
flattenEithers es =
  case partitionEithers es of
    (    [], rs) -> Right rs
    (l : ls, _ ) -> Left $ l :| ls

malRead :: T.Text -> Either String MalValue
malRead = readStr

evalAst :: MalValue -> Either String MalValue
evalAst (MalSym sym) =
  case M.lookup sym env of
    Just val -> Right $ MalFunc val
    Nothing -> Left $ "couldn't find symbol '" ++ (T.unpack sym) ++ "'"
evalAst (MalList vals) =
  case (flattenEithers $ map malEval vals) of
    Left (err :| _) -> Left err
    Right evalVals -> Right $ MalList $ evalVals
evalAst val = Right val

malEval :: MalValue -> Either String MalValue
malEval (MalList []) = Right $ MalList []
malEval l@(MalList vals) =
  case evalAst l of
    Left err -> Left err
    Right (MalList (MalFunc func : args)) -> Right $ func $ MalList args
    wrongVal -> Left $ "expected function, got " ++ show wrongVal
malEval mv = evalAst mv

malPrint :: MalValue -> T.Text
malPrint = printStr

rep :: T.Text -> T.Text
rep str =
  case malRead str of
    Left err -> T.pack err
    Right mv ->
      case malEval mv of
        Left err -> T.pack err
        Right result -> malPrint result

repl :: IO ()
repl = do
  putStr "user> "
  hFlush stdout

  TIO.getLine >>= (TIO.putStrLn . rep)

  repl


main :: IO ()
main = repl

