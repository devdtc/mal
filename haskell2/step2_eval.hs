
import Types
import Reader
import Printer

import Data.Either
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import System.IO


leftOrRights :: [Either a b] -> Either a [b]
leftOrRights xs =
  if null ls
    then Right rs
    else Left $ head ls
 where
  (ls, rs) = partitionEithers xs


simpleOp :: [MalValue -> MalResult] -> ([MalValue] -> MalValue) -> MalFunc
simpleOp argChecks op = \args ->
  let
    nargs = length args
    nchecks = length argChecks
  in
    if nargs < nchecks
      then Left "too few arguments provided"
      else if nargs > nchecks
        then Left "too many arguments provided"
        else
          let checked = zipWith ($) argChecks args
          in op <$> leftOrRights checked

expectInt :: MalValue -> MalResult
expectInt int@(MalInt _) = Right int
expectInt wrongVal = Left $ show "expected int, got " ++ show wrongVal

envFuncs :: [(String, [MalValue] -> Either String MalValue)]
envFuncs =
  [ ("+", binaryIntOp $ \x y -> (x + y))
  , ("-", binaryIntOp $ \x y -> (x - y))
  , ("*", binaryIntOp $ \x y -> (x * y))
  , ("/", binaryIntOp $ \x y -> (x `div` y))
  ]
 where
  binaryIntOp op = simpleOp [expectInt, expectInt] $ \[MalInt x, MalInt y] -> MalInt $ op x y

env :: M.Map T.Text MalFunc
env = M.fromList $ map (\(s, f) -> (T.pack s, f)) envFuncs


malRead :: T.Text -> Either String MalValue
malRead = readStr

evalAst :: MalValue -> Either String MalValue
evalAst (MalSym sym) =
  case M.lookup sym env of
    Just symVal -> Right $ MalFunc symVal
    Nothing -> Left $ "couldn't find symbol '" ++ (T.unpack sym) ++ "'"
evalAst (MalList vals) =
  let evalVals = leftOrRights $ map malEval vals
  in MalList <$> evalVals
evalAst val = Right val

malEval :: MalValue -> Either String MalValue
malEval list@(MalList []) = Right list
malEval list@(MalList  _) =
  case evalAst list of
    Left err -> Left err
    Right (MalList (MalFunc func : args)) -> func args
    Right (MalList (wrongVal : _)) -> Left $ "expected function, got " ++ show wrongVal
    Right _ -> error "evalAst returned non-list from list"
malEval mv = evalAst mv

malPrint :: MalValue -> T.Text
malPrint = printStr

rep :: T.Text -> T.Text
rep str = either T.pack malPrint result
 where
  result = malRead str >>= malEval

repl :: IO ()
repl = do
  putStr "user> "
  hFlush stdout

  TIO.getLine >>= (TIO.putStrLn . rep)

  repl


main :: IO ()
main = repl

