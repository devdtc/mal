
module Printer (
    printStr
)
where

import Types

import qualified Data.Text as T


printStr :: MalValue -> T.Text
printStr (MalList vals) = T.concat [T.pack "(", T.intercalate (T.pack " ") (map printStr vals), T.pack ")"]
printStr (MalSym sym) = sym
printStr (MalInt int) = T.pack $ show int
printStr (MalStr str) = T.concat [T.pack "\"", str, T.pack "\""]

