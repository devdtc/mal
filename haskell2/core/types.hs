
module Types (
    MalFunc,
    MalValue(..)
)
where

import qualified Data.Text as T
import Data.List

type MalFunc = MalValue -> MalValue

data MalValue
    = MalList [MalValue]
    | MalSym T.Text
    | MalInt Int
    | MalStr T.Text
    | MalFunc MalFunc

instance Show MalValue where
    show (MalList vals) = concat ["(", intercalate " " (map show vals), ")"]
    show (MalSym sym) = T.unpack sym
    show (MalInt int) = show int
    show (MalStr str) = concat ["\"", T.unpack str, "\""]
    show (MalFunc _) = "[FUNCTION]"

