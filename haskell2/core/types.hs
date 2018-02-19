
module Types (
    MalValue(..)
)
where

import Data.Text


data MalValue
    = MalList [MalValue]
    | MalSym Text
    | MalInt Int
    | MalStr Text
    deriving (Show)

