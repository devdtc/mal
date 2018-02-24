
module Printer (
    printStr
)
where

import Types

import qualified Data.Text as T


printStr :: MalValue -> T.Text
printStr = T.pack . show

