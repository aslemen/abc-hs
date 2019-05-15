{-# LANGUAGE OverloadedStrings #-}

module DepMarking (
    DepMarking(..),
    createMarking
    ) where

import qualified Data.Text as DT

import qualified PTDumpable as PTD

data DepMarking 
    = Head | Adjunct | Complement | AdjunctControl | None
    deriving (Eq)

instance Show DepMarking where
    show Head = "h"
    show Adjunct = "a"
    show Complement = "c"
    show AdjunctControl = "ac"
    show None = "NA"

instance PTD.Dumpable DepMarking

createMarking :: DT.Text -> DepMarking 
createMarking "h" = Head
createMarking "a" = Adjunct
createMarking "c" = Complement
createMarking "ac" = AdjunctControl
createMarking "NA" = None
createMarking _ = None