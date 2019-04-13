{-# LANGUAGE OverloadedStrings #-}

module DepMarking (
    DepMarking(..),
    createMarking
    ) where

import qualified Data.Text as DT

import qualified PTPrintable as PTP
import qualified PTDumpable as PTD

data DepMarking 
    = Head | Adjunct | Complement | ComplementSpecial | None
    deriving (Eq)

instance Show DepMarking where
    show Head = "h"
    show Adjunct = "a"
    show Complement = "c"
    show ComplementSpecial = "cs"
    show None = "NA"

instance PTP.Printable DepMarking

instance PTD.Dumpable DepMarking

createMarking :: DT.Text -> DepMarking 
createMarking "h" = Head
createMarking "a" = Adjunct
createMarking "c" = Complement
createMarking "cs" = ComplementSpecial
createMarking "NA" = None
createMarking _ = None