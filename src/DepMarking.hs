module DepMarking (
    DepMarking(..),
    createMarking
    ) where

data DepMarking 
    = Head | Adjunct | Complement | ComplementSpecial | None
    deriving (Eq)

instance Show DepMarking where
    show Head = "h"
    show Adjunct = "a"
    show Complement = "c"
    show ComplementSpecial = "cs"
    show None = "NA"

createMarking :: String -> DepMarking 
createMarking "h" = Head
createMarking "a" = Adjunct
createMarking "c" = Complement
createMarking "cs" = ComplementSpecial
createMarking "NA" = None
createMarking _ = None