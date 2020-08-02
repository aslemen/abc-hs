{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-|
    Module:     ABCDepMarking
    Copyright:  (c) T. N. Hayashi, 2020
    License:    Undetermined

    Provide an representation of categories of the Kainoki Treebank.
    The parser is available at the 'KaiCat.Parser' module.
-}

module ABCDepMarking (
    DepMarking(..)
    , parseDepMarking
    ) where

import Data.Char (isAlpha)
import Data.Text (Text)
import Data.Text.Prettyprint.Doc (Pretty)
import Text.Megaparsec

{-|
    A data type representing grammatical roles of non-terminal nodes
        in the ABC Treebank.
-}
data DepMarking 
    = Head -- ^ The head grammatical role, marked with @#role=h@
    | Adjunct -- ^ The adjunct, marked with @#role=a@
    | Complement -- ^ The complement, marked with @#role=c@
    -- | The role for control predicates, marked with @#role=ac@
    | AdjunctControl
    | None -- ^ The vacuous role
    deriving (Eq)

instance Show DepMarking where
    show Head = "h"
    show Adjunct = "a"
    show Complement = "c"
    show AdjunctControl = "ac"
    show None = "NA"

instance Pretty DepMarking
-- Automatically defined via 'show'.

{-|
    A "Text.Megaparsec" parser for the values (i.e. the hole in @#role=_@)
        of grammatical role features.
-}
parseDepMarking :: (Ord e) => ParsecT e Text m DepMarking
parseDepMarking = do
    role <- takeWhile1P 
            (Just "Value of Attribute: Role Marking")
            isAlpha
    return $ case role of
        "h" -> Head
        "a" -> Adjunct
        "c" -> Complement
        "ac" -> AdjunctControl
        _ -> None 