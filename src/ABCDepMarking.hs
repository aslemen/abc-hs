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

data DepMarking 
    = Head | Adjunct | Complement | AdjunctControl | None
    deriving (Eq)

instance Show DepMarking where
    show Head = "h"
    show Adjunct = "a"
    show Complement = "c"
    show AdjunctControl = "ac"
    show None = "NA"

instance Pretty DepMarking
-- Automatically defined via 'show'.

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