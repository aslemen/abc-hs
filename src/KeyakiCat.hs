{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-|
    Module:     KeyakiCat
    Copyright:  (c) T. N. Hayashi, 2020
    License:    Undetermined

    Provide an representation of categories of the Kainoki Treebank.
    The parser is available at the 'KaiCat.Parser' module.
-}

module KeyakiCat (
    -- * Types
    KeyakiCat(..)
    ) where


import Data.Char (isSpace)
import Data.Text (Text, intercalate, unpack)
import Data.Text.Prettyprint.Doc (Pretty)

import Text.Megaparsec
import Data.Tree.Parser.Penn.Megaparsec.Char (
    UnsafelyParsableAsTerm(..)
    )

{-|
    Record structure that represents simple Keyaki categories.
-}
newtype KeyakiCat = KeyakiCat { unwrapCat :: [Text] }

----------------------------------

instance {-# OVERLAPS #-} UnsafelyParsableAsTerm Text KeyakiCat where
    pUnsafeNonTerm = do
            initCat <- takeWhile1P (Just "Initial Category") checkCharCat
            otherCats <- many $ do 
                single '-'
                takeWhileP (Just "Following Category") checkCharCat
            return $ KeyakiCat $ initCat:otherCats
        where
            checkCharCat :: Char -> Bool
            checkCharCat c 
                = c /= '(' 
                && c /= ')' 
                && c /= '#'
                && c /= '-'
                && not (isSpace c)
    pUnsafeTerm = undefined

----------------------------------

instance Show KeyakiCat where
    show = unpack . intercalate "-" . unwrapCat
    
instance Pretty KeyakiCat
-- Automatically defined via 'show'.
