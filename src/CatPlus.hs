{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

{-|
    Module:     CatPlus
    Copyright:  (c) T. N. Hayashi, 2020
    License:    Undetermined

    Provide an representation of categories of the Kainoki Treebank.
    The parser is available at the 'KaiCat.Parser' module.
-}

module CatPlus (
    -- * Types
    CatPlus(.., (:#:), (:#||:))
    -- * Constructors
    , newNonTerm
    -- * Viewers
    , getCat
    ) where

import qualified Data.List as DList

import Data.Char (isSpace, isNumber, isAlpha)

import Data.Text (Text)
import qualified Data.Text as DText
import qualified Data.Text.Read as DTR

import Data.Map.Strict (Map, foldMapWithKey)
import qualified Data.Map.Strict as DMap

import Data.Text.Prettyprint.Doc (Doc, Pretty(..), layoutCompact, comma)
import Data.Text.Prettyprint.Doc.Render.String (renderString)

import Text.Megaparsec
import Text.Megaparsec.Char.Lexer (decimal)
import Data.Tree.Parser.Penn.Megaparsec.Char (
    UnsafelyParsableAsTerm(..)
    )

import ABCDepMarking

{-|
    Record structure that represents Keyaki categories
        which augmented with our own additional attributes.
-}
data CatPlus cat =
    Term { word :: Text }
    | NonTerm { 
        cat :: cat
        , index :: Maybe Int
        , role :: DepMarking
        , deriv :: Text
        , scope :: [Int]
        , covertArgs :: [(Int, cat)]
        , attrs :: Map Text Text
        }
    deriving (Eq)

getCat :: CatPlus cat -> (Maybe cat, cat -> CatPlus cat)
getCat t@(Term w) 
    = (Nothing, const t)
getCat nt@(NonTerm { cat = cs }) 
    = (Just cs, \cnew -> nt { cat = cnew })

pattern cs :#: attr <- (getCat -> (Just cs, attr))
    where
        cs :#: attr = attr cs
infix 9 :#:

getCatRule :: CatPlus cat 
    -> (Maybe (cat, DepMarking), cat -> DepMarking -> CatPlus cat)
getCatRule t@(Term w)
    = (Nothing, const $ const t)
getCatRule nt@(NonTerm { cat = cs, role = rs })
    = (Just (cs, rs), \cnew rnew -> nt { cat = cnew, role = rnew })

pattern cr :#||: attr <- (getCatRule -> (Just cr, attr))
    where 
        (c, r) :#||: attr = attr c r

infix 9 :#||:

{-|
    Smart constructor that generates 
        a non-terminal categorial representation without attributes, 
        given a Keyaki category.
-}
newNonTerm :: cat -> CatPlus cat
{-# INLINE newNonTerm #-}
newNonTerm cs = NonTerm { 
    cat = cs 
    , index = Nothing
    , role = None
    , deriv = ""
    , scope = []
    , covertArgs = []
    , attrs = DMap.empty 
    }

instance Functor CatPlus where
    fmap _ (Term w) = Term w
    fmap f NonTerm {
        cat = cs 
        , index = idx
        , role = r
        , deriv = d
        , scope = s
        , covertArgs = cA
        , attrs = as
    } = NonTerm {
        cat = f cs
        , index = idx
        , role = r
        , deriv = d
        , scope = s
        , covertArgs = fmap (\(i, c2) -> (i, f c2)) cA 
        , attrs = as
    }
----------------------------------

instance {-# OVERLAPS #-} 
    forall cat. (UnsafelyParsableAsTerm Text cat) 
    => UnsafelyParsableAsTerm Text (CatPlus cat) where
    pUnsafeNonTerm = do
            cat <- pUnsafeNonTerm
            pManyAttrVal $ newNonTerm cat 
        where
            pNumCat :: (Ord e) 
                => ParsecT e Text m (Int, cat)
            pNumCat = do
                index <- decimal
                single '^'
                cat <- pUnsafeNonTerm
                return (index, cat)
            pAttrVal :: CatPlus cat -> ParsecT _ Text m (CatPlus cat)
            pAttrVal kc = do
                single '#'
                attr <- takeWhile1P (Just "Attribute") $ \c -> 
                            c /= '('
                            && c /= ')'
                            && c /= '#'
                            && c /= '='
                            && not (isSpace c) 
                single '='
                case attr of
                    "index" -> do 
                        i <- decimal
                        return kc { index = Just i } 
                    "role" -> do 
                        r <- parseDepMarking
                        return kc { role = r }
                    "deriv" -> do
                        dv <- takeWhile1P 
                                (Just "Value of Attribute: Derivation Rule")
                                checkCharValue 
                        return $ kc { deriv = dv }
                    "scope" -> do 
                        res <- decimal `sepBy1` single ','
                        return $ kc {
                            scope = res
                        }
                    "covertArgs" -> do
                        res <- pNumCat `sepBy1` single ','
                        return $ kc {
                            covertArgs = res
                        }
                    otherAttr -> do
                        value <- takeWhile1P 
                            (Just "Value of Some Attribute")
                            checkCharValue
                        return $ kc {
                            attrs = DMap.insert otherAttr value (attrs kc) 
                            }
                    where
                        checkCharValue :: Char -> Bool
                        checkCharValue c 
                            = c /= '('
                            && c /= ')'
                            && c /= '#'
                            && not (isSpace c)
            pManyAttrVal :: CatPlus cat -> ParsecT _ Text m (CatPlus cat)
            pManyAttrVal kc = do
                kcModRaw <- optional $ pAttrVal kc
                case kcModRaw of 
                    Nothing -> return kc
                    Just kcMod -> pManyAttrVal kcMod
    pUnsafeTerm = do
        word <- takeWhile1P (Just "Terminal Label") $ \c -> 
                    c /= '(' 
                    && c /= ')' 
                    && not (isSpace c)
        return $ Term word

----------------------------------

instance (Pretty cat) => Pretty (CatPlus cat) where
    pretty (Term word) = pretty word
    pretty NonTerm {
            cat = c
            , index = idx
            , role = r
            , deriv = d
            , scope = s
            , covertArgs = cA
            , attrs = as
        } = pretty c
        <> (
            case idx of
                Just i -> "#index=" <> pretty i
                Nothing -> mempty
        ) <> (if r == None then mempty else "#role=" <> pretty r)
        <> pretty (if d == "" then "" else "#deriv=" <> d)
        <> (
            if s == [] then mempty
            else "#scope="
                <> (mconcat $ DList.intersperse comma (pretty <$> s))
        ) <> (
            case cA of
                [] -> mempty
                _  -> "#covertArgs=" 
                        <> (mconcat $ DList.intersperse comma (makeArg <$> cA))
        ) 
        <> (pretty $ foldMapWithKey makeAttrVal as)
        where 
            makeArg :: (Pretty cat) => (Int, cat) -> Doc a
            makeArg (i, c) = pretty i <> "^" <> pretty c <> ","
            makeAttrVal :: Text -> Text -> Text
            makeAttrVal attr val = "#" <> attr <> "=" <> val

instance (Show cat) => Show (CatPlus cat) where
    show = renderString . layoutCompact . pretty . fmap show 