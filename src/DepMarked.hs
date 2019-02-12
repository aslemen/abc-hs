module DepMarked (
    DepMarked(..),
    markCat,
    parser,
    ) where

import Control.Applicative

import qualified Text.Parsec as Psc
import Text.Parsec.String (Parser)

import qualified DepMarking as DMing

data DepMarked cat
    = (:|) {
        category:: cat,
        dependency :: DMing.DepMarking
    } deriving (Eq)

showFull :: (Show cat) => DepMarked cat -> String
showFull (cat :| dep)
    = (show cat) ++ "|" ++ (show dep)

showLess :: (Show cat) => DepMarked cat -> String
showLess (cat :| DMing.None) = show cat
showLess dm@(_ :| _) = showFull dm
        
instance (Show cat) => Show (DepMarked cat) where
    show = showFull

instance Functor DepMarked where
    -- fmap :: (a -> b) -> (DepMarked a) -> (DepMarked b)
    fmap f (cat :| dep) = (f cat) :| dep

instance Applicative DepMarked where
    -- pure :: a -> DepMarked a
    pure x = x :| DMing.None
    -- <*> :: DepMarked (a -> b) -> (DepMarked a) -> (DepMarked b)
    (<*>) (df :| _) (cat :| dep2)
        = (df cat) :| dep2
            -- ignoring dep1 

markCat :: String -> cat -> (DepMarked cat)
markCat str x
    = x :| (DMing.createMarking str)

parser :: Parser cat -> Parser (DepMarked cat)
parser parserCat
    = parserCat -- P Cat
        >>= \cat -> -- Cat
            parserMarkingInternal -- P Str -> P (cat -> DepMarked)
            >>= \markfunc -> -- cat -> DepMarked
                return (markfunc cat)
    where
        parserMarkingInternal :: Parser (c -> DepMarked c)
        parserMarkingInternal
            = markCat 
                <$> Psc.option "" 
                    (Psc.string "|" *> (Psc.many1 Psc.letter))
