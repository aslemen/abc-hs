module DepMarked (
    DepMarked(..),
    (<-->),
    markCat,
    parser,
    ) where

import Control.Applicative

import qualified Text.Parsec as Psc
import Text.Parsec.String (Parser)

import qualified DepMarking as DMing

data DepMarked cat
    = DepMarked {
        dependency :: DMing.DepMarking,
        category:: cat
    } deriving (Eq)

(<-->) :: cat -> DMing.DepMarking -> DepMarked cat
c <--> dep
    = DepMarked dep c

instance (Show cat) => Show (DepMarked cat) where
    show (DepMarked dep cat)
        = (show cat) ++ "|" ++ (show dep)

instance Functor DepMarked where
    -- fmap :: (a -> b) -> (DepMarked a) -> (DepMarked b)
    fmap f (DepMarked dep cat) = DepMarked dep (f cat)

instance Applicative DepMarked where
    -- pure :: a -> DepMarked a
    pure = DepMarked DMing.None
    -- <*> :: DepMarked (a -> b) -> (DepMarked a) -> (DepMarked b)
    (<*>) (DepMarked dep1 df) (DepMarked dep2 cat)
        = DepMarked dep2 (df cat)
            -- ignoring dep1 

markCat :: String -> cat -> (DepMarked cat)
markCat str
    = DepMarked (DMing.createMarking str)

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
