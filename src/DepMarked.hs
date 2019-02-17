module DepMarked (
    DepMarked(..),
    markCat,
    parser,
    ) where

import Control.Applicative

import qualified Text.Parsec as Psc
import Text.Parsec.String (Parser)

import qualified DepMarking as DMing
import qualified PTPrintable as PTP

data DepMarked cat
    = (:|) {
        category:: cat,
        dependency :: DMing.DepMarking
    } deriving (Eq)
        
instance (Show cat) => Show (DepMarked cat) where
    show (cat :| dep) = (show cat) ++ "|" ++ (show dep)

instance (PTP.Printable cat) => PTP.Printable (DepMarked cat) where
    psdPrint 
        min@(PTP.Option _ node)
        dm@(cat :| DMing.None)
            | node == PTP.Full
                = show dm
            | otherwise
                = PTP.psdPrint min cat
    psdPrint _ dm = show dm

instance Functor DepMarked where
    -- fmap :: (a -> b) -> (DepMarked a) -> (DepMarked b)
    fmap f (cat :| dep) = (f cat) :| dep

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
