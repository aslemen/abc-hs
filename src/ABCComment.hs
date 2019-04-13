{-# LANGUAGE OverloadedStrings #-}

module ABCComment (
    ABCComment(..),
    ) where

import Control.Applicative

import Data.Char as DCh
import Data.Text as DT
import Data.Text.Lazy as DTL
import Data.Text.Lazy.Builder as DTLB
import Data.String as DS

import qualified PTPrintable as PTP
import qualified PTDumpable as PTD

-- # The Data Type
data ABCComment a 
    = ABCComment {
        content :: a,
        comment :: DT.Text
    }

instance (Eq a) => Eq (ABCComment a) where
    (==) (ABCComment a _) (ABCComment b _)
        = (a == b) -- ignore comments

instance (PTD.Dumpable a) => PTD.Dumpable (ABCComment a) where
    psdDump opt@(PTD.Option _ PTD.Minimal) ca
        = (PTD.psdDump opt) (content ca)
    psdDump opt ca@(ABCComment a c)
        = if c == ""
            then (PTD.psdDump opt a)
            else (PTD.psdDump opt a) 
                    <> (DTLB.fromText ".\"")
                    <> (DTLB.fromText c)
                    <> (DTLB.singleton '\"')

instance (Show a) => Show (ABCComment a) where
    show (ABCComment a c)
        = if c == ""
            then (show a)
            else (show a) 
                    <> ".\"" <> (DT.unpack c) <> "\""

instance (PTP.Printable a) => PTP.Printable (ABCComment a) where
    psdPrint opt@(PTP.Option _ PTP.Minimal) ca
        = (PTP.psdPrint opt) (content ca)
    psdPrint opt ca@(ABCComment a c)
        = if c == ""
            then (PTP.psdPrint opt a)
            else (PTP.psdPrint opt a) 
                    <> ".\"" <> (DT.unpack c) <> "\""

instance Functor ABCComment where
    fmap f ABCComment {
        content = a,
        comment = c
        } = ABCComment {
            content = f a,
            comment = c
        }

instance Applicative ABCComment where
    pure a
        = ABCComment a ""
    (<*>) (ABCComment f cf) (ABCComment a ca)
        = ABCComment (f a) (cf <> ";" <> ca)

instance Monad ABCComment where
    (>>=) (ABCComment a ca) f
        = ABCComment com_new_content (ca <> ";" <> com_new_comment)
            where
                -- com_new :: ABCComment sth
                com_new = f a
                -- com_new_content :: sth
                com_new_content = content com_new
                com_new_comment :: DT.Text
                com_new_comment = comment com_new

-- instance (Monoid w) => CMW.MonadWriter w ABCComment where
--    writer (a, w)
--        = ABCComment {
--            content = a,
--            comment = w
--        }
--    tell w
--        = ABCComment {
--            content = (),
--            comment = w
--        }
--    listen ABCComment {
--        content = a,
--        comment = c
--        } = ABCComment {
--            content = (a, c),
--            comment = c
--        }
--    pass ABCComment{
--        content = (a, f),
--        comment = c
--        } = ABCComment{
--            content = a,
--            comment = f c
--        }