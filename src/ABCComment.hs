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

import qualified Data.Text.Prettyprint.Doc as PDoc

-- # The Data Type
data ABCComment a 
    = ABCComment {
        content :: a,
        comment :: DT.Text
    }

instance (Eq a) => Eq (ABCComment a) where
    (==) (ABCComment a _) (ABCComment b _)
        = (a == b) -- ignore comments

instance (PDoc.Pretty a) => PDoc.Pretty (ABCComment a) where
    pretty (ABCComment a c)
        = (PDoc.pretty a) <> (
            if c == ""
                then mempty
                else ".\"" <> (PDoc.pretty c) <> "\""
        )

instance (Show a) => Show (ABCComment a) where
    show (ABCComment a c)
        = if c == ""
            then (show a)
            else (show a) 
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
        | cf == "" && ca == ""
            = ABCComment (f a) ""
        | cf == ""
            = ABCComment (f a) ca
        | ca == ""
            = ABCComment (f a) cf
        | otherwise
            = ABCComment (f a) (cf <> ";" <> ca)

instance Monad ABCComment where
    (>>=) (ABCComment a ca) f
        = ABCComment com_new_content com_new_comment_comb
        where
            -- com_new :: ABCComment sth
            com_new = f a
            -- com_new_content :: sth
            com_new_content = content com_new
            com_new_comment :: DT.Text
            com_new_comment = comment com_new
            com_new_comment_comb :: DT.Text
            com_new_comment_comb
                | ca == "" && com_new_comment == ""
                    = ""
                | ca == ""
                    = com_new_comment
                | com_new_comment == ""
                    = ca
                | otherwise
                    = (ca <> ";" <> com_new_comment)
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