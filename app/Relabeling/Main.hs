{-# OPTIONS_HADDOCK show-extensions #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}


{- |
    Module      : Relabeling
    Copyright   : Copyright (c) 2018-2020 Noritsugu Hayashi
    License     : MIT
    
    Maintainer  : Noritsugu Hayashi <net@hayashi-lin.net>
    Stability   : alpha
    Portability : Linux x64 only

A program that converts normalized and dependency-marked Keyaki Treebank trees to ABC Treebank trees.
-}
module Relabeling where

import Paths_abc_hs (version)
import Data.Version (showVersion)

import System.IO (stdout, stderr)
import qualified System.IO as SIO
import qualified Options.Applicative as OA

import Data.Function ((&))
import Control.Monad (forM_)
import Control.Monad.Catch (
        MonadThrow(..)
        , MonadCatch(..)
        , Exception(..)
        , SomeException
    )
import Data.Typeable (Typeable)

import Data.Text (Text)
import qualified Data.Text as DT
import qualified Data.Text.IO as DTIO
import Data.Void (Void)
import Data.Set (Set)
import qualified Data.Set as DS
import Data.Tree (Tree(..))

import Text.Megaparsec (runParserT, ParseErrorBundle, errorBundlePretty)
import Text.PennTreebank.Parser.Megaparsec.Char (pUnsafeDoc, PennDocParserT)

import qualified Data.Text.Prettyprint.Doc as PDoc
import qualified Data.Text.Prettyprint.Doc.Render.Text as PDocRT

import KeyakiCat
import CatPlus
import ABCCat
import ABCDepMarking
import ParsedTree

-- | = Data Types

-- | == Categories

matchTerminalNode :: Tree (CatPlus a) -> (Set Text) -> Bool
matchTerminalNode (Node _ ((Node (Term w) []):[])) li 
    = w `elem` li
matchTerminalNode _ _
    = False 

{-|
    Tell whether a subtree contains only a terminal node 
        that is exactly an empty category.
-}
isKTPRO tree = matchTerminalNode tree (DS.fromList ["*PRO*", "*T*"])

dropAnt :: [ABCCat] -> ABCCat -> ABCCat
dropAnt
    forbidList
    cat@(LeftFunctor ant conseq)
        | ant `notElem` forbidList  = dropAnt forbidList conseq
        | otherwise                 = cat
dropAnt _ cat = cat

type RelabelFunc children 
    = ABCCat 
        -> (ABCCat -> CatPlus ABCCat) 
        -> children 
        -> ABCTree

type KeyakiTree = Tree (CatPlus KeyakiCat)
type ABCTree    = Tree (CatPlus ABCCat)

{-|
    A data structure that represents a way of segmentation of 
        children of a 'KeyakiTree' node
        based on their grammatical roles (of type 'DepMarking').
-}
data SeparatedChildren
    = SeparatedChildren {
        preHead :: [KeyakiTree] -- ^ The children that precede the head
        , head :: KeyakiTree    -- ^ The child that is the head of the (sub)tree
        , postHeadRev :: [KeyakiTree] -- ^ The children that follow the head 
    }

{-|
    Peel off from a 'SeparatedChilren' list 
        a leftmost child that precedes the head.
-}
getPreHead :: 
    SeparatedChildren -- ^ A segmented list of 'KeyakiTree's.
    -- | If it has a pre-head element, the result is 
    --          the element and the remainder.
    --      If not, the given list is just returned untouched.
    -> Either SeparatedChildren (KeyakiTree, SeparatedChildren) -- 
getPreHead sc
    = case preHead sc of 
        x:xs -> Right (x, (sc {preHead = xs}))
        []   -> Left sc

pattern x :-|: sc       <- (getPreHead -> Right (x, sc)) 
pattern EmptyPreHead sc <- (getPreHead -> Left sc)

getPostHeadLast :: 
    SeparatedChildren 
    -> Either SeparatedChildren (SeparatedChildren, KeyakiTree) 
getPostHeadLast sc
    = case postHeadRev sc of
        y:ys -> Right ((sc {postHeadRev = ys}), y)
        [] -> Left sc

pattern sc :|-: y           <- (getPostHeadLast -> Right (sc, y))
pattern EmptyPostHeadRev sc <- (getPostHeadLast -> Left sc)

splitChildren :: 
    [KeyakiTree] 
    -> Maybe SeparatedChildren
splitChildren oldChildren@(oldChildFirst:oldChildrenRest)
    = case rootLabel oldChildFirst of
        NonTerm { role = Head } 
            -> Just SeparatedChildren {
                preHead = []
                , Relabeling.head = oldChildFirst
                , postHeadRev = reverse oldChildrenRest
            }
        _ 
            -> case splitChildren oldChildrenRest of -- RECURSION
                Just sc -> Just sc {
                    preHead = oldChildFirst:(preHead sc)
                }
                Nothing -> Nothing
splitChildren [] 
    = Nothing

-- | = Exceptions 

data (Show cat) =>
    IllegalTerminalException cat 
    = IllegalTerminalException { illegalCat :: cat }
    deriving (Show)

instance (Show cat, Typeable cat) 
    => Exception (IllegalTerminalException cat)

---------------------------

genABCCat :: KeyakiCat -> ABCCat
{-# INLINE genABCCat #-}
genABCCat = BaseCategory . DT.pack . show

{-|
    Convert a Keyaki tree to an ABC Grammar one.
-}
relabel :: (MonadThrow m) => KeyakiTree -> m ABCTree
relabel Node {
    rootLabel = nt@(NonTerm { deriv = "" }) -- filter out special derivations
    , subForest = oldTreeChildren
} = do
    case genABCCat <$> nt of 
        newCat :#: attrs
            -> relabelRouting newCat attrs oldTreeChildren
                -- @newCat@: The parent (root) label candidate for the new tree
                -- @attrs@: The dependency marking of the new parent (root) label
                -- @oldTreeChildren@: The immediate children of the root
        Term w ->
            relabelTrivial (Term w) oldTreeChildren
relabel Node {
    rootLabel = oldParent
    , subForest = oldTreeChildren
} = relabelTrivial (genABCCat <$> oldParent) oldTreeChildren

{-| 
    (Internal conversion function) 
    A routing function that detects the head in a (sub)tree and 
        directs to `relabelHeaded` if found,
        or `relabelTrivial` if not.
-}
relabelRouting :: (MonadThrow m)
    => ABCCat 
    -> (ABCCat -> CatPlus ABCCat) 
    -> [KeyakiTree] 
    -> m ABCTree
relabelRouting newParentCandidate newParentPlus oldChildren
    = case oldChildren of
        _:_:_ -- If it has more than 1 children
            -> case splitChildren oldChildren of
                Just sc -> relabelHeaded newParentCandidate newParentPlus sc
                Nothing -> relabelTrivial newParent oldChildren
        _ -> relabelTrivial newParent oldChildren
            -- otherwise
    where
        newParent :: CatPlus ABCCat
        newParent = newParentPlus newParentCandidate

{-|
    (Internal conversion function) 
    A trivial conversion function that 
        does nothing to the topmost layer of the given tree
        but reinitiate conversion processes `relabel` at each children.
-}
relabelTrivial :: (MonadThrow m)
    => CatPlus ABCCat 
    -> [KeyakiTree] 
    -> m ABCTree
relabelTrivial newParent oldChildren = do
    relabeledChildren <- mapM relabel oldChildren -- RECURSION
    return $ Node {
        rootLabel = newParent
        , subForest = relabeledChildren
    }

relabelHeaded :: (MonadThrow m)
    => ABCCat 
    -> (ABCCat -> CatPlus ABCCat) 
    -> SeparatedChildren
    -> m ABCTree

-- Case 1a: Pre-head, Head-Complement
relabelHeaded 
    newParentCandidate 
    newParentPlus
    (
        oldFirstChild@Node {
            rootLabel = (oldFirstChildCat, Complement) :#||: attrs
        }
        :-|: oldRestChildren
    ) = do 
        -- 1. Complementを先に変換
        newFirstChild <- relabel oldFirstChild
        case newFirstChild & rootLabel of
            (newFirstChildCat, _) :#||: attrs -> do
            -- 2. VSST Categoryを計算して，次に渡す
                let newVSSTCat = newFirstChildCat :\: newParentCandidate
                newVSST <- relabelHeaded 
                            newVSSTCat 
                            (\y -> (newNonTerm y) { role = Head })
                            oldRestChildren
                -- 3. Binarizationを行う．もし*PRO*があるならばそれをdropする．
                return $ if isKTPRO newFirstChild
                    then newVSST
                    else Node {
                        rootLabel = newParentPlus newParentCandidate
                        , subForest = [newFirstChild, newVSST]
                    }
            Term w -> throwM $ IllegalTerminalException w

-- Case 1c: Pre-head, other cases (default to Head-Adjunct)
relabelHeaded 
    newParentCandidate 
    newParentPlus
    (
        Node {
            rootLabel = oldFirstChildLabel
            , subForest = oldFirstChildChildren
        }
        :-|: oldRestChildren
    ) = do 
        -- 1. Adjunctを変換．
        let newFirstChildCat = newParentCandidate :/: newParentCandidate
        newFirstChild <- relabelRouting 
                            newFirstChildCat
                            (\y -> ((const y) <$> oldFirstChildLabel) { role = Adjunct })
                            oldFirstChildChildren
        -- 2. 同時に，Headも変換．
        let newVSSTCat = newParentCandidate
        newVSST <- relabelHeaded 
                    newVSSTCat
                    (\x -> (newNonTerm x) { role = Head })
                    oldRestChildren 
        -- 3. Binarizationを行う
        return $ if isKTPRO newFirstChild 
            then newVSST -- dropping newFirstChild (*PRO*)
            else Node {
                rootLabel = newParentPlus newParentCandidate
                , subForest = [newFirstChild, newVSST]
            }

-- Case 1x: Pre-head, unexpected terminal node
relabelHeaded _ _ (Node {rootLabel = Term w} :-|: _)
    = throwM $ IllegalTerminalException w

-- Case 2a: Post-head, Head-Complement
relabelHeaded 
    newParentCandidate 
    newParentPlus
    (
        oldRestChildren 
        :|-: oldLastChild@Node {
            rootLabel = (_, Complement) :#||: attrs
        }
    ) = do 
        -- 1. Complementを先に変換
        newLastChild <- relabel oldLastChild
        let newLastChildCat = newLastChild & rootLabel & cat
        -- 2. VSST Categoryを計算して，次に渡す
            newVSSTCat = newParentCandidate :/: newLastChildCat
        newVSST <- relabelHeaded 
                    newVSSTCat 
                    (\x -> (newNonTerm x) { role = Head }) 
                    oldRestChildren 
        -- 3. Binarizationを行う
        return $ Node {
            rootLabel = newParentPlus newParentCandidate
            , subForest = [newVSST, newLastChild]
        }

-- Case 2b: Post-head, Head-AdjunctControl
relabelHeaded 
    newParentCandidate 
    newParentPlus
    (
        oldRestChildren 
        :|-: oldLastChild@Node {
            rootLabel = oldFirstChildLabel@((_, AdjunctControl) :#||: attrs)
            , subForest = oldLastChildChildren
        }
    ) = do 
        -- 1. Adjunctを変換．
        let newLastChildCatBase = dropAnt [BaseCategory "PPs", BaseCategory "PPs2"] newParentCandidate
            newLastChildCat = newLastChildCatBase :\: newLastChildCatBase
        newLastChild <- relabelRouting 
                            newLastChildCat 
                            (\y -> ((const y) <$> oldFirstChildLabel))
                            oldLastChildChildren
        -- 2. 同時に，Headも変換．
        let newVSSTCat = newParentCandidate
        newVSST <- relabelHeaded 
                    newVSSTCat 
                    (\x -> (newNonTerm x) { role = Head }) 
                    oldRestChildren
        -- 3. Binarizationを行う
        return $ Node {
            rootLabel = newParentPlus newParentCandidate
            , subForest = [newVSST, newLastChild]
        }
        -- TODO: FCの深さについても言えるようにする．

-- Case 2c: Post-head, elsewhere (default to Head-Adjunct)
relabelHeaded 
    newParentCandidate 
    newParentPlus
    (
        oldRestChildren 
        :|-: oldLastChild@Node {
            rootLabel = oldLastChildLabel@((_, r) :#||: attrs)
            , subForest = oldLastChildChildren
        }
    ) = do 
        -- 1. Adjunctを変換．
        let newLastChildCatBase = dropAnt [] newParentCandidate
            newLastChildCat     = newLastChildCatBase :\: newLastChildCatBase
        newLastChild <- relabelRouting
                            newLastChildCat
                            (\y -> ((const y) <$> oldLastChildLabel) { role = r })
                            oldLastChildChildren
        -- 2. 同時に，Headも変換．
        let newVSSTCat = newParentCandidate
        newVSST <- relabelHeaded 
                    newVSSTCat 
                    (\x -> (newNonTerm x) { role = Head })  
                    oldRestChildren 
        -- 3. Binarizationを行う
        return $ Node {
            rootLabel = newParentPlus newParentCandidate
            , subForest = [newVSST, newLastChild]
        }

-- Case 3: Reached the very head
relabelHeaded 
    newParentCandidate 
    newParentPlus
    (EmptyPostHeadRev finalChildList) 
    = relabelRouting 
        newParentCandidate 
        newParentPlus
        (subForest $ Relabeling.head finalChildList) 

-- | = Execution routines

{-|
    The collection of program options.
-}
data Option = Option {
    calledVersion :: Bool -- ^ Whether the version information is inquired.
    , isOneLine :: Bool -- ^ Whether the output trees are each printed in one line.
}

{-|
    The underlying command option parser.
-}
optionParser :: OA.Parser Option
optionParser
    = Option
        <$> (
            OA.switch (OA.long "version" <> OA.short 'v')
        )
        <*> (
            OA.switch (OA.long "oneline" <> OA.short 'w')
        )

{-|
    An command option parser augumented with a program description.
-}
optionParserInfo :: OA.ParserInfo Option
optionParserInfo
    = OA.info (optionParser OA.<**> OA.helper)
        $ OA.briefDesc 
            <> OA.progDesc "The relabel program for the ABC Treebank"

{-|
    A Keyaki tree document parser.
-}
pDocument :: (Monad m) => PennDocParserT Text m (CatPlus KeyakiCat)
{-# INLINE pDocument #-}
pDocument = pUnsafeDoc

{-|
    The main program routine that runs depending on a given set of options.
-}
runWithOptions :: Option -> IO ()
runWithOptions Option { calledVersion = True } = do
    putStr "App \"Relabeling\" in abc-hs "
    putStrLn $ showVersion version

runWithOptions (Option _  isOneLine) = do
    parsedRaw <- DTIO.getContents >>= runParserT pDocument "<STDIN>"
    trees <- case parsedRaw of
        Left errors -> do
            DTIO.hPutStrLn stderr (
                DT.pack $ errorBundlePretty errors
                )   
            return []
        Right ts -> return ts
    forM_ trees $ \tree -> processTree tree `catch` processExecptions tree
    where
        printTree :: _ -> IO ()
        printTree tree = tree 
            & PDoc.pretty
            & (if isOneLine then PDoc.group else id)
            & (if isOneLine   
                    then (<> PDoc.line <> PDoc.line) 
                    else (<> PDoc.line)
                )
            & PDoc.layoutPretty (PDoc.LayoutOptions PDoc.Unbounded)
            & PDocRT.renderIO stdout
        processTree :: _ -> IO ()
        processTree tree = 
            relabel tree -- IO ABCTree
            >>= printTree
        processExecptions :: _ -> SomeException -> IO ()
        processExecptions tree e = do
            SIO.hPutStr stderr "Exception: "
            SIO.hPutStrLn stderr $ show e
            SIO.hPutStrLn stderr "Tree:"
            tree & PDoc.pretty 
                 & PDoc.layoutPretty (PDoc.LayoutOptions PDoc.Unbounded)
                 & PDocRT.renderIO stderr
            SIO.hPutStrLn stderr ""

{-|
    The tnery point of this program.
-}
main :: IO ()
main = OA.execParser optionParserInfo >>= runWithOptions
