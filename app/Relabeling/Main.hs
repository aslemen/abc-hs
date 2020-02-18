{-# OPTIONS_HADDOCK show-extensions #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
    Module      : Relabeling
    Copyright   : Copyright (c) 2018-2019 Noritsugu Hayashi
    License     : MIT
    
    Maintainer  : Noritsugu Hayashi <net@hayashi-lin.net>
    Stability   : alpha
    Portability : Linux x64 only

A program that converts normalized and dependency-marked Keyaki Treebank trees to ABC Treebank trees.
-}
module Relabeling where

import qualified Paths_abc_hs as Paths
import qualified Data.Version as DVer

import qualified System.IO as S
import qualified Options.Applicative as OA

import qualified Data.Text as DT
import qualified Data.Text.IO as DTIO
import qualified Data.Void as DV
import qualified Data.Set as DS
import qualified Data.List.Split as DLS

import qualified Text.Megaparsec as TMega

import qualified Control.Monad.State as CMS

import qualified DepMarking as DMing
import qualified DepMarked as DMed
import qualified DepMarked.Parser as DMedP
import qualified ABCCategory as ABCC

import qualified ParsedTree as PT
import qualified ParsedTree.Parser as PTP

import qualified Data.Text.Prettyprint.Doc as PDoc
import qualified Data.Text.Prettyprint.Doc.Render.Text as PDocRT

-- | = Data Types

-- | == Categories

-- | The type of categories on source tree nodes.
type PlainCat = DT.Text 

-- | The type of dependency-marked categories on source tree nodes.
type PlainMarked = DMed.DepMarked PlainCat

-- | The type of ABC categories.
type ABCCat = ABCC.ABCCategory 

-- | The type of resulting ABC Treebank keeping dependency markings.
type ABCCatMarked = DMed.DepMarked ABCCat

-- | The type of resulting ABC Treebank categories with fallback to original categories.
type ABCOrPlainCat = Either PlainCat ABCCat

-- | == Trees

-- | The type of source trees.
type PlainTMarked = PT.Tree PlainMarked

-- | The type of resulting trees keeping dependency markings.
type ABCTMarked = PT.Tree ABCCatMarked

matchTerminalNode :: ABCTMarked -> (DS.Set DT.Text) -> Bool
matchTerminalNode (PT.Node _ subtrees) li
    = case subtrees of 
        (:)
            PT.Node {
                PT.rootLabel = (ABCC.BaseCategory lex) DMed.:| _
            }
            []
            -> lex `elem` li
        _ -> False

isKTPRO tree = matchTerminalNode tree (DS.fromList ["*PRO*", "*T*"])

dropAnt :: [ABCCat] -> ABCCat -> ABCCat
dropAnt
    forbidList
    cat@(ABCC.LeftFunctor ant conseq)
        | ant `notElem` forbidList  = dropAnt forbidList conseq
        | otherwise                 = cat
dropAnt _ cat = cat

type RelabelFunc children = ABCCat -> DMing.DepMarking -> children -> ABCTMarked

data SeparatedChildren
    = SeparatedChildren {
        preHead :: [PlainTMarked]
        ,
        head :: PlainTMarked
        ,
        postHeadRev :: [PlainTMarked]
    }

getPreHead :: SeparatedChildren -> Either SeparatedChildren (PlainTMarked, SeparatedChildren) 
getPreHead sc
    = case preHead sc of 
        x:xs -> Right (x, (sc {preHead = xs}))
        _ -> Left sc

getPostHeadLast :: SeparatedChildren -> Either SeparatedChildren (SeparatedChildren, PlainTMarked) 
getPostHeadLast sc
    = case postHeadRev sc of
        y:ys -> Right ((sc {postHeadRev = ys}), y)
        _ -> Left sc

splitChildren :: [PlainTMarked] -> Maybe SeparatedChildren
splitChildren oldChildren@(oldChildFirst:oldChildrenRest)
    = if (DMed.dependency $ PT.rootLabel oldChildFirst) == DMing.Head
        then Just SeparatedChildren {
            preHead = []
            ,
            Relabeling.head = oldChildFirst
            ,
            postHeadRev = reverse oldChildrenRest
        }
        else 
            case splitChildren oldChildrenRest of
                Just sc -> Just sc {
                    preHead = oldChildFirst:(preHead sc)
                }
                Nothing -> Nothing
splitChildren [] = Nothing

relabel :: PlainTMarked -> ABCTMarked
relabel (PT.Node oldTreeRootLabel oldTreeChildren)
    = relabelRouting 
        (ABCC.BaseCategory $ DMed.category oldTreeRootLabel) 
            -- The parent (root) label candidate for the new tree
        (DMed.dependency oldTreeRootLabel)
            -- The dependency marking of the new parent (root) label
        oldTreeChildren
            -- The immediate children of the root

relabelRouting :: RelabelFunc [PlainTMarked] 
relabelRouting newParentCandidate newParentMark oldChildren
    = case oldChildren of
        _:_:_ 
            -> case splitChildren oldChildren of
                Just sc -> relabelHeaded newParentCandidate newParentMark sc
                Nothing -> relabelTrivial newParentCandidate newParentMark oldChildren
        _ -> relabelTrivial newParentCandidate newParentMark oldChildren

relabelTrivial :: RelabelFunc [PlainTMarked] 
relabelTrivial newParentCandidate newParentMark oldChildren 
    = PT.Node{
        PT.rootLabel
            = newParentCandidate DMed.:| newParentMark
        ,
        PT.subForest 
            = relabel <$> oldChildren
    }

relabelHeaded :: RelabelFunc SeparatedChildren
relabelHeaded newParentCandidate newParentMark separatedOldChildren 
    = case getPreHead separatedOldChildren of
        Right (oldFirstChild, oldRestChildren)
            -> case PT.rootLabel oldFirstChild of
                oldCompLabel DMed.:| DMing.Complement ->  -- 1. Complementを先に変換
                    let newFirstChild = relabel oldFirstChild
                        newFirstChildCat = DMed.category $ PT.rootLabel newFirstChild
                    -- 2. VSST Categoryを計算して，次に渡す
                        newVSSTCat = newFirstChildCat ABCC.<\> newParentCandidate
                        newVSST = relabelHeaded newVSSTCat DMing.Head oldRestChildren 
                    -- 3. Binarizationを行う．もし*PRO*があるならばそれをdropする．
                    in if isKTPRO newFirstChild -- TODO: 先頭にないPROをdropするのはまずいことなので，避けるべし．
                        then newVSST
                        else makeBinaryTree newFirstChild newVSST
                _ ->
                    -- 1. Adjunctを変換．
                    let newFirstChildCat = newParentCandidate ABCC.</> newParentCandidate 
                        newFirstChild = relabelRouting newFirstChildCat DMing.Adjunct (PT.subForest oldFirstChild) 
                    -- 2. 同時に，Headも変換．
                        newVSSTCat = newParentCandidate
                        newVSST = relabelHeaded newVSSTCat DMing.Head oldRestChildren 
                    -- 3. Binarizationを行う
                    in if isKTPRO newFirstChild
                        then newVSST
                        else makeBinaryTree newFirstChild newVSST
        Left allOldChildren
            -> case getPostHeadLast allOldChildren of
                Right (oldRestChildren, oldLastChild)
                    -> case PT.rootLabel oldLastChild of
                        _ DMed.:| DMing.Complement ->
                            -- 1. Complementを先に変換
                            let newLastChild = relabel oldLastChild
                                newLastChildCat = DMed.category $ PT.rootLabel newLastChild
                            -- 2. VSST Categoryを計算して，次に渡す
                                newVSSTCat = newParentCandidate ABCC.</> newLastChildCat
                                newVSST = relabelHeaded newVSSTCat DMing.Head oldRestChildren 
                            -- 3. Binarizationを行う
                            in makeBinaryTree newVSST newLastChild
                        _ DMed.:| adjMarking -> 
                            -- 1. Adjunctを変換．
                            let newLastChildCatBase 
                                    = if adjMarking == DMing.AdjunctControl 
                                        then dropAnt [ABCC.BaseCategory "PPs", ABCC.BaseCategory "PPs2"] newParentCandidate 
                                        else dropAnt [] newParentCandidate
                                newLastChildCat = newLastChildCatBase ABCC.<\> newLastChildCatBase
                                newLastChild = relabelRouting newLastChildCat adjMarking (PT.subForest oldLastChild) 
                            -- 2. 同時に，Headも変換．
                                newVSSTCat = newParentCandidate
                                newVSST = relabelHeaded newVSSTCat DMing.Head oldRestChildren 
                            -- 3. Binarizationを行う
                            in makeBinaryTree newVSST newLastChild
                Left finalChildList 
                    -> relabelRouting 
                        newParentCandidate 
                        newParentMark
                        (PT.subForest $ Relabeling.head finalChildList) 
                        
        where
            makeBinaryTree :: ABCTMarked -> ABCTMarked -> ABCTMarked 
            makeBinaryTree first second
                = PT.Node {
                    PT.rootLabel = newParentCandidate DMed.:| newParentMark
                    ,
                    PT.subForest = [first, second]
                }

runParserDoc :: 
    String
        -> DT.Text 
        -> Either (TMega.ParseErrorBundle DT.Text DV.Void) [PlainTMarked]
runParserDoc 
    = PTP.createDoc PTP.getDefaultTermParsers

parseDoc :: DT.Text -> IO [PlainTMarked]
parseDoc text
    = case runParserDoc "<STDIN>" text of
        Left errors
            -> DTIO.hPutStrLn S.stderr (
                DT.pack
                    $ TMega.errorBundlePretty errors
            )
            >> return []
        Right res 
            -> return res

------------------------------------------------

data Option = Option {
    calledVersion :: Bool,
    isOneLine :: Bool
}

optionParser :: OA.Parser Option
optionParser
    = Option
        <$> (
            OA.switch (OA.long "version" <> OA.short 'v')
        )
        <*> (
            OA.switch (OA.long "oneline" <> OA.short 'w')
        )

optionParserInfo :: OA.ParserInfo Option
optionParserInfo
    = OA.info (optionParser OA.<**> OA.helper)
        $ OA.briefDesc 
            <> OA.progDesc "The relabel program for the ABC Treebank"

runWithOptions :: Option -> IO ()
runWithOptions Option { calledVersion = True }
    = putStr "App \"Relabeling\" in abc-hs "
        >> putStrLn (DVer.showVersion Paths.version)

runWithOptions (Option _  isOneLine)
    = DTIO.getContents
        >>= parseDoc
        >>= return . (map (PDoc.pretty . relabel))
        >>= return . (map (if isOneLine then PDoc.group else id))
        >>= return . PDoc.vsep
        >>= PDocRT.putDoc

main :: IO ()
main = OA.execParser optionParserInfo >>= runWithOptions
