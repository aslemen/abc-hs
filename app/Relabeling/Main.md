# ABCTreebank：範疇変換のプログラム
## 概要 / Introduction
与えられた，正規化されたKeyakiの木から，範疇を変換して，ABC Treebankの木をつくる．
根本にあるアルゴリズムは＠＠＠のものに他ならないが，
    コントロールされた項の扱いなどのために，より複雑になっている．

## ヘッダー / Header
文字列としては`Data.Text`を使用．同時に，多相的な文字列リテラルを許す．
```haskell
{-# LANGUAGE OverloadedStrings #-}
```

モジュール名を設定する．
```haskell
module Relabeling where
```

基本的なモジュールのインポートをする．
```haskell
import qualified System.IO as S

import qualified Data.Text as DT
import qualified Data.Text.IO as DTIO
import qualified Data.Void as DV
import qualified Data.Set as DS
import qualified Data.List.Split as DLS
```

パーザーコンビネーターとして，Megaparsecを使用する．
```haskell
import qualified Text.Megaparsec as TMega

import qualified Control.Monad.State as CMS
```

ABC Treebankのためのモジュールを読み込む．
```haskell
import qualified DepMarking as DMing
import qualified DepMarked as DMed
import qualified DepMarked.Parser as DMedP
import qualified ABCCategory as ABCC

import qualified ParsedTree as PT
import qualified ParsedTree.Parser as PTP
```

結果の出力として，prettyprinterを用いる．
```haskell
import qualified Data.Text.Prettyprint.Doc as PDoc
import qualified Data.Text.Prettyprint.Doc.Render.Text as PDocRT
```

## データ型の別名 / Aliases of Data Types
木や範疇に関するデータ型の略名をつける．

`ABCCat`：ABC Treebankの（変換後の，組み合わせ論理の）範疇を表現する．
```haskell
type ABCCat = ABCC.ABCCategory
```

`...Marked`：範疇で，dependency markingが付加されたものを表現する．
このうち`Plain`とは，変換前の（Keyakiの）範疇（実際，ただの`DT.Text`）を表現する．
```haskell
type PlainMarked = DMed.DepMarked DT.Text
type ABCCatMarked = DMed.DepMarked ABCCat
```

`...TMarked`：木で，ノードがdependency marked categoryであるようなものを
表現する．
```haskell
type PlainTMarked = PT.Tree PlainMarked
type ABCTMarked = PT.Tree ABCCatMarked
```

## 判定関数 / Decisive Functions
`isKTPRO`：あるKeyaki部分木（dependency markingを含む）が，
    終端ノードと語彙ノードだけを持ち，
    かつ語彙ノードとして空範疇（`*PRO*`または`*T*`）を持つかどうか？

```haskell
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
```

## 型簡略化
関数合成がより簡単な型で達成される場合は，そのようにするが，そのための計算の機序．

```haskell
dropAnt :: [ABCCat] -> ABCCat -> ABCCat
dropAnt
    forbidList
    cat@(ABCC.LeftFunctor ant conseq)
        | ant `notElem` forbidList  = dropAnt forbidList conseq
        | otherwise                 = cat
dropAnt _ cat = cat
```

## 変換アルゴリズムの概要
変換とは，まだ変換を受けていない部分木を持つあるKeyaki（部分）木
    （dependency markingを受けている）を対象とするものである．
変換においては，ルートノードの候補はあらかじめ（再帰的に，外から）ABC Treebank範疇
    として与えられている．
ルートノードは，通常，与えられたものがそのまま最終的なものとなるが，
    部分木として空範疇が与えらているような場合，
    最終的なルートノードはそれに応じて変更されることがある．
```
Parent (as ABCCat)
    |- Subtree 1
    |- Subtree 2
    |- ...
    |- Subtree n
```
従って，変換とは，以下を引数とする関数であるべきである：
- 最終ルートノードの候補：`ABCCat`
- 最終ルートノードのDependency Marking: `DMing.DepMarking`
- 子木のリスト：`[PlainTMarked]`
    - ただし，現状では，（捨てられる運命にある）ルートノードも一緒に
        引数として渡されることになる．
        これについて以後変更することがありうる．
- PRO-droppingに関するパラメータ：`[Text]`

また，変換はtop-downで，引数に加え，さらに以下の要素に左右される：
    部分木の変換の結果
    （すなわち，全体の結果は，部分木の内部（特に，空範疇）にも依存する）．
変換の返り値は，`ABCTMarked`であるべきである．

このような関数の型を，以下のように定義する．
```haskell
type RelabelFunc children = ABCCat -> DMing.DepMarking -> children -> ABCTMarked
```

```haskell
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
```
## 変換 / Relabeling
ルート木の変換は，`relabel`関数でなされる．
`relabel`は，dependency markingのなされている木を取り，
（dependency markingを残した）ABC Treebankの木を返す．

`relabelRouting`は，relabelの再帰において呼び出される．
機能はだた1つで，ルーティングを行う：
- もし，与えられた子木たちが2つ以上の子を持ち，かつ，headを持つなら， `relabelHeaded` を行う．
- そうでないなら，不relabel `relabelTrivial` を行う．

```haskell
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
```

## パーザー実行者 / Parser Executors
組み合わせパーザーを実行する関数．

`runParserDoc name source`：
入力`source`から，
**dependency markを受けたKeyakiの（0個以上の）統語木**を読み込み，
パージングをし，プログラム内部の表現に変換する．
入力源の名前は`name`である（実際のパージングには影響はない）．
結果として，パージングが成功の場合，
`PlainTMarked`（Keyaki木）のリストが`Right`に包まれて得られる．
失敗の場合，MegaParsecのエラー型が`Left`に包まれて得られる．
```haskell
runParserDoc :: 
    String
        -> DT.Text 
        -> Either (TMega.ParseErrorBundle DT.Text DV.Void) [PlainTMarked]
runParserDoc 
    = PTP.createDoc PTP.getDefaultTermParsers
```

# 実行ルーティン / Routines in Execution
```haskell
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

main :: IO ()
main 
    = DTIO.getContents
        >>= parseDoc
        >>= (PDocRT.putDoc . PDoc.vsep . (map (PDoc.pretty . relabel)))
```