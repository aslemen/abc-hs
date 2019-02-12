{- 
The Pipeline:
- pretreatments
- head-marking (Haskell? tsurgeon?)
- **relabelling**
-}

module Relabeling where

import Data.Maybe

import qualified KeyakiCategory as KC
import qualified DepMarking as DMing
import qualified DepMarked as DMed
import qualified ABCCategory as ABCC
import qualified ParsedTree as PT

-- # Type Aliases
type KCat = KC.KeyakiCategory
type ABCCat = ABCC.ABCCategory

type Marked cat = (DMed.DepMarked) cat
type KCatMarked = Marked KCat
type ABCCatMarked = Marked ABCCat

type KTMarked = PT.Tree KCatMarked
type ABCTMarked = PT.Tree ABCCatMarked


parserKTMarked = DMed.parser KC.parser

-- # Tree Parser
runParserKTMarked :: String -> Either PT.ParseError KTMarked
runParserKTMarked
    = PT.createFromString parserKTMarked

runParserDoc :: String -> Either PT.ParseError [KTMarked]
runParserDoc = PT.createDoc parserKTMarked

-- # Main Job
createABCCBaseFromKC :: KCat -> ABCCat
createABCCBaseFromKC = ABCC.createBase . KC.showCat
{- |
    Example:
    [B C D E|h F G]
    -> [B C D], [E], [F G]
-}
data SplitChildren prehead head posthead = SplitChildren {
    preHeads :: [prehead],
    headMaybe :: Maybe head,
    postHeads :: [posthead]
    } deriving (Eq, Show) -- Maybe Intでいいのでは？
type UniformSplitChildren cat = SplitChildren cat cat cat

concatChildren :: UniformSplitChildren cat -> [cat]
concatChildren SplitChildren {
        preHeads = pre,
        headMaybe = hm,
        postHeads = post
    }
    = pre ++ head ++ post
    where
        -- head :: [cat]
        head 
            = case hm of
                Just h -> [h]
                Nothing -> []

findHead ::
    [Marked cat] 
    -> UniformSplitChildren (Marked cat)
findHead children
    = findHeadLoop children $ SplitChildren [] Nothing []
    where
    {- findHeadLoop :: 
        [Marked cat] 
        -> UniformSplitChildren (Marked cat)
        -> UniformSplitChildren (Marked cat) 
    -}
    findHeadLoop
        (leftMost:remainder) -- one or more child
        result@(SplitChildren { headMaybe = Nothing }) -- head not yet found
        | DMed.dependency leftMost == DMing.Head -- head found
            = result {
                headMaybe = Just leftMost,
                postHeads = remainder
                } 
        | otherwise -- continue searching
            = findHeadLoop
                remainder
                result { 
                    preHeads = (preHeads result) ++ [leftMost] 
                }
    findHeadLoop [] splitChildren -- no more searching
        = splitChildren

convertHead :: 
    ABCCat
    -> (UniformSplitChildren KCatMarked)
    -> SplitChildren KCatMarked ABCCatMarked KCatMarked
convertHead 
    catParent 
    SplitChildren {
        preHeads = pre,
        headMaybe = mh, -- type: KCatMarked
        postHeads = post
    }
    = SplitChildren {
        preHeads = pre,
        headMaybe = case mh of
                        Just _ -> Just (catParent DMed.:| DMing.Head)
                        Nothing -> Nothing
        ,
        postHeads = post
    }
{- |
    Example:
    [B C D], H|h, [F G]
    -> [B|c C|c <B\C>/<B\C>|a], B\C\H|h
-}
convertPreHead ::
    SplitChildren KCatMarked ABCCatMarked KCatMarked
    -> SplitChildren ABCCatMarked ABCCatMarked KCatMarked
convertPreHead children@(
    SplitChildren {
        preHeads 
            = preHead@(preHeadCat DMed.:| dep)
                :remainder, -- one or more prehead
        headMaybe 
            = Just head@(headCat DMed.:| _)
              -- there is actually a head
        }
    )
    | dep == DMing.Complement -- preHead ... head -> preHead preHead\head
        = let 
            newPreHeadCat = createABCCBaseFromKC preHeadCat -- preHead
            newPreHead = preHead { DMed.category = newPreHeadCat }
            newRemainder
                = convertPreHead children {
                    preHeads = remainder,
                    headMaybe = Just head {
                        DMed.category = newPreHeadCat ABCC.<\> headCat 
                                            -- preHead\Head
                        }
                }
            in 
                newRemainder {
                    preHeads 
                        = newPreHead:(preHeads newRemainder)
                }
    | dep == DMing.None -- do nothing and continue the loop
        = let
            newPreHead = createABCCBaseFromKC <$> preHead
            newRemainder
                = convertPreHead children{
                    preHeads = remainder
                }
            in 
                newRemainder {
                    preHeads
                        = newPreHead:(preHeads newRemainder)
                }
    | otherwise
        = let
            newPreHead
                = preHead {
                    DMed.category = headCat ABCC.</> headCat
                    } 
            newRemainder
                = convertPreHead children {
                    preHeads = remainder
                }
            in
                newRemainder {
                    preHeads = newPreHead:(preHeads newRemainder)
                }
convertPreHead children@(
    SplitChildren { -- the empty preHead case
            preHeads = [],
            headMaybe = hm,
            postHeads = post
        }
    ) = SplitChildren {
            preHeads = [],
            headMaybe = hm,
            postHeads = post
        }
convertPreHead children@(
    SplitChildren { -- the headless case
        preHeads = pre,
        headMaybe = Nothing,
        postHeads = post
        }
    ) 
    = SplitChildren {
        preHeads = map (createABCCBaseFromKC <$>) pre,
        headMaybe = Nothing,
        postHeads = post
        }

convertPostHead :: 
    SplitChildren ABCCatMarked ABCCatMarked KCatMarked
    -> UniformSplitChildren ABCCatMarked
convertPostHead children@(
    SplitChildren {
        headMaybe 
            = Just head@(headCat DMed.:| _),
                -- there is actually a head,
        postHeads 
            = postHead@(postHeadCat DMed.:| dep)
                :remainder -- one or more posthead
        }
    )
    | dep == DMing.Complement -- head .. postHead -> head/post post
        = let 
            newPostHeadCat = createABCCBaseFromKC postHeadCat -- postHead
            newPostHead = postHead { DMed.category = newPostHeadCat }
            newRemainder
                = convertPostHead children {
                    headMaybe = Just head {
                        DMed.category = headCat ABCC.</> newPostHeadCat
                                            -- head/post
                        },
                    postHeads = remainder                    
                }
            in 
                newRemainder {
                        postHeads 
                            = newPostHead:(postHeads newRemainder)
                    }
    | dep == DMing.None -- do nothing and continue the loop
    = let
        newPostHead = createABCCBaseFromKC <$> postHead
        newRemainder
            = convertPostHead children{
                postHeads = remainder
            }
        in 
            newRemainder {
                postHeads
                    = newPostHead:(postHeads newRemainder)
            }
    | otherwise -- head .. postHead -> head head\head
        = let
            newPostHead
                = postHead {
                    DMed.category = headCat ABCC.<\> headCat
                    } 
            newRemainder
                = convertPostHead children {
                    postHeads = remainder
                }
            in
                newRemainder {
                    postHeads = newPostHead:(postHeads newRemainder)
                }
convertPostHead children@(
    SplitChildren { -- the empty preHead case
            preHeads = pre,
            headMaybe = hm,
            postHeads = []
        }
    ) = SplitChildren { 
            preHeads = pre,
            headMaybe = hm,
            postHeads = []
        }
convertPostHead children@(
    SplitChildren { -- the headless case
        preHeads = pre,
        headMaybe = Nothing,
        postHeads = post
        }
    ) 
    = SplitChildren {
        preHeads = pre,
        headMaybe = Nothing,
        postHeads = map (createABCCBaseFromKC <$>) post
        }

convertAll :: 
    ABCCat
    -> UniformSplitChildren KCatMarked 
    -> UniformSplitChildren ABCCatMarked
convertAll catParent
    = convertPostHead . convertPreHead . (convertHead catParent)

getNewCategory :: 
    ABCCat -> [KCatMarked] -> [ABCCatMarked]
getNewCategory catParent
    = concatChildren 
        . revPostHeads 
        . (convertAll catParent) 
        . revPostHeads 
        . findHead
    where
        revPostHeads :: SplitChildren a b c -> SplitChildren a b c
        revPostHeads sc
            = sc { postHeads = reverse (postHeads sc) }

relabel :: KTMarked -> ABCTMarked
relabel node@(
        PT.Node {
            PT.label = (DMed.:|) {
                DMed.category = cat
            }
        }
    ) = relabelLoop (createABCCBaseFromKC cat) node 
    where
        relabelLoop :: ABCCat -> KTMarked -> ABCTMarked
        relabelLoop newParentCat PT.Node {
                PT.label = cat DMed.:| dep,
                PT.children = oldChildren
                }
            = PT.Node {
                PT.label = newParentCat DMed.:| dep,
                     -- convert the parent label lately
                PT.children = newChildren
                }
            where
                newChildrenCat :: [ABCCatMarked]
                newChildrenCat 
                    = getNewCategory newParentCat $ map PT.label oldChildren
                execSub :: (ABCCatMarked, KTMarked) -> ABCTMarked
                execSub (newCat, oldTree) 
                    = relabelLoop (DMed.category newCat) oldTree -- Loop
                newChildren :: [ABCTMarked]
                newChildren = 
                    map execSub $ zip newChildrenCat oldChildren

-- # Routine
parseDoc :: String -> IO [KTMarked]
parseDoc str
    = case runParserDoc str of
        Left err 
            -> putStrLn ("\n" ++ show err) >> return []
        Right res 
            -> return res

batchRelabel :: [KTMarked] -> [ABCTMarked]
batchRelabel = fmap relabel

main :: IO ()
main 
    = (
        batchRelabel 
        <$> (getContents >>= parseDoc)
    ) >>= foldr ((>>) . (putStr . show)) (return ())