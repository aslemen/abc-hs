module Main where

import System.IO
import Data.Monoid

import qualified ABCCategory as ABCC
import qualified ParsedTree as PT

import qualified Options.Applicative as OPT

-- # Type Aliases
type ABCT = PT.Tree ABCC.ABCCategory

-- # Tree Parser
runParserTree :: String -> Either PT.ParseError ABCT
runParserTree = PT.createFromString ABCC.parser

runParserDoc :: String -> Either PT.ParseError [ABCT]
runParserDoc = PT.createDoc ABCC.parser

-- # Commandline Option Parser
-- よくわかっていないので、放っておくことにする。しばらくはstdin/stdoutを使う。
-- 並列処理とかできればもっと嬉しい。
data Options 
    = Options {
    inputPath :: FilePath,
    outputPath :: FilePath
    } deriving (Show)

parserInputPath :: OPT.Parser FilePath
parserInputPath
    = OPT.strArgument $ mconcat [
        OPT.help "The path to the input file",
        OPT.metavar "input_path",
        OPT.action "file"
    ]

parserOutputPath :: OPT.Parser FilePath
parserOutputPath
    = OPT.strArgument $ mconcat [
        OPT.help "The path of output",
        OPT.metavar "output_path",
        OPT.action "file"
    ]
    

parserOptions :: OPT.Parser Options
parserOptions
    = (<*>) OPT.helper (
        Options <$> parserInputPath <*> parserOutputPath
    )
        

parserOptionsWithInfo :: OPT.ParserInfo Options
parserOptionsWithInfo 
    = OPT.info parserOptions $ mconcat [
        OPT.fullDesc,
        OPT.progDesc "ABC Tree Checker"
        ]

-- # Actual Job
relabel :: ABCT -> ABCT
relabel (PT.Node node children)
    | (length children) == 2 
        = PT.Node {
            PT.label = node `ABCC.addComment` (com res node),
            PT.children = map relabel children
        }
    | otherwise 
        = PT.Node {
            PT.label = node,
            PT.children = map relabel children
        }
    where
        child1 :: ABCT
        child2 :: ABCT
        child1 : (child2 : _) = children
        res :: (ABCC.ABCCategory, ABCC.ABCStatusFC)
        res = ABCC.reduceWithResult (PT.label child1) (PT.label child2)
        com :: (ABCC.ABCCategory, ABCC.ABCStatusFC) -> ABCC.ABCCategory -> String
        com (_, ABCC.Failed) _ = "FAIL"
        com (cat_new, stat) cat_orig
            | cat_new == cat_orig
                = show stat
            | otherwise
                = "INCORR;" 
                    ++ (show cat_new) 
                    ++ ":"
                    ++ show stat

batchRelabel :: [ABCT] -> [ABCT]
batchRelabel
    = fmap relabel

parseDoc :: String -> IO [ABCT]
parseDoc str
    = case runParserDoc str of
        Left err 
            -> putStrLn ("\n" ++ show err) >> return []
        Right res 
            -> return res

-- # Main Procedure
main :: IO ()
main 
    = (
        batchRelabel 
        <$> (getContents >>= parseDoc)
    ) >>= foldr ((>>) . print) (return ())