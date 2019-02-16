module Linting where

import System.IO
import Data.Monoid

import qualified ABCCategory as ABCC
import qualified ParsedTree as PT
import qualified StringWithBrackets as SWB
import qualified Options.Applicative as OPT

-- # Type Aliases
type ST = PT.Tree String

-- # Tree Parser
runParserTree :: String -> Either PT.ParseError ST
runParserTree 
    = PT.createFromString $ SWB.parserStringOrBracketedString "()"

runParserDoc :: String -> Either PT.ParseError [ST]
runParserDoc 
    = PT.createDoc $ SWB.parserStringOrBracketedString "()"

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
lint :: ST -> ST
lint = id

parseDoc :: String -> IO [ST]
parseDoc str
    = case runParserDoc str of
        Left err 
            -> putStrLn ("\n" ++ show err) >> return []
        Right res 
            -> return res

-- # Main Procedure
main :: IO ()
main 
    = getContents
        >>= parseDoc
        >>= mapM_  (putStr . show . lint)