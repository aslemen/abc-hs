{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module KaiCheck where

import qualified Data.Text as DT
import qualified Data.Text.IO as DTIO
import qualified Data.Text.Lazy as DTL
import qualified Data.Text.Lazy.Builder as DTLB
import qualified Data.Void as DV
import qualified Data.Set as DS

import qualified Text.Megaparsec as TMega
import qualified Control.Monad.State as CMS
import qualified Control.Monad.Catch as CMC

import qualified KaiCat as Kai
import qualified ParsedTree as PT
import qualified ParsedTree.Parser as PTP

import qualified PTDumpable as PTD

import qualified Options.Applicative as OPT

-- # Type Aliases
type KaiCat = Kai.KaiCat
type KaiT = PT.Tree KaiCat

{-
    ======
    Tree Parsers
    ======
-}
runParserTree :: String 
    -> DT.Text
    -> Either (TMega.ParseErrorBundle DT.Text DV.Void) KaiT
runParserDoc :: String 
    -> DT.Text
    -> Either (TMega.ParseErrorBundle DT.Text DV.Void) [KaiT]
runParserTree = PTP.createFromString PTP.getDefaultTermParsers
runParserDoc = PTP.createDoc PTP.getDefaultTermParsers

-- # Commandline Option Parser
-- よくわかっていないので、放っておくことにする。しばらくはstdin/stdoutを使う。
-- 並列処理とかできればもっと嬉しい。
data Options 
    = Options {
    inputPath :: [FilePath]
    } deriving (Show)

parserInputPath :: OPT.Parser FilePath
parserInputPath
    = OPT.many 
        $ OPT.strArgument 
        $ mconcat [
        OPT.help "The path to the input file",
        OPT.metavar "input_path",
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
        OPT.progDesc "Kainoki Tree Checker"
        ]

{-
    ======
    Actual Job
    ======
-}
checkIPMAT :: [KaiT] -> Bool
checkIPMAT li = check' $ rootLabel <$> li
    where
        check' :: [KaiCat] -> Bool
        check' (x:xs)
            = case x of
                Kai.KaiCat [] _ _ -> False
                Kai.KaiCat ("VB":_) _ _ -> True
                Kai.KaiCat ("ADJI":_) _ _ -> True
                Kai.KaiCat _ _ _ -> check' xs

check :: KaiT -> Bool
check (PT.Tree rootLabel@(Kai.KaiCat catlist iched sortinfo) subForest)
    = case checkLocal of 
        _ -> checkLocal
    where
        checkLocal :: Bool
        checkLocal = case catlist of
            "IP":_ -> checkIPMAT subForest
            _ -> True

parseDoc :: DT.Text -> IO [KaiT]
parseDoc text
    = case runParserDoc "<STDIN>" text of
        Left errors
            -> DTIO.putStrLn (
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
        >>= mapM_ 
            (
                DTIO.putStrLn 
                . DTL.pack
                . show
                . check
            )