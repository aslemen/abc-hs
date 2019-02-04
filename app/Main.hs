module Main where

import qualified ABCCategory as ABCC
import qualified ParsedTree as PT
import System.IO

type ABCT = PT.Tree ABCC.ABCCategory

parseTree :: String -> Either PT.ParseError ABCT
parseTree = PT.createFromString ABCC.parser

readPrompt :: IO String
readPrompt
    = putStr "Tree Parse>>"
        >> hFlush stdout
        >> getLine

change :: ABCT -> ABCT
change (PT.Node node children)
    = PT.Node (ABCC.createBot {ABCC.comment = show node}) (map change children)

parse :: String -> IO()
parse str
    = case parseTree str of
        Left err 
            -> putStrLn ("\n" ++ show err)
        Right res 
            -> print $ change res

interactive :: IO()
interactive
    = readPrompt
        >>= \str ->
            parse str
            >> interactive

main :: IO ()
main = interactive


