module Main where

import qualified ABCCategory as ABCC
import System.IO

a :: ABCC.ABCCategory
a = ABCC.createBase "A" ABCC.<<- ABCC.createBase "B"
b = ABCC.createBase "B" ABCC.<<- ABCC.createBase "C"

main :: IO String
main = 
    print (a ABCC.->> b)
    >> print ABCC.createBot
    >> print a
    >> print (ABCC.reduceWithComment a b)
    >> interactive
    where
        interactive :: IO String
        interactive
            = putStr "Cat Parse >> "
                >> hFlush stdout
                >> getLine
                >>= \str ->
                    case ABCC.parse (str :: String) of
                        Left x -> putStrLn ("\n" ++ show x)
                        Right res -> putStrLn (show res)
                    >> hFlush stdout
                    >> isEOF
                    >>= \eof ->
                        if eof
                            then return ""
                            else interactive


