module ParsedTree () where

import Text.Parsec
import Control.Applicative ((<$>), (<*>))

data Tree termtype = NonTerminalNode [Tree termtype] | TernimalNode termtype
