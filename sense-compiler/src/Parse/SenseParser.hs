module Parse.SenseParser where

import Data.SThing
import Parse.Parser (parse)
import Parse.TypeSystem

parseProgram :: String -> [SThing]
parseProgram s = map (TData . fst) (parse dataType s)