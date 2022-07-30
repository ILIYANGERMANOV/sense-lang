module Data.SThing where

import Data.SData (SData)
import Data.SFunction
import Data.STranslator (STranslator)
import Data.SVariable (SVariable)

data SThing
  = TData {tData :: SData}
  | TFunction {tFunction :: SFunction}
  | TTranslator {tTranslator :: STranslator}
  | TVariable {tVariable :: SVariable}
  | TEmptyLine
  deriving (Show)