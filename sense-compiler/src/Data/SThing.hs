module Data.SThing where

import Data.SData (SData)
import Data.SFunction
import Data.STranslator (STranslator)
import Data.SVariable (SVariable)

data SThing
  = TData {sData :: SData}
  | TFunction {sFunction :: SFunction}
  | TTranslator {sTranslator :: STranslator}
  | TVariable {sVariable :: SVariable}
  deriving (Show)