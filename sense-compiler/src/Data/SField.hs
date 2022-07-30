module Data.SField where

import Data.Identifier
import Data.SType

data SField = SField
  { fId :: Identifier,
    fType :: SType
  }
  deriving (Show)
