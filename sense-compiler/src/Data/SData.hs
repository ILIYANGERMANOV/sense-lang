module Data.SData where

import Data.Identifier (Identifier)
import Data.SField

data SData = SData
  { dId :: Identifier,
    dFields :: [SField]
  }
  deriving (Show)
