module Data.SVariable where

import Data.Identifier (Identifier (Identifier))
import Data.SType (SType)

data SVariable
  = SLet {letId :: Identifier, letType :: SType}
  | SState {stateId :: Identifier, stateType :: SType}
  deriving Show