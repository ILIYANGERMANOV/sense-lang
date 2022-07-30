module Data.SType where

import Data.Identifier

data SType
  = SInt
  | SDecimal
  | SBool
  | SString
  | SUnit
  | SDate
  | SDateTime
  | SMap {mKey :: SType, mVal :: SType}
  | SArray {arrType :: SType}
  | SOptional {maybeType :: SType}
  | STuple {_fst :: SType, s_nd :: SType}
  | SOr {orFst :: SType, orSnd :: SType}
  | SDataRef {typeName :: String}
  deriving (Show)