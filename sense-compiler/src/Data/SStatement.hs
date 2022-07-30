module Data.SStatement where

import Data.Identifier
import Data.SType (SType)
import Data.SVariable (SVariable)

data SCaseStatement = SCaseStatement {scCase :: SCase, scStm :: SStatement}
  deriving (Show)

data SStatement
  = DeclareVariable {decVar :: SVariable}
  | SetState {setId :: Identifier, setVal :: String}
  | FunctionCall {callId:: Identifier, callArgs :: String}
  deriving (Show)

data SCase = SCase {argId :: Maybe Identifier, argType :: SType}
  deriving (Show)