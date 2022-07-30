module Data.Identifier where

newtype Identifier = Identifier
  { idVal :: String
  }
  deriving (Show)
