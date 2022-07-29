module TypeSystem where

import Data.Char (isUpper)
import Field
import Identifier (Identifier (idVal), uppercaseIdentifier)
import Parser

data SData = SData
  { dataName :: String,
    fields :: [SField]
  }
  deriving (Show)

dataKeyword :: String
dataKeyword = "data"

{-
data Typename(
    field1: String
    field2: Int
    fieldN: SPrimitive
)
-}
dataType :: Parser SData
dataType = do
  senseRecord <|> simpleData

senseRecord :: Parser SData
senseRecord = do
  typeName <- dataDeclartion
  char '('
  newline
  fields <- consecutiveNonEmpty fieldParser
  char ')'
  return
    SData
      { dataName = typeName,
        fields = fields
      }

simpleData :: Parser SData
simpleData = do
  typeName <- dataDeclartion
  return
    SData
      { dataName = typeName,
        fields = []
      }

dataDeclartion :: Parser String
dataDeclartion = do
  string "data"
  space
  idVal <$> uppercaseIdentifier