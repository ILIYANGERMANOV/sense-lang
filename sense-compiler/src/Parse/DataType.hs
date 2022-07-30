module Parse.DataType(
  dataType
) where

import Data.Char (isUpper)
import Parse.Parser
import Data.SData
import Parse.Identifier
import Data.Identifier
import Parse.Field


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
  id <- dataDeclartion
  char '('
  newline
  fields <- oneOrMany senseField
  char ')'
  return
    SData
      { dId = id,
        dFields = fields
      }

simpleData :: Parser SData
simpleData = do
  id <- dataDeclartion
  return
    SData
      { dId = id,
        dFields = []
      }

dataDeclartion :: Parser Identifier
dataDeclartion = do
  string "data"
  space
  uppercaseIdentifier