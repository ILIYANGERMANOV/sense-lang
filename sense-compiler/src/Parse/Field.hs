module Parse.Field
  ( senseField,
  )
where

import Data.Char (isUpper)
import Data.Identifier
import Data.SField
import Data.SType
import Parse.Identifier
import Parse.Parser
import Parse.Type

{-
    field1: String
-}
senseField :: Parser SField
senseField = do
  tab
  identifier <- lowercaseIdentifier
  fieldDelimiter
  fType <- senseType
  newline
  return
    SField
      { fId = identifier,
        fType = fType
      }
  where
    fieldDelimiter :: Parser Char
    fieldDelimiter = do
      char ':'
      space
