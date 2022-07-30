module Parse.Field where

import Data.Char (isUpper)
import Parse.Parser
import Data.SType
import Data.SField
import Parse.Identifier
import Data.Identifier

{-
    field1: String
-}
fieldParser :: Parser SField
fieldParser = do
  tab
  identifier <- lowercaseIdentifier
  fieldDelimiter
  fType <- fieldType
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

fieldType :: Parser SType
fieldType = do
  -- TODO: Handle | (OR) type later
  -- TODO: Handle Tupple
  array <|> optional <|> parseSType
  where
    array :: Parser SType
    array = do
      char '['
      sType <- parseSType
      char ']'
      return SArray {arrType = sType}

    optional :: Parser SType
    optional = do
      sType <- parseSType
      char '?'
      return SOptional {maybeType = sType}

parseSType :: Parser SType
parseSType = primitive . idVal <$> uppercaseIdentifier

primitive :: String -> SType
primitive "Bool" = SBool
primitive "String" = SString
primitive "Int" = SInt
primitive "Decimal" = SDecimal
primitive "Unit" = SUnit
primitive "Date" = SDate
primitive "DateTime" = SDateTime
primitive s = SDataRef {typeName = s}
