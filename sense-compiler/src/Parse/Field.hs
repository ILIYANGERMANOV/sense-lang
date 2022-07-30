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

{-
    field1: String
-}
senseField :: Parser SField
senseField = do
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
  -- TODO: Handle SMap type later
  array <|> optional <|> tupple <|> senseType
  where
    array :: Parser SType
    array = do
      char '['
      sType <- senseType
      char ']'
      return SArray {arrType = sType}

    optional :: Parser SType
    optional = do
      sType <- senseType
      char '?'
      return SOptional {maybeType = sType}

    tupple :: Parser SType
    tupple = do
      char '('
      fstType <- senseType
      char ','
      sndType <- senseType
      char ')'
      return STuple {fstType = fstType, sndType = sndType}

senseType :: Parser SType
senseType = primitive . idVal <$> uppercaseIdentifier

primitive :: String -> SType
primitive "Bool" = SBool
primitive "String" = SString
primitive "Int" = SInt
primitive "Decimal" = SDecimal
primitive "Unit" = SUnit
primitive "Date" = SDate
primitive "DateTime" = SDateTime
primitive s = SDataRef {typeName = s}
