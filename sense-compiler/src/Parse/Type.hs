module Parse.Type
  ( senseType,
  )
where

import Data.Identifier
import Data.SType
import Parse.Identifier
import Parse.Parser

senseType :: Parser SType
senseType = do
  -- TODO: Handle | (OR) type later
  -- TODO: Handle SMap type later
  array <|> optional <|> tupple <|> sType
  where
    array :: Parser SType
    array = do
      char '['
      sType <- sType
      char ']'
      return SArray {arrType = sType}

    optional :: Parser SType
    optional = do
      sType <- sType
      char '?'
      return SOptional {maybeType = sType}

    tupple :: Parser SType
    tupple = do
      char '('
      fstType <- sType
      char ','
      sndType <- sType
      char ')'
      return STuple {fstType = fstType, sndType = sndType}

sType :: Parser SType
sType = primitive . idVal <$> uppercaseIdentifier

primitive :: String -> SType
primitive "Bool" = SBool
primitive "String" = SString
primitive "Int" = SInt
primitive "Decimal" = SDecimal
primitive "Unit" = SUnit
primitive "Date" = SDate
primitive "DateTime" = SDateTime
primitive s = SDataRef {typeName = s}
