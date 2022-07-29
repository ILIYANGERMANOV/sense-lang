module Field where

import Data.Char (isUpper)
import Identifier (Identifier (idVal), lowercaseIdentifier, uppercaseIdentifier)
import Parser

data SPrimitive
  = SInt
  | SDecimal
  | SBool
  | SString
  | SUnit
  | SArray {arrType :: SPrimitive}
  | SOptional {maybeType :: SPrimitive}
  | STuple {fst :: SPrimitive, snd :: SPrimitive}
  | SOr {orFst :: SPrimitive, orSnd :: SPrimitive}
  | SDataRef {typeName :: String}
  deriving (Show)

data SField = SField
  { fId :: Identifier,
    fType :: SPrimitive
  }
  deriving (Show)

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

fieldType :: Parser SPrimitive
fieldType = do
  -- TODO: Handle | (OR) type later
  -- TODO: Handle Tupple
  array <|> optional <|> parsePrimitive
  where
    array :: Parser SPrimitive
    array = do
      char '['
      sPrimitive <- parsePrimitive
      char ']'
      return SArray {arrType = sPrimitive}

    optional :: Parser SPrimitive
    optional = do
      sPrimitive <- parsePrimitive
      char '?'
      return SOptional {maybeType = sPrimitive}

parsePrimitive :: Parser SPrimitive
parsePrimitive = primitive . idVal <$> uppercaseIdentifier

primitive :: String -> SPrimitive
primitive "Bool" = SBool
primitive "String" = SString
primitive "Int" = SInt
primitive "Decimal" = SDecimal
primitive "Unit" = SUnit
primitive s = SDataRef {typeName = s}
