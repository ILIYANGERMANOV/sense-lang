module Parse.Identifier
  ( 
    uppercaseIdentifier,
    lowercaseIdentifier
  )
where

import Data.Char (isAlphaNum, isUpper, isAlpha)
import Parse.Parser
  ( Parser,
    char,
    charIn,
    consecutive,
    consecutiveNonEmpty,
    newline,
    sat,
    space,
    (<|>),
  )
import Data.Identifier

uppercaseIdentifier :: Parser Identifier
uppercaseIdentifier = do
  c <- sat isUpper
  cs <- consecutive identifierSymbol
  return Identifier {idVal = c : cs}

lowercaseIdentifier :: Parser Identifier
lowercaseIdentifier = do
  c <- sat (\c -> isAlpha c && (not . isUpper $ c))
  cs <- consecutive identifierSymbol
  return Identifier {idVal = c : cs}

identifierSymbol :: Parser Char
identifierSymbol = sat isAlphaNum <|> charIn allowedSpecial

allowedSpecial :: [Char]
allowedSpecial = ['\'', '_']
