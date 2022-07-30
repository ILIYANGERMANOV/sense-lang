module Parse.SenseParser
  ( parseProgram,
  )
where

import Data.SThing
import Parse.DataType
import Parse.Function (function)
import Parse.Parser (Parser, newline, parse, zeroOrMany, (<|>))

parseProgram :: String -> [SThing]
parseProgram = fst . head <$> parse senseParser

senseParser :: Parser [SThing]
senseParser = zeroOrMany $ parseData <|> parseFunction <|> emptyLine
  where
    parseData :: Parser SThing
    parseData = do
      sData <- dataType
      return TData {tData = sData}

    parseFunction :: Parser SThing
    parseFunction = do
      sFunction <- function
      return TFunction {tFunction = sFunction}

    emptyLine :: Parser SThing
    emptyLine = do
      newline
      return TEmptyLine