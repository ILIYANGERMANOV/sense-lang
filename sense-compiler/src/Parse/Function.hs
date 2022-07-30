module Parse.Function
  ( function,
  )
where

import Data.SFunction (SFunction (..))
import Data.SStatement (SCase (SCase, argId, argType), SCaseStatement (SCaseStatement, scCase, scStm), SStatement (FunctionCall, callArgs, callId))
import Data.SType (SType (SUnit))
import GHC.TypeLits (ErrorMessage (ShowType))
import Parse.Identifier (lowercaseIdentifier)
import Parse.Parser (Parser, char, newline, oneOrMany, space, string, (<|>), takeUntil)
import Parse.Type
import Parse.Type (senseType)

{-
hello :: Unit
> print("Hello, world!")
-}
function :: Parser SFunction
function = do
  fId <- lowercaseIdentifier
  string " :: "
  fSignature <- signatureTypes
  newline
  csStms <- oneOrMany caseStatement
  return SFunction {fId = fId, fSignature = fSignature, fStms = csStms}

signatureTypes :: Parser (SType, SType)
signatureTypes = inputAndOutput <|> outputOnly
  where
    inputAndOutput :: Parser (SType, SType)
    inputAndOutput = do
      inputType <- senseType
      string " -> "
      outputType <- senseType
      return (inputType, outputType)

    outputOnly :: Parser (SType, SType)
    outputOnly = do
      returnType <- senseType
      return (SUnit, returnType)

caseStatement :: Parser SCaseStatement
caseStatement = do
  sCase <- sCase
  space
  stm <- statement
  return SCaseStatement {scCase = sCase, scStm = stm}

sCase :: Parser SCase
sCase = argAndType <|> typeOnly <|> defaultCase
  where
    argAndType :: Parser SCase
    argAndType = do
      argId <- lowercaseIdentifier
      char ':'
      argType <- senseType
      return SCase {argId = Just argId, argType = argType}

    typeOnly :: Parser SCase
    typeOnly = do
      sType <- senseType
      return SCase {argId = Nothing, argType = sType}

    defaultCase :: Parser SCase
    defaultCase = do
      char '>'
      return SCase {argId = Nothing, argType = SUnit}

statement :: Parser SStatement
statement = functionCall
  where
    functionCall :: Parser SStatement
    functionCall = do
      fId <- lowercaseIdentifier
      char '('
      callArgs <- takeUntil ')'
      char ')'
      return FunctionCall {callId = fId, callArgs = callArgs}