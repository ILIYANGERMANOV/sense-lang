module Validate.SenseValidator where

import Data.CompileError (CompileError)
import Data.SThing

validateSense :: [SThing] -> Either CompileError [SThing]
validateSense = Right