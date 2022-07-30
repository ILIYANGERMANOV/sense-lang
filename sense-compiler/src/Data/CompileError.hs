module Data.CompileError where

newtype CompileError = CompileError
  { errMsg :: String
  }