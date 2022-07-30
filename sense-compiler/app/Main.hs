module Main where

import Compile.SenseCompiler
import Data.CompileError
import Parse.SenseParser
import Validate.SenseValidator

demoFilePath :: String
demoFilePath = "sample-data/Demo.sense"

loadTestData :: IO String
loadTestData = readFile demoFilePath

compileDemo :: IO String
compileDemo = compileFile demoFilePath

main :: IO ()
main = putStrLn "Hello"

compileFile :: FilePath -> IO String
compileFile path = do
  senseFile <- readFile path
  return $ compile senseFile

compile :: String -> String
compile s = do
  case validateSense . parseProgram $ s of
    Left err -> errMsg err
    Right program -> compileSense program