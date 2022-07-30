module Main where

import Compile.SenseCompiler
import Parse.SenseParser
import Validate.SenseValidator
import Data.CompileError

loadTestData :: IO String
loadTestData = readFile "sample-data/Demo.sense"

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