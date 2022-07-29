module Main where


loadTestData :: IO String
loadTestData = readFile "sample-data/SenseRecord.sense"

main :: IO ()
main = putStrLn "Hello"
