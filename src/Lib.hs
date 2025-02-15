module Lib (
    someFunc,
)
where

f :: String -> String
f = (++ "Hello")

someFunc :: IO ()
someFunc = putStrLn "someFunc"
