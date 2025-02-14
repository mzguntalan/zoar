module Main where

import Debug.Trace (trace)
import System.IO.Error (isAlreadyExistsError)

data TokenType = Literal | Symbol | Space deriving (Show)

data Token = Token String TokenType deriving (Show)

parseUntilWord :: String -> String -> Maybe String
parseUntilWord (t : ts) (c : cs)
    | c == t = parseUntilWord ts cs
    | otherwise = Nothing
parseUntilWord _ [] = Nothing
parseUntilWord [] corpus = Just corpus

-- isSpace :: String -> Bool
-- isSpace (c : cs)
--     | c `elem` [' ', '\t', '\n'] = isSpace cs
--     | otherwise = False
-- isSpace [] = True

smartTokenizer :: String -> Token
smartTokenizer word
    | all isSymbolChar word = Token word Symbol
    | all isSpaceChar word = Token word Space
    | otherwise = Token word Literal

expectWordAsToken :: String -> String -> Maybe (Token, String)
expectWordAsToken word corpus = case parseUntilWord word corpus of
    Just remaining -> Just (Token word Literal, remaining)
    _ -> Nothing

isSpaceChar :: Char -> Bool
isSpaceChar = (`elem` [' ', '\t', '\n'])

isSymbolChar :: Char -> Bool
isSymbolChar = (`elem` ['=', '|', ',', '"', ':', ';'])

expectByConditionAsToken :: (Char -> Bool) -> String -> Maybe (Token, String)
expectByConditionAsToken condition (c : cs)
    | condition c = f corpus []
    | otherwise = Nothing
  where
    corpus = c : cs
    f :: String -> String -> Maybe (Token, String)
    f (t : ts) toBeToken
        | t `elem` [' ', '\t', '\n'] = trace "it is a space" f ts (t : toBeToken)
        | otherwise = trace "The end" Just (smartTokenizer toBeToken, t : ts)
    f [] (s : ss) = trace "No more corpus" Just (smartTokenizer (reverse (s : ss)), "")
    f [] [] = Nothing
expectByConditionAsToken _ [] = Nothing

expectSpaceAsToken :: String -> Maybe (Token, String)
expectSpaceAsToken = expectByConditionAsToken isSpaceChar

expectSymbolAsToken :: String -> Maybe (Token, String)
expectSymbolAsToken = expectByConditionAsToken isSymbolChar

expectLiteralAsToken :: String -> Maybe (Token, String)
expectLiteralAsToken = expectByConditionAsToken isLiteralChar

isLiteralChar :: Char -> Bool
isLiteralChar c = not (isSpaceChar c) && not (isSymbolChar c)

main :: IO ()
main = putStrLn "Hello, Haskell!"
