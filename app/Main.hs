module Main where

import Text.Parsec

data StructDef = ConstructorOnly ConstructorDef Rules | Complex [StructDef]

data ConstructorDef = Unqualified BareStruct | Qualified Qualifier BareStruct

data BareStruct = BareStruct [StructField] | ValueOnly BareValue

data BareValue = BareValue String String -- value and type symbol

data StructField = StructField String StructDef Struct

data Qualifier = Qualifier String

data Struct = Struct Qualifier BareStruct

type Rules = [Rule]

data Rule = Rule Capture StructDef

data Capture = ByBind Struct | ByConditional [Conditional]

data Conditional = Conditional Struct RelationalOperator Struct

data RelationalOperator = Req | Rge | Rg | Rle | Rl | Rne

data SingletonStructDef = SinInt | SinString

main :: IO ()
main = putStrLn "Hello, Haskell!"
