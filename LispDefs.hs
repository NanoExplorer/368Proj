module LispDefs where

import Data.IORef

data LispVal = Atom String
             | Llist [LispVal]
             | DottedList [LispVal] LispVal --NYI
             | Number Integer
             | Lstring String
             | Lbool Bool
             | Lfloat Double --NYI
             | CHARizard Char
             | PrimitiveFunc ([LispVal] -> LispVal)
             | Func { params :: [String], vararg :: (Maybe String),
                      body :: [LispVal], closure :: Env }

instance Show LispVal where show = showVal

showVal :: LispVal -> String
showVal (Atom x) = x
showVal (Llist x) = "(" ++ (showLispList x) ++ ")"
showVal (DottedList head tail) = "(" ++ showLispList head ++ "." ++ showVal tail ++ ")"
showVal (Number x) = show x
showVal (Lstring x) = "\"" ++ x ++ "\""
showVal (Lbool True) = "#t"
showVal (Lbool False) = "#f"
showVal (Lfloat x) = show x
showVal (CHARizard x) = "#\\" ++ showLChar x
showVal (PrimitiveFunc _) = "<primitive>"
showVal (Func {params = args, vararg = varargs, body = body, closure = env}) =
    "(lambda (" ++ unwords (map show args) ++ 
       (case varargs of
          Nothing -> ""
          Just arg -> " . " ++ arg) ++ ") ...)"

showLChar :: Char -> String
showLChar '\n' = "newline"
showLChar ' '  = "space"
showLChar x = [x]

showLispList :: [LispVal] -> String
showLispList [] = "()"
showLispList (x:[]) = showVal x
showLispList (x:xs) = showVal x ++ " " ++ showLispList xs
--showLispList = unwords . map showVal is their implementation. I did it this way.

type Env = IORef [(String, IORef LispVal)] --maps strings (variable names) to a "mutable-ish" LispVal
