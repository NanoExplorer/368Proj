{- This file contains 
the two data types LispVal and Env, and most of their supporting functionality.

-}

module LispDefs where

import Data.IORef
import Control.Monad



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

bindVars :: Env -> [(String,LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
    where extendEnv bindings env = liftM (++ env) (mapM addBinding bindings)
          addBinding (var, value) = do ref <- newIORef value
                                       return (var,ref)

setVar :: Env -> String -> LispVal -> IO LispVal 
setVar envRef var value = do env <- readIORef envRef
                             maybe (error "Set an unbound variable")
                                   (flip writeIORef value)
                                   (lookup var env)
                             return value

nullEnv :: IO Env
nullEnv = newIORef []


--Christopher A Priori Code--

varExists :: Env -> String -> IO Bool
varExists env name = readIORef env >>= return . maybe False (const True) . lookup name
--here I got the const function from the wikibook. Apparently it makes any value into a function that takes one
--argument and always returns that value -> const = \x y -> x so (const True) = \y -> True

getVar :: Env -> String -> IO LispVal
getVar env name = readIORef env >>=  maybe (error $ "No such variable" ++ name) (readIORef) . lookup name


--Based heavily on setVar, but without looking at recommended defineVar definitions
defineVar :: Env -> String -> LispVal -> IO LispVal --I got the type signature from the book
defineVar envRef name value = do env <- readIORef envRef
                                 maybe (do newRef <- newIORef value
                                           modifyIORef envRef ((name, newRef):))
                                       (flip writeIORef value)
                                       (lookup name env)
                                 return value


