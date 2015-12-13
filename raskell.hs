-- Parsec is kind of hard to get the hang of. So most of the functions that 
-- use parsec are heavily guided by the wikibook:
-- I tried to do the exercises at least for the first two or three chapters
-- without looking at the answers.
-- https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours/Answers

{-
Prelude> sequence_ [print i | i <- [1..10]]
1
2
3
4
5
6
7
8
9
10
Prelude> 

-}
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import System.IO
import Control.Monad
import Numeric (readInt)
import Data.Char (digitToInt)
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

eval :: Env -> LispVal -> IO LispVal
eval env (Atom id)       = getVar env id
eval _ a@(Lstring _)     = return a
eval _ a@(Lbool _)       = return a
eval _ a@(Number _)      = return a
eval _ a@(Lfloat _)      = return a
eval _ a@(CHARizard _)   = return a
eval env (Llist [Atom "define", Atom var, value]) = eval env value >>= defineVar env var 
eval env (Llist [Atom "set!", Atom var, value])   = eval env value >>= setVar env var 
eval env (Llist [Atom "quote", val]) = return val
eval env (Llist [Atom "cond", Llist alts]) = lispCond env alts
eval env (Llist [Atom "if", pred, conseq, alt]) =
     do result <- eval env pred
        case result of
          Lbool False -> eval env alt
          otherwise -> eval env conseq
eval env (Llist (Atom "lambda" : Llist params : funbody)) = return $ Func (map showVal params) Nothing funbody env

eval env (Llist (fun:args)) = do 
        func <- eval env fun
        argVals <- mapM (eval env) args 
        apply func argVals

apply :: LispVal -> [LispVal] -> IO LispVal
apply (PrimitiveFunc func) args = return $ func args
apply (Func params varargs body closure) args = 
      if num params /= num args && varargs == Nothing
         then error "Wrong number of arguments"
         else bindVars closure (zip params args) >>= bindVarArgs varargs >>= evalBody
      where remainingArgs = drop (length params) args
            num = toInteger . length
            evalBody env = liftM last $ mapM (eval env) body
            bindVarArgs arg env = case arg of
                                    Just argName -> bindVars env [(argName, Llist $ remainingArgs)]
                                    Nothing -> return env


bindVars :: Env -> [(String,LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
    where extendEnv bindings env = liftM (++ env) (mapM addBinding bindings)
          addBinding (var, value) = do ref <- newIORef value
                                       return (var,ref)


primitives :: [(String, [LispVal] -> LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("<", comparisonOps (<)),
              (">", comparisonOps (>)),
              (">=", comparisonOps (>=)),
              ("<=", comparisonOps (<=)),
              ("equal?", testEqual),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("car", lispCar),
              ("cdr", lispCdr),
              ("symbol?",oneArg testSymbol),
              ("string?",oneArg testString),
              ("number?",oneArg testNumber),
              ("char?",oneArg testChar),
              ("print", oneArg lispPrint),
              ("list", Llist),
              ("cons", lispCons),
              ("null?",oneArg testNull),
              ("list?",oneArg testList),
              ("string->list", oneArg stringlist),
              ("list->string", oneArg liststring)]

type Env = IORef [(String, IORef LispVal)] --maps strings (variable names) to a "mutable-ish" LispVal
setVar :: Env -> String -> LispVal -> IO LispVal 
setVar envRef var value = do env <- readIORef envRef
                             maybe (error "Set an unbound variable")
                                   (flip writeIORef value)
                                   (lookup var env)
                             return value
--So the IO LispVal that this function spits out is just the LispVal that you put in.

--A PRIORI CODE (code that I didn't get from the website, I made myself (Christopher))--

testList, stringlist, liststring :: LispVal -> LispVal
testList (Llist _) = Lbool True
testList _ = Lbool False


stringlist (Lstring x) = Llist $ map CHARizard x
stringlist _ = error "contract violation"

liststring (Llist x) = Lstring $ map unpackChar x
liststring _ = error "contract violation"

varExists :: Env -> String -> IO Bool
varExists env name = readIORef env >>= return . maybe False (const True) . lookup name
--here I got the const function from the wikibook. Apparently it makes any value into a function that takes one
--argument and always returns that value -> const = \x y -> x so (const True) = \y -> True

getVar :: Env -> String -> IO LispVal
getVar env name = readIORef env >>=  maybe (error "No such variable") (readIORef) . lookup name


--Based heavily on setVar, but without looking at recommended defineVar definitions
defineVar :: Env -> String -> LispVal -> IO LispVal --I got the type signature from the book
defineVar envRef name value = do env <- readIORef envRef
                                 maybe (do newRef <- newIORef value
                                           modifyIORef envRef ((name, newRef):))
                                       (flip writeIORef value)
                                       (lookup name env)
                                 return value
newEnv :: IO Env
newEnv = do x <- nullEnv
            bindVars x $ map (\(x, y) -> (x, PrimitiveFunc y)) primitives
          

lispCar :: [LispVal] -> LispVal
lispCar ((Llist (x:xs)):[]) = x
lispCar _ = error "Contract Violation"

lispCdr :: [LispVal] -> LispVal
lispCdr ((Llist (x:xs)):[]) = Llist xs
lispCdr _ = error "Contract Violation"

oneArg :: (LispVal -> LispVal) -> [LispVal] -> LispVal
oneArg func (x:[]) = func x
oneArg _ _ = error "Too many arguments"

testSymbol, testString, testNumber :: LispVal -> LispVal
testSymbol (Atom _) = Lbool True
testSymbol _ = Lbool False

testString (Lstring _) = Lbool True
testString _ = Lbool False

testNumber (Number _) = Lbool True
testNumber _ = Lbool False

lispPrint s = Lstring (showVal s) --not really implemented

--A PRIORI CODE (David)--
lispCons :: [LispVal] -> LispVal
lispCons [x, Llist ys] = Llist $ x:ys
lispCons _ = error "Contract Violation"

testNull :: LispVal -> LispVal
testNull (Llist []) = Lbool True
testNull _ = Lbool False

testChar :: LispVal -> LispVal
testChar (CHARizard _) = Lbool True
testChar _ = Lbool False


--This lispCond implementation was not totally without the source material !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
lispCond :: Env -> [LispVal] -> IO LispVal
lispCond env ((Llist (Atom "else" : value : [])) : []) = eval env value
lispCond env ((Llist (condition : value : [])) : alts) = do
     result <- eval env condition
     if (unpackBool result) then eval env value
                   else lispCond env alts
lispCond _ _ = error "No viable alternative in cond"


comparisonOps :: (Integer -> Integer -> Bool) -> [LispVal] -> LispVal
comparisonOps op [(Number x), (Number y)] = Lbool $ x `op` y
comparisonOps op [_, _] = error "Comparison only available for integers"
comparisonOps op _ = error "Contract Violation"

testEqual :: [LispVal] -> LispVal
testEqual [(Number x), (Number y)]         = Lbool $ (x == y)
testEqual [(CHARizard x), (CHARizard y)]   = Lbool $ (x == y)
testEqual [(Lstring x), (Lstring y)]       = Lbool $ (x == y)
testEqual [(Atom x), (Atom y)]             = Lbool $ (x == y)
testEqual [(Lbool x), (Lbool y)]           = Lbool $ (x == y)
testEqual [(Llist []), (Llist [])]         = Lbool True
testEqual [Llist (x:xs), Llist (y:ys)] = Lbool $ ((unpackBool $ testEqual [x, y]) && (unpackBool $ testEqual [Llist xs, Llist ys]))
testEqual [_, _] = Lbool False
testEqual _ = error "Contract Violation"

--END A PRIORI CODE--


numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Number $ foldl1 op $ map unpackNum params

unpackNum :: LispVal -> Integer 
unpackNum (Number n) = n
unpackNum _ = error "Not a number"

--BEGIN A PRIORI CODE: David
unpackBool :: LispVal -> Bool
unpackBool (Lbool b) = b
unpackBool _ = error "Not a boolean"

unpackChar :: LispVal -> Char
unpackChar (CHARizard c) = c
unpackChar _ = error "Not a char"

unpackString :: LispVal -> String
unpackString (Lstring s) = s
unpackString (Atom a) = a
unpackString _ = error "Not a string"

--END A PRIORI CODE: David

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
showLispList (x:[]) = showVal x
showLispList (x:xs) = showVal x ++ " " ++ showLispList xs
--showLispList = unwords . map showVal is their implementation. I did it this way.

main :: IO ()
main = runRepl

nullEnv :: IO Env
nullEnv = newIORef []

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

runRepl = newEnv >>= until_ (=="(exit)") (readPrompt "Lisp>>> ") . rep

until_ :: Monad m => (a->Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
  result <- prompt
  if pred result
     then return()
     else action result >> until_ pred prompt action

rep :: Env -> String -> IO()
rep env command = liftM show (eval env $ readExpr command) >>= putStrLn

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err -> Lstring $ "No match: " ++ show err
    Right val -> val

spaces :: Parser ()
spaces = skipMany1 space

parseExpr :: Parser LispVal
parseExpr = parseAtom
    <|> parseString
    <|> parseNumber
    <|> parseHash
    <|> parseQuoted
    <|> do char '('
           x <- parseList
           char ')'
           return x

parseHash :: Parser LispVal
parseHash = char '#' >> (processBool <|> processChar)

processBool :: Parser LispVal
processBool = do x <- oneOf "tf"
                 return $ case x of
                            't' -> Lbool True
                            'f' -> Lbool False

processChar :: Parser LispVal
processChar = do char '\\'
                 x <- try (string "newline" <|> string "space") 
                     <|> do x <- anyChar;
                            notFollowedBy alphaNum
                            return [x]
                 return $ case x of
                          "newline" -> CHARizard '\n'
                          "space"   -> CHARizard ' '
                          _         -> CHARizard (x !! 0)

parseString :: Parser LispVal
parseString = do
                char '"'
                x <- many (noneOf "\\\"" <|> escapedChars)
                char '"'
                return $ Lstring x

escapedChars :: Parser Char
escapedChars = do
                 char '\\'
                 x <- oneOf "\"\\nrt"
                 return $ case x of
                     'n' -> '\n'
                     't' -> '\t'
                     'r' -> '\r'
                     '\\'-> x
                     '"' -> x

parseAtom :: Parser LispVal
parseAtom = do
              first <- letter <|> symbol
              rest <- many (letter <|> digit <|> symbol)
              let atom = first:rest
              return $ Atom atom

parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit

parseNumber' :: Parser LispVal
parseNumber' = do
                 x <- many1 digit
                 return $ Number $ read x

parseNumber'' :: Parser LispVal
parseNumber'' = (many1 digit) >>= \x -> (return . Number . read) x


parseList :: Parser LispVal
parseList = liftM Llist $ sepBy parseExpr spaces

parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ Llist [Atom "quote", x]

{-
(cons 'a '(a b c))
(a a b c)

(cons '(a b c) 'a)
((a b c) . a)
-}
