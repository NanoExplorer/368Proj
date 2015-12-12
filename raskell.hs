-- Parsec is kind of hard to get the hang of. So far most of this entire file has come straight from the wikibooks on Write Yourself a Scheme
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



Things implemented: *+-/ car cdr
all of the lispval primitives but dottedlist, in show and eval


-}
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
import Numeric (readInt)
import Data.Char (digitToInt)
import Control.Monad.Error

data LispVal = Atom String
             | Llist [LispVal]
             | DottedList [LispVal] LispVal --NYI
             | Number Integer
             | Lstring String
             | Lbool Bool
             | Lfloat Double --NYI
             | CHARizard Char

instance Show LispVal where show = showVal

eval :: LispVal -> LispVal
eval a@(Atom _)    = a
eval a@(Lstring _) = a
eval a@(Lbool _)   = a
eval a@(Number _)  = a
eval a@(Lfloat _)  = a
eval a@(CHARizard _)   = a
eval (Llist [Atom "quote", val]) = val
eval (Llist (Atom fun:args)) = apply fun $ map eval args


apply :: String -> [LispVal] -> LispVal
apply func args = maybe (error $ func ++ " is not a procedure") ($ args) $ lookup func primitives

primitives :: [(String, [LispVal] -> LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("<", lispLessThan), --start new functions
              (">", lispGreaterThan),
              (">=", lispGtEqTo),
              ("<=", lispLtEqTo),
              ("equal?", lispEqualMa), --end new functions
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("car", lispCar),
              ("cdr", lispCdr),
              ("symbol?",oneArg testSymbol),
              ("string?",oneArg testString),
              ("number?",oneArg testNumber),
              --("char?",oneArg testChar),
              ("print", oneArg lispPrint),
              ("list", Llist),
              ("cons", lispCons),
              ("null?", lispNullMa)]

--A PRIORI CODE (code that I didn't get from the website, I made myself (Christopher))--

lispCar :: [LispVal] -> LispVal
lispCar ((Llist (x:xs)):[]) = x
lispCar _ = error "Contract Violation"

lispCdr :: [LispVal] -> LispVal
lispCdr ((Llist (x:xs)):[]) = Llist xs
lispCdr _ = error "Contract Violation"

oneArg :: (LispVal -> LispVal) -> [LispVal] -> LispVal
oneArg func (x:[]) = func x
oneArg _ _ = error "Too many arguments"

testSymbol, testString, testNumber, lispPrint :: LispVal -> LispVal
testSymbol (Atom _) = Lbool True
testSymbol _ = Lbool False

testString (Lstring _) = Lbool True
testString _ = Lbool False

testNumber (Number _) = Lbool True
testNumber _ = Lbool False

lispPrint s = Lstring (showVal s)

--A PRIORI CODE (David)--
lispCons :: [LispVal] -> LispVal
lispCons [x, Llist ys] = Llist $ x:ys
lispCons _ = error "Contract Violation"

lispNullMa :: [LispVal] -> LispVal
lispNullMa [Llist []] = Lbool True
lispNullMa _ = Lbool False

lispLessThan :: [LispVal] -> LispVal
lispLessThan [x, y] = Lbool $ (unpackNum x) < (unpackNum y)

lispGreaterThan :: [LispVal] -> LispVal
lispGreaterThan [x, y] = Lbool $ (unpackNum x) > (unpackNum y)

lispGtEqTo :: [LispVal] -> LispVal
lispGtEqTo [x, y] = Lbool $ (unpackNum x) >= (unpackNum y)

lispLtEqTo :: [LispVal] -> LispVal
lispLtEqTo [x, y] = Lbool $ (unpackNum x) <= (unpackNum y)

lispEqualMa :: [LispVal] -> LispVal
lispEqualMa [(Number x), (Number y)]         = Lbool $ x == y
lispEqualMa [(CHARizard x), (CHARizard y)]   = Lbool $ x == y
--lispEqualMa [Llist (x:xs), Llist (y:ys)] = Lbool $ (x == y) && (unpackBool $ lispEqualMa [(Llist xs),(Llist ys)])
lispEqualMa _ = error "Contract Violation"

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


showLChar :: Char -> String
showLChar '\n' = "newline"
showLChar ' '  = "space"
showLChar x = [x]

showLispList :: [LispVal] -> String
showLispList (x:[]) = showVal x
showLispList (x:xs) = showVal x ++ " " ++ showLispList xs
--showLispList = unwords . map showVal is their implementation. I did it this way.

--more a priori code--
main :: IO ()
main = do x <- getLine
          if x == "(exit)"
              then putStrLn "Exiting."
              else rep x >> main
--end--

rep :: String -> IO()
rep = print . eval . readExpr 

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
