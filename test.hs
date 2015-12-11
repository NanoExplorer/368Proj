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

-}
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
import Numeric (readInt)
import Data.Char (digitToInt)
import Control.Monad.Error


data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal --NYI
             | Number Integer
             | Lstring String
             | Lbool Bool
             | Lfloat Double --NYI
             | Lchar Char
instance Show LispVal where show = showVal

eval :: LispVal -> LispVal
eval a@(Atom _)   = a
eval a@(Lstring _) = a
eval a@(Lbool _)  = a
eval a@(Number _) = a
eval a@(Lfloat _) = a
eval a@(Lchar _)  = a
eval (List [Atom "quote", val]) = val
eval (List (Atom fun:args)) = apply fun $ map eval args

apply :: String -> [LispVal] -> LispVal
apply func args = maybe (error $ func ++ " is not a procedure") ($ args) $ lookup func primitives

primitives :: [(String, [LispVal] -> LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("car", lispCar),
              ("cdr", lispCdr),
              ("symbol?",oneArg testSymbol),
              ("string?",oneArg testString),
              ("number?",oneArg testNumber),
              ("print", oneArg lispPrint),
              ("list", List),
              ("cons", lispCons)]

--A PRIORI CODE (code that I didn't get from the website, I made myself (Christopher))--
lispCons :: [LispVal] -> LispVal
lispCons (x:ys:[]) = List (x:ys)
lispCons _ = error "Contract Violation"

lispCar :: [LispVal] -> LispVal
lispCar ((List (x:xs)):[]) = x
lispCar _ = error "Contract Violation"

lispCdr :: [LispVal] -> LispVal
lispCdr ((List (x:xs)):[]) = List xs
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


--END A PRIORI CODE--

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Number $ foldl1 op $ map unpackNum params

unpackNum :: LispVal -> Integer 
unpackNum (Number n) = n
unpackNum _ = error "Not a number"

showVal :: LispVal -> String
showVal (Atom x) = x
showVal (List x) = "(" ++ (showLispList x) ++ ")"
showVal (Number x) = show x
showVal (Lstring x) = "\"" ++ x ++ "\""
showVal (Lbool True) = "#t"
showVal (Lbool False) = "#f"
showVal (Lfloat x) = show x
showVal (Lchar x) = "#\\" ++ showLChar x

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
              then print "Exiting."
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
                          "newline" -> Lchar '\n'
                          "space"   -> Lchar ' '
                          _         -> Lchar (x !! 0)

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
parseList = liftM List $ sepBy parseExpr spaces

parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]
