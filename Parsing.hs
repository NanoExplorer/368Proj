module Parsing where

import Text.ParserCombinators.Parsec hiding (spaces)
import LispDefs
import Control.Monad

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

readExpr :: String -> [LispVal]
readExpr input = case parse parseManyExpr "lisp" input of
    Left err -> [Lstring $ "No match: " ++ show err]
    Right val -> val

spaces :: Parser ()
spaces = skipMany1 (space <|> newline <|> tab)

parseManyExpr :: Parser [LispVal]
parseManyExpr = sepBy parseExpr spaces
 
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
