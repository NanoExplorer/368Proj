-- Parsec is kind of hard to get the hang of. So most of the functions that 
-- use parsec are heavily guided by the wikibook:
-- I tried to do the exercises at least for the first two or three chapters
-- without looking at the answers.
-- https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours/Answers

import System.Environment
import System.IO
import Control.Monad
import Data.IORef
import LispDefs
import Parsing
import LispStdLib

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

main :: IO ()
main = do args <- getArgs
          case length args of 
             0 -> runRepl
             1 -> loadRun $ args !! 0
             otherwise -> putStrLn "Invalid number of arguments"

newEnv :: IO Env
newEnv = do x <- nullEnv
            bindVars x $ map (\(x, y) -> (x, PrimitiveFunc y)) primitives

runRepl = newEnv >>= until_ (=="(exit)") (readPrompt "Lisp>>> ") . rep

rep :: Env -> String -> IO()
rep env command = sequence (map ((liftM show) . (eval env)) (readExpr command)) >>= (\x -> sequence_ [putStrLn i | i<-x])

loadRun :: String -> IO()
loadRun filename = do 
  handle <- openFile filename ReadMode
  contents <- hGetContents handle
  env <- newEnv
  rep env contents
  hClose handle
  until_ (=="(exit)") (readPrompt "Lisp>>> ") (rep env)
  return ()

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

until_ :: Monad m => (a->Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
  result <- prompt
  if pred result
     then return()
     else action result >> until_ pred prompt action



--rep _ command = print $ readExpr command 
--parser debug

{-
(cons 'a '(a b c))
(a a b c)

(cons '(a b c) 'a)
((a b c) . a)
-}

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
