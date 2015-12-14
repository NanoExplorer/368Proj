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

import System.Environment
import System.IO
import Control.Monad
import Numeric (readInt)
import Data.Char (digitToInt)
import Data.IORef
import LispDefs
import Parsing

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
eval env (Llist (Atom "cond" : alts)) = lispCond env alts
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


setVar :: Env -> String -> LispVal -> IO LispVal 
setVar envRef var value = do env <- readIORef envRef
                             maybe (error "Set an unbound variable")
                                   (flip writeIORef value)
                                   (lookup var env)
                             return value




numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Number $ foldl1 op $ map unpackNum params

unpackNum :: LispVal -> Integer 
unpackNum (Number n) = n
unpackNum _ = error "Not a number"




main :: IO ()
main = do args <- getArgs
          case length args of 
             0 -> runRepl
             1 -> loadRun $ args !! 0
             otherwise -> putStrLn "Invalid number of arguments"

loadRun :: String -> IO()
loadRun filename = do 
  handle <- openFile filename ReadMode
  contents <- hGetContents handle
  env <- newEnv
  rep env contents
  hClose handle
  until_ (=="(exit)") (readPrompt "Lisp>>> ") (rep env)
  return ()

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
rep env command = sequence (map ((liftM show) . (eval env)) (readExpr command)) >>= (\x -> sequence_ [putStrLn i | i<-x])

--rep _ command = print $ readExpr command 
--parser debug

{-
(cons 'a '(a b c))
(a a b c)

(cons '(a b c) 'a)
((a b c) . a)
-}
