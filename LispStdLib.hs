module LispStdLib where
import Control.Monad
import LispDefs
import Numeric (readInt)
import Data.Char (digitToInt)
import Data.IORef

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

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Number $ foldl1 op $ map unpackNum params

unpackNum :: LispVal -> Integer 
unpackNum (Number n) = n
unpackNum _ = error "Not a number"

--A PRIORI CODE (code that I didn't get from the website, I made myself (Christopher))--

testList, stringlist, liststring :: LispVal -> LispVal
testList (Llist _) = Lbool True
testList _ = Lbool False


stringlist (Lstring x) = Llist $ map CHARizard x
stringlist _ = error "contract violation"

liststring (Llist x) = Lstring $ map unpackChar x
liststring _ = error "contract violation"          

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


--This lispCond implementation was not totally without the source material !!!!!!!!!!!!
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

unpackBool :: LispVal -> Bool
unpackBool (Lbool b) = b
unpackBool _ = error "Not a boolean"

unpackChar :: LispVal -> Char 
unpackChar (CHARizard c) = c
unpackChar badThings =  error $ "Not a char: " ++ (show badThings)

unpackString :: LispVal -> String
unpackString (Lstring s) = s
unpackString (Atom a) = a
unpackString _ = error "Not a string"

--END A PRIORI CODE: David
