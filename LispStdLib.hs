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
getVar env name = readIORef env >>=  maybe (error $ "No such variable" ++ name) (readIORef) . lookup name


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

--MORE A PRIORI CODE: David
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
