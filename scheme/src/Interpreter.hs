{-# OPTIONS_GHC -fwarn-incomplete-patterns -fwarn-incomplete-uni-patterns #-}

module Interpreter where

import ADT
import Parser
import Text.Megaparsec
import System.IO

-------------------------

type Env = [(String, Val)]

data Val = NumVal Integer
         | StrVal String 
         | Cons (Val, Val)
         | Func [String] Expr Env
         | Define String Val
         | PrimOp ([Val] -> Errorable Val)
         | Macro [String] Expr Env

data Error = UndefinedVar String 
           | WrongNumberOfArgs String 
           | WrongSyntax String
           | TypeMismatch
           | AnotherError String
            deriving Show

type Errorable = Either Error

-------------------------

instance Show Val where
    show (NumVal x) = show x
    show (StrVal s) = s
    show (Cons c@_) = "(" ++ showCons c ++ ")" where
        showCons (x, StrVal "nil") = show x
        showCons (x, Cons y) = show x ++ " " ++ showCons y
        showCons (x, y) = show x ++ " " ++ showCons (y, StrVal "nil")
    show (Func _ _ _) = show "function declaration"
    show (Define name _) = "defined " ++ name
    show (PrimOp _) = show "primitive operation"
    show (Macro _ _ _) = show "macro declaration"

-------------------------

repl :: Env -> IO ()
repl env = do
    putStr "scheme> "
    hFlush stdout
    input <- getLine
    case runParser parseExpr "" input of
        Left err -> putStrLn (show err) >> repl env
        Right (Name "quit") -> putStrLn "bye"
        Right expr -> case eval expr env of
            Left err -> putStrLn (show err) >> repl env
            Right val -> putStrLn (show val) >> repl modifiedEnv where
                modifiedEnv = case val of
                    Define name closure -> (name, closure) : env
                    _ -> env

-------------------------

defaultEnv :: Env
defaultEnv = [
    ("+", PrimOp $ multiIntOp (+) 0),
    ("-", PrimOp $ multiIntOp (-) 0),
    ("*", PrimOp $ multiIntOp (*) 1),
    (">", PrimOp $ binaryCompOp (>)),
    (">=", PrimOp $ binaryCompOp (>=)),
    ("<", PrimOp $ binaryCompOp (<)),
    ("<=", PrimOp $ binaryCompOp (<=)),
    ("=", PrimOp $ binaryEqOp),
    ("not", PrimOp $ unaryBoolOp (not)),
    ("and", PrimOp $ binaryBoolOp (&&)),
    ("or", PrimOp $ binaryBoolOp (||)),
    ("car", PrimOp $ carOp),
    ("cdr", PrimOp $ cdrOp),
    ("cons", PrimOp $ consOp),
    ("list", PrimOp $ listOp) ]

-------------------------

eval :: Expr -> Env -> Errorable Val

eval (Num n) _ = return (NumVal n)

eval (Name str@('\"' : _)) _ = return (StrVal str)

eval (List []) _ = return (StrVal "nil")

eval (List [(Name "lambda"), (List formals), body]) env = do
    liftedArgs <- liftName formals
    return (Func liftedArgs body env)

eval (List [(Name "if"), test, consequent, alternate]) env = do
    cond <- eval test env
    case cond of 
        (StrVal "#t") -> eval consequent env
        (StrVal "#f") -> eval alternate env
        _ -> Left (WrongSyntax "if")

eval (List ((Name "cond") : clauses)) env = processCond clauses where
    processCond ((List ((Name "else") : body : [])) : []) = eval body env
    processCond ((List (cond : body : [])) : other) = case eval cond env of
        Right (StrVal "#t") -> eval body env
        Right (StrVal "#f") -> processCond other
        _ -> Left (WrongSyntax "cond")
    processCond _ = Left (WrongSyntax "cond")

eval (List [(Name "define"), (Name var), expr]) env = case expr of
    List [(Name "lambda"), (List formals), body] -> 
        eval (List [(Name "define"), (List (Name var : formals)), body]) env
    _ -> do
        val <- eval expr env
        return (Define var val)

eval (List [(Name "define"), (List (Name var : args)), expr]) env = case liftName args of
    Right liftedArgs -> return (Define var val) where
        val = Func liftedArgs expr modifiedEnv
        modifiedEnv = (var, val) : env
    Left err -> Left err

eval (List [(Name "let"), (List bindings), body]) env = do
    defList <- liftList bindings
    vars <- getVars defList
    vals <- getDefs defList
    evalParams <- checkParams (map (\x -> eval x env) vals)
    eval body ((zip vars evalParams) ++ env) where
        getVars [] = Right []
        getVars ([(Name x), def] : xs) = case getVars xs of
            Right xsLifted -> Right (x : xsLifted)
            Left err -> Left err
        getVars _ = Left (WrongSyntax "let")

        getDefs [] = Right []
        getDefs ([(Name x), def] : xs) = case getDefs xs of
            Right xsLifted -> Right (def : xsLifted)
            Left err -> Left err
        getDefs _ = Left (WrongSyntax "let")

eval (List [(Name "define-syntax-rule"), List ((Name macro) : args), expr]) env = do
    liftedArgs <- liftName args
    return (Define macro (Macro liftedArgs expr env))

eval (Name var) env = case lookup var env of
    Just val -> return val
    Nothing -> Left (UndefinedVar var)

eval (List ((Name f) : params)) env = case lookup f env of
    Just (Func args expr fenv) -> if length params /= length args then Left (WrongNumberOfArgs f) else do
        evalParams <- checkParams (map (\x -> eval x env) params)
        eval expr ((zip args evalParams) ++ fenv)
    Just (PrimOp op) -> do
        evalParams <- checkParams (map (\x -> eval x env) params)
        op evalParams
    Just (Macro args expr menv) -> if length params /= length args then Left (WrongNumberOfArgs f) else do
        evalParams <- processQuasi (List params) env 1
        paramsList <- toList evalParams
        evalExpr <- eval expr ((zip args paramsList) ++ menv)
        uexpr <- unquote evalExpr
        eval uexpr env
    Just _ -> Left (AnotherError "Function couldn't evaluate")
    Nothing -> case f of
        "quote" -> case params of
            [p] -> return (processQuote p)
            _ -> Left (WrongNumberOfArgs "Quote")
        "quasiquote" -> case params of
            [p] -> processQuasi p env 1
            _ -> Left (WrongNumberOfArgs "Quasiquote")
        _ -> Left (UndefinedVar f)

eval (List (lambda : params)) env = do
    evalParams <- checkParams (map (\x -> eval x env) params)
    evalLambda <- eval lambda env
    case evalLambda of
        Func args expr fenv -> if length params /= length args then Left (WrongNumberOfArgs "lambda") 
                               else eval expr ((zip args evalParams) ++ fenv)
        _ -> Left (AnotherError "Lambda couldn't evaluate")

-------------------------

multiIntOp :: (Integer -> Integer -> Integer) -> Integer -> [Val] -> Errorable Val
multiIntOp op initial args = do
    numArgs <- liftNum args
    return $ NumVal $ foldr op initial numArgs
    where
        liftNum [] = Right []
        liftNum ((NumVal x) : xs) = case liftNum xs of
            Right xsLifted -> Right (x : xsLifted)
            Left err -> Left err
        liftNum _ = Left TypeMismatch

toBool :: Val -> Maybe Bool
toBool x = case x of 
    StrVal "#t" -> Just True
    StrVal "#f" -> Just False
    _ -> Nothing

fromBool :: Bool -> Val
fromBool x = case x of
    True -> StrVal "#t"
    False -> StrVal "#f"

binaryCompOp :: (Integer -> Integer -> Bool) -> [Val] -> Errorable Val
binaryCompOp op [(NumVal x), (NumVal y)] =  return $ fromBool $ op x y
binaryCompOp _ _ = Left TypeMismatch

binaryEqOp :: [Val] -> Errorable Val 
binaryEqOp [(NumVal x), (NumVal y)] = return $ fromBool $ x == y
binaryEqOp [(StrVal x), (StrVal y)] = return $ fromBool $ x == y
binaryEqOp _ = Left TypeMismatch

unaryBoolOp :: (Bool -> Bool) -> [Val] -> Errorable Val
unaryBoolOp op [StrVal "#t"] = return $ fromBool $ op True
unaryBoolOp op [StrVal "#f"] = return $ fromBool $ op False
unaryBoolOp _ _ = Left TypeMismatch

binaryBoolOp :: (Bool -> Bool -> Bool) -> [Val] -> Errorable Val
binaryBoolOp op [x, y] = case toBool x of
    Just xval -> case toBool y of
        Just yval -> return $ fromBool $ op xval yval
        Nothing -> Left TypeMismatch
    Nothing -> Left TypeMismatch
binaryBoolOp _ _ = Left (WrongNumberOfArgs "Bool Operation")

carOp :: [Val] -> Errorable Val
carOp [Cons (x, y)] = return $ x
carOp _ = Left TypeMismatch

cdrOp :: [Val] -> Errorable Val
cdrOp [Cons (x, y)] = return $ y
cdrOp _ = Left TypeMismatch

consOp :: [Val] -> Errorable Val
consOp [x, y] = return $ Cons (x, y)
consOp _ = Left (WrongNumberOfArgs "Cons")

listOp :: [Val] -> Errorable Val
listOp l = return $ toCons l

-------------------------

toCons :: [Val] -> Val
toCons [] = StrVal "nil"
toCons (x:xs) = Cons (x, toCons xs)

toList :: Val -> Errorable [Val]
toList (StrVal "nil") = Right []
toList (Cons (x, y)) = case toList y of
    Right list -> Right (x : list)
    Left err -> Left err
toList _ = Left (AnotherError "Can't convert cons to list")

unquote :: Val -> Errorable Expr
unquote (StrVal "nil") = Right $ List []
unquote (StrVal x) = Right $ Name x
unquote (NumVal x) = Right $ Num x
unquote c@(Cons _) = do
    consList <- toList c
    unquotedList <- liftRight $ map unquote consList
    return $ List unquotedList where
        liftRight [] = Right []
        liftRight ((Right x) : xs) = case liftRight xs of
            Right xsLifted -> Right (x : xsLifted)
            Left err -> Left err
        liftRight _ = Left TypeMismatch
unquote _ = Left TypeMismatch

processQuote :: Expr -> Val
processQuote datum = case datum of
    List [] -> StrVal "nil"
    Num x -> NumVal x
    Name x -> StrVal x
    List (x:xs) -> Cons (processQuote x, processQuote (List xs))

processQuasi :: Expr -> Env -> Integer -> Errorable Val
processQuasi (List [Name "unquote", datum]) env 1 = case eval datum env of
    Left (UndefinedVar "unquote") -> Left (AnotherError "UnquotesMoreThanQuotes")
    other -> other
processQuasi (List (lst@[Name "unquote", _])) env cnt = do
    l <- checkParams (map (\x -> processQuasi x env (cnt - 1)) lst)
    return (toCons l)
processQuasi (List (lst@[Name "quasiquote", _])) env cnt = do
    l <- checkParams (map (\x -> processQuasi x env (cnt + 1)) lst)
    return (toCons l)
processQuasi (List lst) env cnt = do
    l <- checkParams (map (\x -> processQuasi x env cnt) lst)
    return (toCons l)
processQuasi temp _ _ = return (processQuote temp)

checkParams :: [Errorable Val] -> Errorable [Val]
checkParams [] = Right []
checkParams ((Right x) : xs) = case checkParams xs of
    Right xsLifted -> Right (x : xsLifted)
    Left err -> Left err
checkParams ((Left err) : xs) = Left err

liftName :: [Expr] -> Errorable [String]
liftName [] = Right []
liftName ((Name x) : xs) = case liftName xs of
    Right xsLifted -> Right (x : xsLifted)
    Left err -> Left err
liftName _ = Left (AnotherError "Can't lift name expr")

liftList :: [Expr] -> Errorable [[Expr]]
liftList [] = Right []
liftList ((List x) : xs) = case liftList xs of
    Right xsLifted -> Right (x : xsLifted)
    Left err -> Left err
liftList _ = Left (AnotherError "Can't lift list expr")