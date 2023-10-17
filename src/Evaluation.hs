module Evaluation (evalExpr) where

import Control.Monad (liftM2)
import Env
import Datas
import Debug.Trace
import MathFunctions

getName :: Expr -> Either String String
getName (Symbol s) = Right s
getName _ = Left "Expected a symbol but got something else"

eval :: Env -> Expr -> Either String (Env, Expr)
eval env (List (Symbol "lambda" : List params : [body])) = 
    let strParams = map getName params 
    in case sequence strParams of
        Right names -> Right (env, Lambda names body)
        Left err -> Left err
eval env (List (f:args)) = do
    (env', func) <- eval env f
    case func of
        Lambda params body -> 
            if length params /= length args 
            then Left "Lambda: Number of arguments does not match"
            else do
                argVals <- mapM (eval env') args
                let newBindings = zip params (map snd argVals)
                let newEnv = extendEnv env' newBindings
                eval newEnv body
        Symbol s -> 
            apply env' func args
        _ -> Left "Expected a function or symbol at head of list"
eval env expr = 
    case expr of
        Symbol s -> 
            case lookupEnv s env of
                Just expr' -> Right (env, expr')
                Nothing    -> Left $ "Variable not found: " ++ s 
        Number n -> Right (env, Number n)
        Bool b -> Right (env, Bool b)
        List(x:xs) -> apply env x xs
        _ -> Left "Unsupported expression type"


apply :: Env -> Expr -> [Expr] -> Either String (Env, Expr)
apply env (Symbol "lambda") [List params, body] = 
    let strParams = map getName params 
    in case sequence strParams of
        Right names -> Right (env, Lambda names body)
        Left err -> Left err
apply env (Symbol s) args =
   case s of
        "define" -> defineVar env args
        "+" -> addArgs env args
        "*" -> multiplyArgs env args
        "-" -> subtractArgs env args
        "div" -> euclideanDivision env args  -- Utilisez euclideanDivision ici
        "/" -> floatDivision env args  -- Utilisez floatDivision ici
        "mod" -> moduloArgs env args
        "eq?" -> equalExpr env args
        "<" -> lessThanExpr env args
        "if" -> ifExpr env args
        _   -> Left $ "Unknown function: " ++ s
apply _ _ _ = Left "Expected symbol at head of list"


ifExpr :: Env -> [Expr] -> Either String (Env, Expr)
ifExpr env [test, consequent, alternative] = 
    do (env1, testVal) <- eval env test
       case testVal of
           Bool b -> if b then eval env1 consequent else eval env1 alternative
           _      -> Left "if: test expression did not evaluate to a boolean"
ifExpr _ _ = Left "if: expects exactly three arguments"

equalExpr :: Env -> [Expr] -> Either String (Env, Expr)
equalExpr env [expr1, expr2] = 
    do (env1, val1) <- eval env expr1
       (env2, val2) <- eval env1 expr2
       return (env2, Bool $ val1 == val2)
equalExpr _ _ = Left "eq? expects exactly two arguments"

lessThanExpr :: Env -> [Expr] -> Either String (Env, Expr)
lessThanExpr env [expr1, expr2] = 
    do (env1, val1) <- eval env expr1
       (env2, val2) <- eval env1 expr2
       case (val1, val2) of
         (Number n1, Number n2) -> return (env2, Bool $ n1 < n2)
         _                      -> Left "lessThan: expects two numbers"
lessThanExpr _ _ = Left "lessThan: expects exactly two arguments"

defineVar :: Env -> [Expr] -> Either String (Env, Expr)
defineVar env (List (Symbol funcName : params) : body : []) = 
    let strParams = map (\(Symbol s) -> s) params 
        lambdaExpr = Lambda strParams body
    in Right (extendEnv env [(funcName, lambdaExpr)], lambdaExpr)
defineVar env [Symbol var, expr] = 
    eval env expr >>= \(newEnv, val) -> Right (extendEnv newEnv [(var, val)], val)
defineVar _ _ = Left "define expects a symbol and an expression or a function definition"


addArgs :: Env -> [Expr] -> Either String (Env, Expr)
addArgs env args = 
    fmap (\nums -> (env, Number $ foldl (+) 0 nums)) (mapM (evalFloat env) args)

evalExpr :: Env -> Expr -> Either String (Env, Expr)
evalExpr = eval
