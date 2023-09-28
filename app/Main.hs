import Control.Exception
import Datas
import Env
import Evaluation
import Control.Monad (foldM)

main :: IO ()
main = do
    let expressions = [List [Symbol "define", Symbol "ajouter", 
                         List [Symbol "lambda", List [Symbol "x", Symbol "y"], 
                               List [Symbol "+", Symbol "x", Symbol "y"]]],
                   List [Symbol "ajouter", Number 2, Number 3]]

    env <- foldM evalAndPrint initialEnv expressions
    return ()

    
evalAndPrint :: Env -> Expr -> IO Env
evalAndPrint env expr = do
    result <- try (evaluate (evalExpr env expr)) :: IO (Either SomeException (Either String (Env, Expr)))
    case result of
        Left ex -> do 
            putStrLn $ "An error occurred: " ++ show ex
            return env
        Right val ->
            case val of
                Left err -> do 
                    putStrLn $ "Evaluation error: " ++ err
                    return env
                Right (newEnv, res) -> do
                    putStrLn $ "Expression: " ++ show expr
                    putStrLn $ "Result: " ++ show res
                    putStrLn "------"
                    return newEnv

