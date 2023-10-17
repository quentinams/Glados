import Control.Exception
import Datas
import Env
import Evaluation
import ParserModule
import ParseExpr

main :: IO ()
main = loop initialEnv
  where
    loop env = do
        putStrLn "Enter your lisp expression (or type 'quit' to exit):"
        input <- getLine
        if input == "quit"
            then putStrLn "Goodbye!"
            else do
                case runParser parseExpr input of
                    Just (expr, _) -> do
                        (newEnv, maybeResult) <- evalAndStore (env, Nothing) expr
                        case maybeResult of
                            Just lastResult -> putStrLn $ show lastResult
                            Nothing -> putStrLn "No result to show."
                        loop newEnv
                    Nothing -> putStrLn "Failed to parse the lisp expression."

evalAndStore :: (Env, Maybe Expr) -> Expr -> IO (Env, Maybe Expr)
evalAndStore (env, _) expr = do
    result <- try (evaluate (evalExpr env expr)) :: IO (Either SomeException (Either String (Env, Expr)))
    case result of
        Left ex -> do 
            putStrLn $ "An error occurred: " ++ show ex
            return (env, Nothing)
        Right val ->
            case val of
                Left err -> do 
                    putStrLn $ "Evaluation error: " ++ err
                    return (env, Nothing)  -- Ici, nous renvoyons toujours l'ancien env
                Right (newEnv, res) -> do
                    return (newEnv, Just res)  -- Utilisez newEnv ici

