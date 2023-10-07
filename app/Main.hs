import Control.Exception
import Datas
import Env
import Evaluation
import Control.Monad (foldM)

main :: IO ()
main = do
    let expressions = 
            [ List [Symbol "define", Symbol "factorial", 
                    List [Symbol "lambda", List [Symbol "n"], 
                          List [Symbol "if", 
                                List [Symbol "eq?", Symbol "n", Number 1],
                                Number 1,
                                List [Symbol "*", Symbol "n", 
                                      List [Symbol "factorial", 
                                            List [Symbol "-", Symbol "n", Number 1]
                                      ]
                                ]
                          ]
                    ]
            ]
            , List [Symbol "factorial", Number 5]
            ]

    (finalEnv, maybeLastResult) <- foldM evalAndStore (initialEnv, Nothing) expressions
    case maybeLastResult of
        Just lastResult -> putStrLn $ show lastResult
        Nothing -> putStrLn "No result to show."
    return ()

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
                    return (env, Nothing)
                Right (newEnv, res) -> do
                    return (newEnv, Just res)

