import Control.Exception
import Datas
import Env
import Evaluation
import ParserModule
import ParseExpr
import ASTConversion
import WriteByteCode
import EvalByteCode
import System.Environment
import System.IO


main :: IO ()
main = do 
    args <- getArgs
    case args of 
        ["-i", filename] -> do
            content <- readFile filename
            processLisp content
        ["--asm", filename] -> do
            putStrLn "Print en human readable la compilation"
        [filename] -> do
            putStrLn "Création d'un binaire"
        _ -> putStrLn "Usage: ./glados [-i|--asm] <filename>"

processLisp :: String -> IO ()
processLisp content = 
    case runParser parseExpr content of
        Just (expr, _) -> do
            let ast = exprToAST expr
            case compile ast of
                Left err -> putStrLn $ "Compilation error: " ++ err
                Right bytecode -> do
                    case exec bytecode [] of
                        Left runtimeErr -> putStrLn $ "Runtime error: " ++ runtimeErr
                        Right value -> putStrLn $ show value
        Nothing -> putStrLn "Failed to parse the lisp expression."

