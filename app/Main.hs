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
import DataByteCode (Value(..), Op(..), Instruction(..), Stack, Insts, showInstructions)
import Control.Monad (forM_)
import Data.Word (Word8)
import Data.Bits (shiftR, (.&.))
import Data.ByteString.Lazy (pack, hPut)
import Data.Char (isDigit)

main :: IO ()
main = do 
    args <- getArgs
    case args of 
        ["-i", filename] -> do
            content <- readFile filename
            processLisp content
        ["--asm", filename] -> do
            content <- readFile filename
            processLispHumanReadable content
        ["-c", filename] -> do
            content <- readFile filename
            case runParser parseExpr content of 
                Just (expr, _) -> do
                    let ast = exprToAST expr
                    case compile ast of
                        Left err -> putStrLn $ "Compilation error: " ++ err
                        Right bytecode -> do
                            let bytecodeInstructions = showInstructions bytecode
                            writeFile (changeExtensionToBC filename) bytecodeInstructions
                            putStrLn "Bytecode écrit avec succès dans le fichier .bc."
                Nothing -> putStrLn "Failed to parse the file content."
        [filename] 
            | hasBCExtension filename -> do
                content <- readFile filename
                processByteCodeFile content
            | otherwise -> putStrLn "Please provide a valid .bc file."
        _ -> putStrLn "Usage: ./glados [-i|--asm|-c] <filename>"


processLisp :: String -> IO ()
processLisp content = 
    case runParser parseExpr content of
        Just (expr, _) -> do
            let ast = exprToAST expr
            case compile ast of
                Left err -> putStrLn $ "Compilation error: " ++ err
                Right bytecode -> do
                    case exec bytecode [] [] of
                        Left runtimeErr -> putStrLn $ "Runtime error: " ++ runtimeErr
                        Right value -> putStrLn $ show value
        Nothing -> putStrLn "Failed to parse the lisp expression."

processLispHumanReadable :: String -> IO ()
processLispHumanReadable content = 
    case runParser parseExpr content of
        Just (expr, _) -> do
            let ast = exprToAST expr
            case compile ast of
                Left err -> putStrLn $ "Compilation error: " ++ err
                Right bytecode -> do
                    putStr $ showInstructions bytecode
        Nothing -> putStrLn "Failed to parse the lisp expression."


changeExtensionToBC :: String -> String
changeExtensionToBC filename = base ++ ".bc"
  where base = takeWhile (/= '.') filename

hasBCExtension :: String -> Bool
hasBCExtension filename = dropWhile (/= '.') filename == ".bc"

processByteCodeFile :: String -> IO ()
processByteCodeFile content = 
    case parseInstructions content of
        Left errMsg -> putStrLn errMsg
        Right instructions -> 
            case exec instructions [] [] of
                Left runtimeErr -> putStrLn $ "Runtime error: " ++ runtimeErr
                Right value -> putStrLn $ show value

parseInstructions :: String -> Either String [Instruction]
parseInstructions content = 
    case sequence $ zipWith debugParse (lines content) [1..] of
        Left err -> Left err
        Right instructions -> Right instructions

debugParse :: String -> Int -> Either String Instruction
debugParse line num = 
    case parseInstruction line of
        Nothing -> Left ("Failed to parse line " ++ show num ++ ": " ++ line)
        Just instruction -> Right instruction


parseInstruction :: String -> Maybe Instruction
parseInstruction str = 
    case words str of
        ["Push", value] 
            | all isDigit value -> Just $ Push (Num (read value))
        ["Push", "Bool", "#t"]  -> Just $ Push (DataByteCode.Bool True)
        ["Push", "Bool", "#f"]  -> Just $ Push (DataByteCode.Bool False)
        ["Call", op]            -> parseOp op >>= \o -> Just (Call o)
        ["Ret"]                 -> Just Ret
        ["JumpIfFalse", offset] -> Just $ JumpIfFalse (read offset)
        ["Jump", offset]        -> Just $ Jump (read offset)
        _                       -> Nothing


parseOp :: String -> Maybe Op
parseOp op = 
    case op of
        "Add"  -> Just DataByteCode.Add
        "Sub"  -> Just Sub
        "Mul"  -> Just Mul
        "Div"  -> Just Div
        "Eq"   -> Just Eq
        "Less" -> Just Less
        _      -> Nothing
