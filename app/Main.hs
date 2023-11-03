import Control.Exception
import Datas
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
import EvalByteCode

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
            case parseLispFile content of 
                Right exprs -> do
                    let asts = map exprToAST exprs
                    case compile (Sequence asts) of
                        Left err -> putStrLn $ "Compilation error: " ++ err
                        Right bytecode -> do
                            let bytecodeInstructions = showInstructions bytecode
                            writeFile (changeExtensionToBC filename) bytecodeInstructions
                            putStrLn "Bytecode écrit avec succès dans le fichier .bc."
                Left err -> putStrLn err
        [filename] 
            | hasBCExtension filename -> do
                content <- readFile filename
                processByteCodeFile content
            | otherwise -> putStrLn "Please provide a valid .bc file."
        _ -> putStrLn "Usage: ./glados [-i|--asm|-c] <filename>"

processLisp :: String -> IO ()
processLisp content =
    let initialEnv = [] in
    case parseLispFile content of
        Right exprs -> do
            let (_, finalEnv, values) = foldl
                    (\(env, accEnv, accValues) expr ->
                        case compile (exprToAST expr) of
                            Left err -> (env, accEnv ++ [env], accValues ++ [Left err])
                            Right instructions ->
                                case exec instructions [] env of
                                    Left runtimeErr -> (env, accEnv ++ [env], accValues ++ [Left runtimeErr])
                                    Right (result, newEnv) -> (newEnv, accEnv ++ [newEnv], accValues ++ [Right result])
                    )
                    (initialEnv, [], [])
                    exprs
            case values of
                [] -> putStrLn "No expressions found."
                _ -> do
                    forM_ values $ \value ->
                        case value of
                            Left err -> putStrLn $ "Error: " ++ err
                            Right result -> putStrLn $ show result
        Left err -> putStrLn err

processLispHumanReadable :: String -> IO ()
processLispHumanReadable content = 
    case parseLispFile content of
        Right exprs -> do
            forM_ exprs $ \expr -> do
                let ast = exprToAST expr
                case compile ast of
                    Left err -> putStrLn $ "Compilation error: " ++ err
                    Right bytecode -> do
                        putStr $ showInstructions bytecode
        Left err -> putStrLn err

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
                Right value -> putStrLn $ show (fst value)

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
        ["Store", var]          -> Just $ Store var
        ["Load", var]           -> Just $ Load var
        _                       -> Nothing



parseOp :: String -> Maybe Op
parseOp op = 
    case op of
        "Add"  -> Just DataByteCode.Add
        "Sub"  -> Just DataByteCode.Sub
        "Mul"  -> Just Mul
        "Div"  -> Just Div
        "Eq"   -> Just Eq
        "Less" -> Just Less
        _      -> Nothing
