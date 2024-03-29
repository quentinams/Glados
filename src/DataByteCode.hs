module DataByteCode (Value(..), Op(..), Instruction(..), Stack, Insts, SymbolTable, showInstructions) where

data Value = Num Int | Bool Bool deriving (Eq)

instance Show Value where
    show (Num n) = show n
    show (Bool b) = if b then "#t" else "#f"

data Op = Add | Sub | Mul | Div | Eq | Less deriving (Show, Eq)

data Instruction = Push Value 
                 | Call Op 
                 | Ret 
                 | JumpIfFalse Int 
                 | Jump Int 
                 | Store String
                 | Load String
                 deriving (Show, Eq)

type Stack = [Value]
type Insts = [Instruction]
type SymbolTable = [(String, Value)]

showInstructions :: Insts -> String
showInstructions insts = unlines (map show insts)
