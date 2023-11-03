module EvalByteCode (execOp, exec) where

import DataByteCode (Value(..), Op(..), Instruction(..), Stack, Insts, SymbolTable)

execOp :: Op -> Stack -> Either String Stack
execOp opN (Num x:Num y:stack) =
    case opN of 
        Div | y == 0 -> Left "Error: division by 0"
        Eq  -> Right $ Bool (x == y):stack
        Less -> Right $ Bool (x < y):stack
        _ -> Right $ Num (op x y):stack
    where op = case opN of 
                Add -> (+)
                Sub -> (-)
                Mul -> (*)
                Div -> div
execOp _ _ = Left "Error: Invalid stack for operation"

exec :: Insts -> Stack -> SymbolTable -> Either String (Value, SymbolTable)
exec [] (v:_) symTable = Right (v, symTable)
exec [] [] _ = Left "Error: Empty stack and no instructions"
exec (Push v:is) s symTable = exec is (v:s) symTable
exec (Call op:is) s symTable = 
    case execOp op s of
        Left err -> Left err
        Right newStack -> exec is newStack symTable
exec (JumpIfFalse n:is) (Bool False:s) symTable = 
    if n <= length is then
        exec (drop n is) s symTable
    else
        Left "Error: Jump leads outside of instructions range"
exec (JumpIfFalse _:is) (_:s) symTable = exec is s symTable
exec (Jump n:is) s symTable =
    if n <= length is then
        exec (drop n is) s symTable
    else
        Left "Error: Jump leads outside of instructions range"
exec (Load var:is) s symTable = 
    case lookup var symTable of
        Just value -> exec is (value:s) symTable
        Nothing -> Left $ "Error: Undefined variable " ++ var
exec (Store var:is) (value:s) symTable = exec is (value:s) ((var, value):symTable)
exec (Store _:is) [] _ = Left "Error: Stack is empty, nothing to store"
exec inst@(instruction:_) _ _ = Left $ "Error: Invalid instruction - " ++ show instruction
