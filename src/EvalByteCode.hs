module EvalByteCode (execOp, exec) where 
import DataByteCode (Value(..), Op(..), Instruction(..), Stack, Insts)

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

exec :: Insts -> Stack -> Either String Value
exec [] _ = Left "Error: No instructions"
exec (Ret:_) (v:_) = Right v
exec (Push v:is) s = exec is (v:s)
exec (Call op:is) s = 
    case execOp op s of
        Left err -> Left err
        Right newStack -> exec is newStack
exec (JumpIfFalse n:is) (Bool False:s) = 
    if n <= length is then
        exec (drop n is) s
    else
        Left "Error: Jump leads outside of instructions range"
exec (JumpIfFalse _:is) (_:s) = exec is s
exec (Jump n:is) s =
    if n <= length is then
        exec (drop n is) s
    else
        Left "Error: Jump leads outside of instructions range"
exec _ _ = Left "Error: Invalid instruction"