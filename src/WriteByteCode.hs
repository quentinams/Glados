module WriteByteCode (compile) where

import Datas (Expr(..), AST(..))
import DataByteCode (Value(..), Op(..), Instruction(..), Insts)

-- Compile une expression AST en bytecode
compile :: AST -> Either String Insts
compile ast = 
    case ast of
        Var x -> compileVar x
        Definition var value -> compileDefinition var value
        Const n -> Right [Push (Num (round n))]  -- conversion Float vers Int, attention à la perte de précision
        TruthValue b -> Right [Push (DataByteCode.Bool b)]
        If cond thenBranch elseBranch -> compileIf cond thenBranch elseBranch
        Datas.Add left right -> compileBinaryOp DataByteCode.Add left right
        Datas.Sub left right -> compileBinaryOp DataByteCode.Sub left right
        Datas.Eq left right -> compileBinaryOp DataByteCode.Eq left right
        Sequence exprs -> compileSequence exprs
        -- Les autres cas nécessitent une gestion plus complexe des environnements, etc.
        _ -> Left $ "Unsupported operation during compilation: " ++ show ast

-- Compile un AST 'If'
compileIf :: AST -> AST -> AST -> Either String Insts
compileIf cond thenBranch elseBranch = do
    condCode <- compile cond
    thenCode <- compile thenBranch
    elseCode <- compile elseBranch
    let jumpOverThen = [JumpIfFalse (length thenCode + 2)]  -- +2 pour sauter l'instruction 'Jump' et tout le bloc 'else'
    let jumpToEnd = [Jump (length elseCode)]  -- saut inconditionnel à la fin du 'then'
    return $ condCode ++ jumpOverThen ++ thenCode ++ jumpToEnd ++ elseCode


-- Compile un AST 'Sequence'
compileSequence :: [AST] -> Either String Insts
compileSequence [] = Right []
compileSequence (x:xs) = do
    xCode <- compile x
    xsCode <- compileSequence xs
    return $ xCode ++ xsCode

-- Compile une opération binaire
compileBinaryOp :: Op -> AST -> AST -> Either String Insts
compileBinaryOp op left right = do
    leftCode <- compile left
    rightCode <- compile right
    return $ leftCode ++ rightCode ++ [Call op]

-- Compile une définition AST 'Definition'
compileDefinition :: String -> AST -> Either String Insts
compileDefinition var valueAst = do
    valueCode <- compile valueAst
    return $ valueCode ++ [Store var]

-- Compile une référence à une variable AST 'Var'
compileVar :: String -> Either String Insts
compileVar x = Right [Load x]
