module Main where

import Test.HUnit
import EvalByteCode (execOp, exec)
import DataByteCode (Value(..), Op(..), Instruction(..), Stack, Insts)
import WriteByteCode (compile)
import Datas (Expr(..), AST(..))
import ASTConversion (exprToAST)


-- Test cases for execOp
testExecOpDivByZero = TestCase (assertEqual "Should return error on division by zero" (Left "Error: division by 0") (execOp Div [Num 4, Num 0]))
testExecOpInvalidStack = TestCase (assertEqual "Should return error on invalid stack" (Left "Error: Invalid stack for operation") (execOp Add [DataByteCode.Bool True]))
testExecOpEq = TestCase (assertEqual "Should perform equality check correctly" (Right [DataByteCode.Bool True]) (execOp Eq [Num 5, Num 5]))
testExecOpLess = TestCase (assertEqual "Should perform less than check correctly" (Right [DataByteCode.Bool True]) (execOp Less [Num 4, Num 5]))

-- Additional test cases for execOp
testExecOpAdd = TestCase (assertEqual "Should perform addition correctly" (Right [Num 9]) (execOp Add [Num 4, Num 5]))
testExecOpSubtract = TestCase (assertEqual "Should perform subtraction correctly" (Right [Num 1]) (execOp Sub [Num 5, Num 4]))
testExecOpMul = TestCase (assertEqual "Should perform multiplication correctly" (Right [Num 20]) (execOp Mul [Num 4, Num 5]))
testExecOpInvalidOp = TestCase (assertEqual "Should return error for invalid operation" (Left "Error: Invalid stack for operation") (execOp Add [Num 4]))

-- Test cases for exec
testExecNoInstructions = TestCase (assertEqual "Should return error on no instructions" (Left "Error: No instructions") (exec [] []))
testExecInvalidInstruction = TestCase (assertEqual "Should return error on invalid instruction" (Left "Error: Invalid instruction") (exec [JumpIfFalse 2] []))

-- Additional test cases for exec
testExecSimpleProgram = TestCase (assertEqual "Should execute a simple program correctly" (Right (Num 2)) (exec [Push (Num 1), Push (Num 1), Call Add, Ret] []))
testExecJumpIfFalse = TestCase (assertEqual "Should jump if top of stack is false" (Right (Num 2)) (exec [Push (DataByteCode.Bool False), JumpIfFalse 2, Push (Num 1), Ret, Push (Num 2), Ret] []))
testExecInvalidJump = TestCase (assertEqual "Should return error for invalid jump" (Left "Error: Jump leads outside of instructions range") (exec [JumpIfFalse 3] [DataByteCode.Bool False]))

-- Test cases for compile
testCompileConst = TestCase (assertEqual "Should compile Const correctly" (Right [Push (Num 5)]) (compile (Const 5.0)))
testCompileTruthValue = TestCase (assertEqual "Should compile TruthValue correctly" (Right [Push (DataByteCode.Bool True)]) (compile (TruthValue True)))
testCompileIf = TestCase (assertEqual "Should compile If correctly"
    (Right [Push (DataByteCode.Bool True), JumpIfFalse 3, Push (Num 5), Jump 1, Push (Num 4)]) 
    (compile (If (TruthValue True) (Const 5.0) (Const 4.0))))


-- Here we test the integration of compile and exec functions
testCompileAndExecSimple = TestCase (do
    let ast = Sequence [Const 1.0, Const 2.0, If (TruthValue True) (Const 5.0) (Const 4.0)]
    let bytecode = compile ast
    case bytecode of
        Left err -> assertFailure err
        Right insts -> assertEqual "Should compile and execute a simple AST" (Right (Num 5)) (exec (insts ++ [Ret]) []))

-- Test list
tests = TestList [TestLabel "testExecOpDivByZero" testExecOpDivByZero,
                  TestLabel "testExecOpInvalidStack" testExecOpInvalidStack,
                  TestLabel "testExecOpEq" testExecOpEq,
                  TestLabel "testExecOpLess" testExecOpLess,
                  TestLabel "testExecOpAdd" testExecOpAdd,
                  TestLabel "testExecOpSubtract" testExecOpSubtract,
                  TestLabel "testExecOpMul" testExecOpMul,
                  TestLabel "testExecOpInvalidOp" testExecOpInvalidOp,
                  TestLabel "testExecNoInstructions" testExecNoInstructions,
                  TestLabel "testExecInvalidInstruction" testExecInvalidInstruction,
                  TestLabel "testExecSimpleProgram" testExecSimpleProgram,
                  TestLabel "testExecJumpIfFalse" testExecJumpIfFalse,
                  TestLabel "testExecInvalidJump" testExecInvalidJump,
                  TestLabel "testCompileConst" testCompileConst,
                  TestLabel "testCompileTruthValue" testCompileTruthValue,
                  TestLabel "testCompileIf" testCompileIf,
                  TestLabel "testCompileAndExecSimple" testCompileAndExecSimple,
                  "testSymbol" ~: Var "x" ~=? exprToAST (Symbol "x"),
                  "testNumber" ~: Const 5.0 ~=? exprToAST (Number 5.0),
                  "testBoolTrue" ~: TruthValue True ~=? exprToAST (Datas.Bool True),
                  "testBoolFalse" ~: TruthValue False ~=? exprToAST (Datas.Bool False),
                  "testLambda" ~: LambdaFunc ["x"] (Var "x") ~=? exprToAST (Lambda ["x"] (Symbol "x"))]

main :: IO Counts
main = runTestTT tests
