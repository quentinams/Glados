module Main where

import Test.HUnit
import EvalByteCode (execOp, exec)
import DataByteCode (Value(..), Op(..), Instruction(..), Stack, Insts)

-- Test cases for execOp
testExecOpDivByZero = TestCase (assertEqual "Should return error on division by zero" (Left "Error: division by 0") (execOp Div [Num 4, Num 0]))
testExecOpInvalidStack = TestCase (assertEqual "Should return error on invalid stack" (Left "Error: Invalid stack for operation") (execOp Add [Bool True]))
testExecOpEq = TestCase (assertEqual "Should perform equality check correctly" (Right [Bool True]) (execOp Eq [Num 5, Num 5]))
testExecOpLess = TestCase (assertEqual "Should perform less than check correctly" (Right [Bool True]) (execOp Less [Num 4, Num 5]))

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
testExecJumpIfFalse = TestCase (assertEqual "Should jump if top of stack is false" (Right (Num 2)) (exec [Push (Bool False), JumpIfFalse 2, Push (Num 1), Ret, Push (Num 2), Ret] []))
testExecInvalidJump = TestCase (assertEqual "Should return error for invalid jump" (Left "Error: Jump leads outside of instructions range") (exec [JumpIfFalse 3] [Bool False]))

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
                  TestLabel "testExecInvalidJump" testExecInvalidJump]

main :: IO Counts
main = runTestTT tests
