module Main (main) where

import Lib

main :: IO ()
main = do
    putStrLn "Enter commande"
    string <-  getLine
    putStrLn (string)