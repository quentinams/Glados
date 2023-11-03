---
sidebar: auto
---

# Fichier Principal

## Module : Main.hs

Ce module est le point d'entrée principal du programme. Il fournit des fonctions pour interpréter et compiler du code Lisp vers du bytecode, et pour exécuter ce bytecode.

### Fonction principale

#### main.hs

```haskell
main :: IO ()
```
Le point d'entrée du programme. Il traite les arguments de la ligne de commande et agit en conséquence.

Fonctions Auxiliaires
loopReadEval
```haskell
loopReadEval :: IO ()
```
Boucle infiniment pour lire, évaluer et imprimer les expressions Lisp entrées par l'utilisateur.

handleEOF
```haskell
handleEOF :: IOException -> IO String
```
Traite la fin de fichier ou les entrées Ctrl-D.

processLisp
```haskell
processLisp :: String -> IO ()
```
Traite une chaîne de caractères contenant du code Lisp, la compile et exécute le bytecode résultant.

processLispHumanReadable
```haskell
processLispHumanReadable :: String -> IO ()
```
Traite une chaîne de caractères contenant du code Lisp et affiche le bytecode résultant de manière lisible.

changeExtensionToBC
```haskell
changeExtensionToBC :: String -> String
```
Change l'extension d'un nom de fichier en .bc.

hasBCExtension
```haskell
hasBCExtension :: String -> Bool
```
Vérifie si un nom de fichier a l'extension .bc.

processByteCodeFile
```haskell
processByteCodeFile :: String -> IO ()
```
Traite un fichier de bytecode et exécute le bytecode.

parseInstructions
```haskell
parseInstructions :: String -> Either String [Instruction]
```
Parse une chaîne de caractères contenant des instructions bytecode et renvoie une liste d'instructions.

debugParse
```haskell
debugParse :: String -> Int -> Either String Instruction
```
Parse une instruction bytecode à partir d'une ligne avec un numéro de ligne pour le débogage.

parseInstruction
```haskell
parseInstruction :: String -> Maybe Instruction
```
Parse une instruction bytecode à partir d'une chaîne de caractères.

parseOp
```haskell
parseOp :: String -> Maybe Op
```
Parse une opération à partir d'une chaîne de caractères.

::: tip Note
Cette documentation donne un aperçu général des fonctions définies dans le fichier principal. Pour une compréhension plus détaillée, il est recommandé de consulter directement le code source.
:::