---
sidebar: auto
---

# ParseChar.hs

## Module : `ParseChar`

Ce module fournit des fonctions de parsing pour des caractères spécifiques et pour n'importe quel caractère d'une chaîne donnée. Il s'appuie sur la définition du type `Parser` du module `ParserModule`.

### Fonctions Principales

#### parseChar

Parse un caractère spécifique.

```haskell
parseChar :: Char -> Parser Char
```
Reçoit un caractère cible.
Renvoie un parser qui tente de parser ce caractère spécifique

#### parseAnyChar

Parse n'importe quel caractère qui se trouve dans la chaîne fournie.

```haskell
parseAnyChar :: String -> Parser Char
```
Reçoit une chaîne de caractères.
Renvoie un parser qui tente de parser n'importe quel caractère de cette chaîne.

### Détails d'implémentation

La fonction **parseChar** tente de parser le caractère spécifié en tête de la chaîne d'entrée. Si ce caractère correspond, il renvoie le caractère et le reste de la chaîne.

**parseAnyChar** vérifie si le premier caractère de la chaîne d'entrée appartient à la chaîne fournie. Si c'est le cas, il renvoie le caractère et le reste de la chaîne.

::: tip Note
Cette documentation donne un aperçu général des fonctions définies dans le module ParseChar. Pour une compréhension plus détaillée, il est recommandé de consulter directement le code source.
:::