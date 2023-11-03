---
sidebar: auto
---

# ParseUtils.hs

## Module : `ParseUtils`

Le module `ParseUtils` fournit une collection d'utilitaires de parsing pour traiter des éléments syntaxiques courants tels que les espaces, les nombres, les symboles, etc.

### Fonctions principales

#### `parseString`

Analyse une chaîne de caractères donnée, en vérifiant chaque caractère individuellement.

```haskell
parseString :: String -> Parser String
```

#### `skipSpaces`

Ignore tous les espaces, tabulations et sauts de ligne rencontrés.

```haskell
skipSpaces :: Parser ()
```

#### `parseBool`
Analyse et retourne une expression de type booléen (True ou False).
```haskell
parseBool :: Parser Expr
```

#### `parseSymbol`

Analyse et retourne une expression symbolique. Un symbole est constitué d'une suite de caractères alphanumériques et de certains caractères spéciaux.
```haskell
parseSymbol :: Parser Expr
```

#### `parseNumber`

Analyse et retourne une expression numérique. Actuellement, elle prend en charge uniquement les nombres entiers.
```haskell
parseNumber :: Parser Expr
```

### Détails d'implémentation

- Ces fonctions sont principalement basées sur des combinateurs fournis par **ParserModule**, **ParseChar** et **ParseAnd**.
- **parseString** utilise traverse pour analyser chaque caractère de la chaîne donnée.
- **skipSpaces** utilise **void** pour ignorer le résultat et se concentrer sur l'effet secondaire du parsing (c'est-à-dire consommer des espaces).
- Les fonctions **parseBool**, **parseSymbol** et **parseNumber** retournent une valeur de type Expr définie dans le module Datas.

::: tip Note
Cette documentation donne un aperçu général des fonctions définies dans le module **ParseUtils**. Pour une compréhension plus détaillée, il est recommandé de consulter directement le code source.
:::
