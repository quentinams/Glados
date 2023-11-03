---
sidebar: auto
---

# ParseAnd.hs

## Module : `ParseAnd`

Ce module fournit des fonctions pour combiner et répéter des parsers. Il s'appuie sur la définition du type `Parser` du module `ParserModule`.

### Fonctions Principales

#### parseAnd

Combine deux parsers et renvoie leurs résultats sous forme de tuple.

```haskell
parseAnd :: Parser a -> Parser b -> Parser (a, b)
```

Reçoit deux parsers.
Renvoie un parser qui combine leurs résultats.

#### parseAndWith

Combine deux parsers avec une fonction de combinaison.

```haskell
parseAndWith :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
```
Reçoit une fonction de combinaison et deux parsers.
Renvoie un parser qui utilise la fonction pour combiner les résultats des deux parsers.

#### parseMany

Répète un parser autant de fois que possible.
```haskell
parseMany :: Parser a -> Parser [a]
```

Reçoit un parser.
Renvoie un parser qui répète le parser donné autant de fois que possible et renvoie une liste des résultats.

#### parseSome
Répète un parser au moins une fois.

```haskell
parseSome :: Parser a -> Parser [a]
```
Reçoit un parser.
Renvoie un parser qui répète le parser donné au moins une fois et renvoie une liste des résultats.

### Détails d'implémentation
La fonction parseAnd tente d'abord d'exécuter le premier parser. Si elle réussit, elle tente d'exécuter le second parser sur l'entrée restante.

parseAndWith est similaire à parseAnd, mais elle utilise une fonction pour combiner les résultats.

parseMany tente de répéter un parser autant de fois que possible.

parseSome s'assure qu'un parser est exécuté au moins une fois.

::: tip Note
Cette documentation donne un aperçu général des fonctions définies dans le module ParseAnd. Pour une compréhension plus détaillée, il est recommandé de consulter directement le code source.
:::