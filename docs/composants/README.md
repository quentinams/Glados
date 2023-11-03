# Composants de notre Glados

Bienvenue dans la section des composants du projet Glados. Chaque composant joue un rôle essentiel dans la structure globale du langage. Dans ce répertoire, vous trouverez une documentation détaillée pour chaque module et sa fonction au sein du langage.

## Vue d'ensemble des composants

- **[ASTConversion.md](./ASTConversion.md)** : Ce composant gère la conversion des expressions vers l'Arbre de Syntaxe Abstraite (AST).
  
- **[DataByteCode.md](./DataByteCode.md)** : Une vue d'ensemble du bytecode utilisé pour optimiser l'exécution du langage.
  
- **[Datas.md](./Datas.md)** : Description des structures de données fondamentales utilisées dans le langage.
  
- **[EvalByteCode.md](./EvalByteCode.md)** : Ce module s'occupe de l'évaluation du bytecode.
  
- **[Main.md](./Main.md)** : Le point d'entrée principal du langage, orchestrant les différents composants pour l'exécution.
  
- **[ParseAnd.md](./ParseAnd.md), [ParseChar.md](./ParseChar.md), [ParseExpr.md](./ParseExpr.md), [ParseOr.md](./ParseOr.md), [ParseUtils.md](./ParseUtils.md)** : Ces composants forment le cœur de l'analyseur syntaxique du langage, décomposant le code source en éléments manipulables.
  
- **[ParserModule.md](./ParserModule.md)** : Une vue d'ensemble du système d'analyse syntaxique.
  
- **[Types.md](./Types.md)** : Définitions des types de données et des structures utilisées dans le langage.
  
- **[WriteByteCode.md](./WriteByteCode.md)** : Ce composant traite de la génération du bytecode à partir de l'AST.

## Comment utiliser cette documentation

Pour comprendre le fonctionnement interne du langage, nous recommandons de commencer par [Main.md](./Main.md), qui fournit une introduction à la structure générale du projet. Ensuite, selon votre intérêt, vous pouvez vous plonger dans les composants spécifiques pour obtenir une compréhension plus approfondie.
