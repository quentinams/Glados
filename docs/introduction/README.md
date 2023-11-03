# Projet Glados

Ce projet vise à concevoir et développer un nouveau langage de programmation en deux parties principales : la création d'un langage de base et son évolution vers une langue plus avancée. Le langage commencera avec une syntaxe inspirée des expressions symboliques (S-Expressions) typiques des langages LISP, puis évoluera vers une syntaxe propre et unique, tout en améliorant ses performances d'exécution.

## Vue d'ensemble

- **Partie 1** : Dans cette phase, le langage sera minimaliste mais fonctionnel, basé sur des S-Expressions.
  
- **Partie 2** : La langue évoluera à la fois en termes de syntaxe, de grammaire et de sémantique, ainsi qu'en termes de performances d'exécution. Elle adoptera sa propre syntaxe, se débarrassant des S-Expressions et mettant en œuvre un ensemble d'instructions virtuelles pour une exécution rapide.

## Structure du Répertoire

```plaintext
.
├── CHANGELOG.md
├── LICENSE
├── Makefile
├── README.md
├── Setup.hs
├── app
│   └── Main.hs
├── docs
├── glados.cabal
├── package.yaml
├── src
├── stack.yaml
├── stack.yaml.lock
└── test

```

## Comment commencer

- **Installation** : Consultez le guide [Utilisation](../utilisation/configuration.md) pour les instructions d'installation.
  
- **Tests** : Des tests préliminaires sont disponibles dans le dossier [Tests](../tests) pour évaluer les fonctionnalités de base du langage.

- **Composants** : Pour une compréhension approfondie des composants clés du projet, reportez-vous au dossier [Composants](../composants/README.md).

Nous espérons que ce projet fournira un aperçu passionnant de la conception des langages de programmation et encouragera une exploration plus approfondie dans ce domaine. Bon codage !
