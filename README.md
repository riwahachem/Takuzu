# Takuzu Game
![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)

## Description
Le Takuzu Game est une application interactive développée avec [Shiny](https://shiny.rstudio.com/) en R par Wahel El Mazzouji et Riwa Hachem Reda. Elle permet aux utilisateurs de jouer au jeu de logique Takuzu, également connu sous le nom de Binairo.
Le but du jeu est de remplir une grille en respectant certaines règles spécifiques tout en s'amusant avec une interface intuitive.

## Règles du jeu
Le Takuzu (aussi appelé Binairo) est un jeu de logique qui se joue sur une grille carrée. Voici les règles à respecter :

1. **Remplissage binaire** : Chaque case doit contenir soit un 0, soit un 1.

2. **Équilibre parfait** : Chaque ligne et chaque colonne doit contenir exactement le même nombre de 0 et de 1.

3. **Pas de triplets** : Il est interdit d'avoir trois 0 ou trois 1 consécutifs, que ce soit horizontalement ou verticalement.

4. **Lignes et colonnes uniques** : Aucune ligne ne peut être identique à une autre ligne. De même, aucune colonne ne peut être identique à une autre colonne.

## Auteurs
- **Riwa Hachem Reda** - [riwahachemreda@gmail.com](mailto:riwahachemreda@gmail.com)
- **Wahel El Mazzouji** - [wahel.el-mazzouji@etu.umontpellier.fr](mailto:wahel.el-mazzouji@etu.umontpellier.fr)

## Installation
### Prérequis
- R et RStudio doivent être installés sur votre machine.
- Les packages R suivants sont nécessaires : [shiny](https://cran.r-project.org/web/packages/shiny/index.html), [shinydashboard](https://cran.r-project.org/web/packages/shinydashboard/index.html), [shinyWidgets](https://cran.r-project.org/web/packages/shinyWidgets/index.html), [shinyalert](https://cran.r-project.org/web/packages/shinyalert/index.html).

### Installation des Packages
Dans votre console R, exécutez la commande suivante pour installer les packages requis :
```R
install.packages(c("shiny", "shinydashboard", "shinyWidgets", "shinyalert"))
```

### Exécution de l'application
1. Clonez ce dépôt sur votre machine locale
2. Ouvrez le projet dans RStudio
3. Exécutez le fichier `app.R`

## Fonctionnalités
- Grille de jeu 8×8
- Trois niveaux de difficulté : Facile, Normal, Difficile
- Chronomètre intégré
- Système d'indices
- Vérification automatique de la solution
