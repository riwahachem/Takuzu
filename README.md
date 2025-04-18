# Takuzu Game
![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)

## Description
**Takuzu Game** est une application web interactive développée en R avec [Shiny](https://shiny.rstudio.com/). Elle permet de jouer au célèbre jeu de logique **Takuzu**, également connu sous le nom de **Binairo**.

Le but du jeu ? Remplir une grille binaire tout en respectant des règles simples mais exigeantes.

---

## 🧩 Règles du jeu
Le **Takuzu** se joue sur une grille carrée (souvent 8×8) et suit les règles suivantes :

1. Chaque case doit contenir un `0` ou un `1`.
2. Chaque ligne et chaque colonne doit contenir autant de `0` que de `1`.
3. Il est interdit d’avoir trois `0` ou trois `1` consécutifs (horizontalement ou verticalement).
4. Aucune ligne ne doit être identique à une autre. Même contrainte pour les colonnes.

---

## ⚙️ Fonctionnalités
- **Trois niveaux de difficulté** : Débutant, Amateur, Expert
- **Chronomètre intégré**
- **Système d’indices**
- **Vérification automatique** de la solution

---

## Pré-requis
Assurez-vous d’avoir installé :
- [R](https://cran.r-project.org/)
- [RStudio](https://posit.co/download/rstudio-desktop/)

Packages R nécessaires :
- [`shiny`](https://cran.r-project.org/web/packages/shiny/index.html)
- [`shinydashboard`](https://cran.r-project.org/web/packages/shinydashboard/index.html)
- [`shinyWidgets`](https://cran.r-project.org/web/packages/shinyWidgets/index.html)
- [`shinyalert`](https://cran.r-project.org/web/packages/shinyalert/index.html)
- [`devtools`](https://cran.r-project.org/web/packages/devtools/index.html)


### Installation des packages
Dans la console R :
```R
install.packages(c("shiny", "shinydashboard", "shinyWidgets", "shinyalert", "devtools"))
```

## Lancer l’application

1. Clonez notre dépôt sur votre machine :

```bash
git clone https://github.com/riwahachem/Takuzu.git
```

2. Installation des packages
   
```R
setwd("~/Takuzu")
install.packages(c("shiny", "shinydashboard", "shinyWidgets", "shinyalert", "devtools"))
```

3. Installez le package localement avec devtools::install_local() :

```R
devtools::install_local("./Takuzu/TakuzuGame", force = TRUE)
```
4. Chargez le package :

```R
library(TakuzuGame)
```

4. Vous pouvez maintenant lancer l’application Takuzu 

```R
shiny::runApp("./Takuzu/TakuzuGame/app.R")
```

## Auteurs
- **Riwa Hachem Reda** - [riwahachemreda@gmail.com](mailto:riwahachemreda@gmail.com)
- **Wahel El Mazzouji** - [wahel.el-mazzouji@etu.umontpellier.fr](mailto:wahel.el-mazzouji@etu.umontpellier.fr)
