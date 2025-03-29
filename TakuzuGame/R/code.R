#' @title Paramétrage du niveau pour une grille de Takuzu
#' @description
#' Cette fonction indique combien de cases seront retirées dans une grille 8×8
#' pour un certain niveau de difficulté.
#'
#' @param niveau Un texte parmi : "Débutant", "Amateur", "Intermédiaire" ou "Expert".
#' @return Un entier correspondant au nombre de cases à supprimer.
#' @export
#'
parametre <- function(niveau) {
  suppr_par_niveau <- list("Débutant" = 16,"Amateur" = 24,"Intermédiaire" = 32,"Expert" = 44)
  return(suppr_par_niveau[[niveau]])
}

#' @title Vérifier la validité d'une grille 
#'
#' @description
#' Cette fonction contrôle qu'une grille terminée satisfait
#' toutes les règles du jeu :
#' \enumerate{
#'   \item Il ne doit pas y avoir de \code{NA} (chaque case est un 0 ou un 1).
#'   \item Chaque ligne et chaque colonne contient autant de 0 que de 1.
#'   \item Aucune suite de trois chiffres identiques (000 ou 111).
#'   \item Toutes les lignes sont différentes entre elles, et toutes les colonnes sont différentes entre elles.
#' }
#'
#' @param grille Une matrice 8x8
#'
#' @return \code{TRUE} si la grille est valide, \code{FALSE} sinon.
#'
#' @export
#'
verifier <- function(grille) {
  # Pas de case vide
  if (any(is.na(grille))) {
    return(FALSE)
  }
  
  # Que des 0 et des 1
  valeurs_uniques <- unique(as.vector(grille))
  if (!all(valeurs_uniques %in% c(0, 1))) {
    return(FALSE)
  }
  
  # Autant de 0 que de 1
  for (i in 1:8) {
    ligne <- grille[i, ]
    colonne <- grille[, i]
    if ((sum(ligne == 0) != sum(ligne == 1)) || (sum(colonne == 0) != sum(colonne == 1))){
      return(FALSE)
    }
  }
  
  # Pas de 000, 111
  for (i in 1:8) {
    ligne <- grille[i, ]
    colonne <- grille[, i]
    for (k in 1:6) {
      if (ligne[k] == ligne[k + 1] && ligne[k] == ligne[k + 2]) {
        return(FALSE)
      }
      if (colonne[k] == colonne[k + 1] && colonne[k] == colonne[k + 2]) {
        return(FALSE)
      }
    }
  }
  
  # Pas de ligne/colonne identiques
  lignes_concat <- apply(grille, 1, paste, collapse = "")
  if (length(unique(lignes_concat)) != length(lignes_concat)) {
    return(FALSE)
  }
  colonnes_concat <- apply(grille, 2, paste, collapse = "")
  if (length(unique(colonnes_concat)) != length(colonnes_concat)) {
    return(FALSE)
  }
  
  return(TRUE)
}

#' @title Générer une grille de Takuzu complète (solution)
#'
#' @description
#' Cette fonction génère une grille 8×8 entièrement remplie,
#' qui respecte les règles du Takuzu.
#'
#' @details
#' L'algorithme utilise le backtracking pour remplir la grille
#' en vérifiant progressivement les contraintes.
#'
#' @return Une matrice 8×8 de 0 et 1, conforme aux règles de Takuzu
#' (et validée par \code{\link{verifier}}).
#'
#' @export
generer_grille_solution <- function() {
  grille <- matrix(NA, nrow = 8, ncol = 8)
  
  # Vérification locale
  peut_placer <- function(g, i, j, val) {
    g[i, j] <- val
    # Vérifie qu'on n'a pas déjà plus de 4 zeros ou 4 uns dans la ligne/colonne
    ligne   <- g[i, ]
    colonne <- g[, j]
    if (sum(ligne == 0, na.rm = TRUE) > 4 || sum(ligne == 1, na.rm = TRUE) > 4) {
      return(FALSE)
    }
    if (sum(colonne == 0, na.rm = TRUE) > 4 || sum(colonne == 1, na.rm = TRUE) > 4) {
      return(FALSE)
    }
    # Vérifie l'absence de 000 ou 111
    ligne_str   <- paste(ligne, collapse = "")
    colonne_str <- paste(colonne, collapse = "")
    if (grepl("000|111", ligne_str) || grepl("000|111", colonne_str)) {
      return(FALSE)
    }
    return(TRUE)
  }
  
  # Fonction récursive
  remplir_case <- function(index) {
    if (index > 64) {
      # Vérification finale
      if (verifier(grille)) {
        return(TRUE)
      } else {
        return(FALSE)
      }
    }
    i <- ((index - 1) %/% 8) + 1
    j <- ((index - 1) %%  8) + 1
    
    for (val in c(0, 1)) {
      if (peut_placer(grille, i, j, val)) {
        grille[i, j] <<- val
        if (remplir_case(index + 1)) {
          return(TRUE)
        }
        grille[i, j] <<- NA
      }
    }
    return(FALSE)
  }
  
  # Lancement du backtracking
  success <- remplir_case(1)
  if (!success) {
    stop("Impossible de générer une grille solution (cas très rare).")
  }
  
  return(grille)
}

#' @title Créer un puzzle de Takuzu
#'
#' @description
#' Cette fonction génère une grille 8×8 incomplète
#' en supprimant un nombre de cases (remplacées par \code{NA})
#' selon le niveau de difficulté.
#'
#' @param niveau Une chaîne de caractères parmi : "débutant", "amateur",
#' "intermédiaire" ou "expert". Voir \code{\link{parametre}}.
#'
#' @return Une matrice 8×8 partiellement remplie de 0, 1 et \code{NA},
#' selon le niveau choisi.
#'
#' @export
creer_puzzle <- function(niveau = "débutant") {
  # Générer une grille solution
  sol <- generer_grille_solution()
  
  # Nombre de cases à retirer
  nb_a_supprimer <- parametre(niveau)
  
  # Retirer aléatoirement nb_a_supprimer positions
  indices_tous  <- 1:64
  indices_suppr <- sample(indices_tous, size = nb_a_supprimer, replace = FALSE)
  
  puzzle <- sol
  for (idx in indices_suppr) {
    i <- ((idx - 1) %/% 8) + 1
    j <- ((idx - 1) %%  8) + 1
    puzzle[i, j] <- NA
  }
  
  return(puzzle)
}
