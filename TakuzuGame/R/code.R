choisir_difficulte <- function(taille, niveau) {
  # Initialisation du nombre de cases à remplir
  nb_cases <- 0
  if (niveau == "Facile") {
    nb_cases <- taille * taille * 0.9
  } else if (niveau == "Normal") {
    nb_cases <- taille * taille * 0.4
  } else if (niveau == "Difficile") {
    nb_cases <- taille * taille * 0.2
  } else {
    stop("Veuillez choisir un niveau parmi : Facile, Normal, Difficile")
  }

  return(round(nb_cases))
}
generer_takuzu <- function(taille, niveau) {
  nb_cases_prepremplies <- choisir_difficulte(taille, niveau)
  grille <- matrix(NA, nrow = taille, ncol = taille)

  est_valide <- function(grille, i, j, val) {
    # Appliquer temporairement la valeur pour les tests
    grille[i, j] <- val
    ligne <- grille[i, ]
    colonne <- grille[, j]

    # Réinitialiser pour ne pas affecter l'état de la grille à l'extérieur
    grille[i, j] <- NA

    # Vérifier équilibre 0/1
    if (sum(ligne == 0, na.rm = TRUE) > taille / 2 || sum(ligne == 1, na.rm = TRUE) > taille / 2) return(FALSE)
    if (sum(colonne == 0, na.rm = TRUE) > taille / 2 || sum(colonne == 1, na.rm = TRUE) > taille / 2) return(FALSE)

    # Vérifier absence de trois chiffres consécutifs
    check_rle <- function(vec) {
      runs <- rle(vec)
      return(!any(runs$lengths > 2, na.rm = TRUE))
    }

    if (!check_rle(ligne) || !check_rle(colonne)) return(FALSE)

    # Vérifier l'unicité des lignes complètes
    if (all(!is.na(ligne))) {
      # Convertir la ligne actuelle en chaîne pour comparaison
      ligne_str <- paste(ligne, collapse = "")
      # Vérifier si cette ligne existe déjà ailleurs dans la grille
      for (k in 1:taille) {
        if (k != i && all(!is.na(grille[k, ]))) {
          autre_ligne_str <- paste(grille[k, ], collapse = "")
          if (ligne_str == autre_ligne_str) return(FALSE)
        }
      }
    }

    # Vérifier l'unicité des colonnes complètes
    if (all(!is.na(colonne))) {
      # Convertir la colonne actuelle en chaîne pour comparaison
      colonne_str <- paste(colonne, collapse = "")
      # Vérifier si cette colonne existe déjà ailleurs dans la grille
      for (k in 1:taille) {
        if (k != j && all(!is.na(grille[, k]))) {
          autre_colonne_str <- paste(grille[, k], collapse = "")
          if (colonne_str == autre_colonne_str) return(FALSE)
        }
      }
    }

    return(TRUE)
  }

  remplir_grille <- function(i, j) {
    if (i > taille) return(TRUE)  # Fin de la grille

    next_i <- ifelse(j == taille, i + 1, i)
    next_j <- ifelse(j == taille, 1, j + 1)

    if (!is.na(grille[i, j])) return(remplir_grille(next_i, next_j))

    valeurs <- sample(c(0, 1))  # Choix aléatoire

    for (val in valeurs) {
      if (est_valide(grille, i, j, val)) {
        grille[i, j] <<- val
        if (remplir_grille(next_i, next_j)) return(TRUE)
        grille[i, j] <<- NA  # Backtrack
      }
    }
    return(FALSE)
  }

  remplir_grille(1, 1)

  # Retirer des cases pour correspondre au niveau
  indices <- sample(1:(taille^2), taille^2 - nb_cases_prepremplies)
  grille[indices] <- NA

  return(grille)
}
verifier_takuzu <- function(grille) {
  taille <- ncol(grille)

  # Vérifie si la grille contient uniquement des 0 et des 1
  if (!all(grille %in% c(0, 1))) return(FALSE)

  # Vérifie si chaque ligne et colonne a exactement la moitié de 0 et de 1
  if (any(rowSums(grille) != taille / 2) || any(colSums(grille) != taille / 2)) return(FALSE)

  # Vérifie s'il y a plus de 2 chiffres identiques consécutifs
  verification_suite <- function(vec) {
    for (i in seq_len(length(vec) - 2)) {
      if (vec[i] == vec[i + 1] && vec[i] == vec[i + 2]) return(FALSE)
    }
    return(TRUE)
  }

  # Vérifie si toutes les lignes/colonnes respectent la règle de non-répétition de plus de 2 chiffres identiques
  if (!all(apply(grille, 1, verification_suite)) || !all(apply(grille, 2, verification_suite))) return(FALSE)

  # Vérifie l'unicité des lignes et colonnes
  verification_unicite <- function(mat) {
    lignes <- apply(mat, 1, paste, collapse = "")
    return(length(unique(lignes)) == nrow(mat))
  }

  # Vérifie que toutes les lignes sont différentes les unes des autres
  # et que toutes les colonnes sont différentes les unes des autres
  if (!verification_unicite(grille) || !verification_unicite(t(grille))) return(FALSE)

  return(TRUE)
}
donner_indice <- function(grille) {
  taille <- nrow(grille)
  indices <- list()

  # Fonction pour vérifier s'il y a deux chiffres identiques consécutifs
  verifier_paire <- function(ligne, valeur) {
    for (i in 1:(length(ligne) - 1)) {
      if (!is.na(ligne[i]) && !is.na(ligne[i + 1]) &&
          ligne[i] == valeur && ligne[i + 1] == valeur) {
        # Si on trouve une paire, on regarde à gauche et à droite
        if (i > 1 && is.na(ligne[i - 1])) {
          return(c(i - 1, 1 - valeur))  # Position et valeur à placer
        } else if (i < length(ligne) - 1 && is.na(ligne[i + 2])) {
          return(c(i + 2, 1 - valeur))  # Position et valeur à placer
        }
      }
    }
    return(NULL)
  }

  # Vérifier l'équilibre 0/1 dans chaque ligne et colonne
  verifier_equilibre <- function(ligne) {
    if (sum(ligne == 0, na.rm = TRUE) == taille / 2) {
      # Si on a déjà atteint le max de 0, les cases NA doivent être 1
      for (i in 1:length(ligne)) {
        if (is.na(ligne[i])) {
          return(c(i, 1))
        }
      }
    } else if (sum(ligne == 1, na.rm = TRUE) == taille / 2) {
      # Si on a déjà atteint le max de 1, les cases NA doivent être 0
      for (i in 1:length(ligne)) {
        if (is.na(ligne[i])) {
          return(c(i, 0))
        }
      }
    }
    return(NULL)
  }

  # Chercher les paires dans les lignes
  for (i in 1:taille) {
    # Chercher les paires de 0
    resultat <- verifier_paire(grille[i, ], 0)
    if (!is.null(resultat)) {
      return(list(type = "ligne", indice = paste("Dans la ligne", i, "à la position", resultat[1],
                                                 "vous devez placer un", resultat[2],
                                                 "pour éviter trois 0 consécutifs.")))
    }

    # Chercher les paires de 1
    resultat <- verifier_paire(grille[i, ], 1)
    if (!is.null(resultat)) {
      return(list(type = "ligne", indice = paste("Dans la ligne", i, "à la position", resultat[1],
                                                 "vous devez placer un", resultat[2],
                                                 "pour éviter trois 1 consécutifs.")))
    }

    # Vérifier l'équilibre dans la ligne
    resultat <- verifier_equilibre(grille[i, ])
    if (!is.null(resultat)) {
      return(list(type = "ligne", indice = paste("Dans la ligne", i, "à la position", resultat[1],
                                                 "vous devez placer un", resultat[2],
                                                 "car vous avez déjà le maximum de", 1 - resultat[2], "dans cette ligne.")))
    }
  }

  # Chercher les paires dans les colonnes
  for (j in 1:taille) {
    # Chercher les paires de 0
    resultat <- verifier_paire(grille[, j], 0)
    if (!is.null(resultat)) {
      return(list(type = "colonne", indice = paste("Dans la colonne", j, "à la position", resultat[1],
                                                   "vous devez placer un", resultat[2],
                                                   "pour éviter trois 0 consécutifs.")))
    }

    # Chercher les paires de 1
    resultat <- verifier_paire(grille[, j], 1)
    if (!is.null(resultat)) {
      return(list(type = "colonne", indice = paste("Dans la colonne", j, "à la position", resultat[1],
                                                   "vous devez placer un", resultat[2],
                                                   "pour éviter trois 1 consécutifs.")))
    }

    # Vérifier l'équilibre dans la colonne
    resultat <- verifier_equilibre(grille[, j])
    if (!is.null(resultat)) {
      return(list(type = "colonne", indice = paste("Dans la colonne", j, "à la position", resultat[1],
                                                   "vous devez placer un", resultat[2],
                                                   "car vous avez déjà le maximum de", 1 - resultat[2], "dans cette colonne.")))
    }
  }

  # Si aucun indice spécifique n'est trouvé, donner un indice générique
  cases_vides <- which(is.na(grille), arr.ind = TRUE)
  if (nrow(cases_vides) > 0) {
    case_aleatoire <- cases_vides[sample(nrow(cases_vides), 1), ]
    return(list(type = "general", indice = paste("Essayez de remplir la case à la ligne",
                                                 case_aleatoire[1], "et à la colonne", case_aleatoire[2],
                                                 "en vous basant sur les règles du jeu.")))
  }

  return(list(type = "aucun", indice = "La grille semble complète. Vérifiez si elle respecte toutes les règles du jeu."))
}
