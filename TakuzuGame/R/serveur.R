
ligne_valide <- function(vec) {
  nRows <- 8
  nCols <- 8
  if (any(is.na(vec))) return(FALSE)
  sum(vec == 0) == nCols / 2 &&
    sum(vec == 1) == nCols / 2 &&
    all(rle(vec)$lengths <= 2)
}

logique <- function(input, output, session) {
  source("R/code.R")

  nRows <- 8
  nCols <- 8

  niveau <- reactive({ input$niveau })
  debut_temps <- reactiveVal(NULL)
  depart_chrono <- reactiveVal(FALSE)

  rv <- reactiveValues(grille = NULL, verrouillees = NULL, lignes_valides = rep(FALSE, nRows),
                       colonnes_valides = rep(FALSE, nCols))

  observeEvent(input$new_game, {
    debut_temps(Sys.time())
    depart_chrono(TRUE)
    showNotification(paste("Nouvelle partie - Niveau :", niveau(), "- Taille : 8x8"), type = "message")
    grille_init <- generer_takuzu(nRows, niveau())
    rv$grille <- grille_init
    rv$verrouillees <- !is.na(grille_init)

    rv$lignes_valides <- rep(FALSE, nRows)
    rv$colonnes_valides <- rep(FALSE, nCols)

    output$result <- renderText("Nouvelle partie commencée ! Bonne chance ")
  })

  output$timer <- renderText({
    req(debut_temps(), depart_chrono())
    invalidateLater(1000, session)
    temps_ecoule <- difftime(Sys.time(), debut_temps(), units = "secs")
    paste("Temps écoulé :", round(temps_ecoule), "secondes")
  })

  output$grille_boutons <- renderUI({
    boutons <- lapply(1:nRows, function(i) {
      fluidRow(
        lapply(1:nCols, function(j) {
          valeur_case <- rv$grille[i, j]
          couleur_fond <- ""
          if (rv$lignes_valides[i] || rv$colonnes_valides[j]) {
            couleur_fond <- "background-color: #d4edda;"
          }

          actionButton(
            inputId = paste("bouton", i, j, sep = "_"),
            label = ifelse(is.na(valeur_case), "", as.character(valeur_case)),
            style = paste0("width: 50px; height: 50px; font-size: 18px; margin: 5px;", couleur_fond),
            disabled = rv$verrouillees[i, j]
          )
        })
      )
    })
    tagList(boutons)
  })

  observe({
    lapply(1:nRows, function(i) {
      lapply(1:nCols, function(j) {
        observeEvent(input[[paste("bouton", i, j, sep = "_")]], {
          valeur_actuelle <- rv$grille[i, j]
          if (is.na(valeur_actuelle)) {
            valeur_nouvelle <- 0
          } else if (valeur_actuelle == 0) {
            valeur_nouvelle <- 1
          } else {
            valeur_nouvelle <- NA
          }
          rv$grille[i, j] <- valeur_nouvelle

          # Mise à jour des lignes/colonnes valides
          rv$lignes_valides[i] <- ligne_valide(rv$grille[i, ])
          rv$colonnes_valides[j] <- ligne_valide(rv$grille[, j])

          updateActionButton(
            session,
            paste("bouton", i, j, sep = "_"),
            label = ifelse(is.na(valeur_nouvelle), "", as.character(valeur_nouvelle))
          )
        })
      })
    })
  })

  observeEvent(input$check_grid, {
    if (verifier_takuzu(rv$grille)) {
      depart_chrono(FALSE)
      delta_temps <- difftime(Sys.time(), debut_temps(), units = "secs")
      output$timer <- renderText({paste("Temps écoulé :", round(delta_temps), "secondes")})
      output$result <- renderText("🎉 Bravo, vous avez réussi !")
    } else {
      output$result <- renderText("La grille n'est pas bonne, réessayez !")
    }
  })
  observeEvent(input$hint, {
    # Vérifier si une partie est en cours
    if (is.null(rv$grille)) {
      showNotification("Commencez une nouvelle partie pour obtenir un indice", type = "warning")
      return()
    }

    # Obtenir un indice
    resultat_indice <- donner_indice(rv$grille)

    # Si un indice spécifique est trouvé, le mettre en évidence
    if (resultat_indice$type %in% c("ligne", "colonne")) {
      # Afficher l'indice
      showModal(modalDialog(
        title = "Indice",
        resultat_indice$indice,
        easyClose = TRUE,
        footer = modalButton("Compris !")
      ))
    } else {
      # Indice général
      showModal(modalDialog(
        title = "Indice",
        resultat_indice$indice,
        easyClose = TRUE,
        footer = modalButton("Compris !")
      ))
    }
  })

}
