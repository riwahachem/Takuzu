logique <- function(input, output, session) {
  source("R/code.R")

  nRows <- 8
  nCols <- 8

  niveau <- reactive({ input$niveau })
  debut_temps <- reactiveVal(NULL)
  depart_chrono <- reactiveVal(FALSE)

  rv <- reactiveValues(grille = NULL, verrouillees = NULL)

  observeEvent(input$new_game, {
    debut_temps(Sys.time())
    depart_chrono(TRUE)
    showNotification(paste("Nouvelle partie - Niveau :", niveau(), "- Taille : 8x8"), type = "message")
    grille_init <- generer_takuzu(nRows, niveau())
    rv$grille <- grille_init
    rv$verrouillees <- !is.na(grille_init)
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
          actionButton(inputId = paste("bouton", i, j, sep = "_"),
                       label = ifelse(is.na(valeur_case), "", as.character(valeur_case)),
                       style = "width: 50px; height: 50px; font-size: 18px; margin: 5px;",
                       disabled = rv$verrouillees[i, j])
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
