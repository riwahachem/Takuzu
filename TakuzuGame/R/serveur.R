
# Logique du serveur
logique <- function(input, output, session) {
  source("R/code.R")

  # Ajouter une valeur réactive pour le thème
  theme_actuel <- reactiveVal("clair")

  # Observer le changement de thème
  observeEvent(input$switch_theme, {
    if(input$switch_theme) {
      theme_actuel("sombre")
    } else {
      theme_actuel("clair")
    }
  })

  # Générer dynamiquement le theme
  output$theme_selector <- renderUI({
    if(theme_actuel() == "clair") {
      use_theme(theme_clair)
    } else {
      use_theme(theme_sombre)
    }
  })

  # Appliquer des styles CSS supplémentaires selon le thème
  observe({
    if(theme_actuel() == "sombre") {
      shinyjs::addClass(selector = "body", class = "dark-mode")
    } else {
      shinyjs::removeClass(selector = "body", class = "dark-mode")
    }
  })

  nRows <- 8
  nCols <- 8

  niveau <- reactive({ input$niveau })
  debut_temps <- reactiveVal(NULL)
  depart_chrono <- reactiveVal(FALSE)
  meilleurs_temps <- reactiveValues(
    Débutant = Inf,
    Amateur = Inf,
    Expert = Inf
  )
  meilleur_actuel <- reactive({
    switch(niveau(),
           "Débutant" = meilleurs_temps$Débutant,
           "Amateur" = meilleurs_temps$Amateur,
           "Expert" = meilleurs_temps$Expert)
  })

  rv <- reactiveValues(grille = NULL, verrouillees = NULL)

  observeEvent(input$new_game, {
    debut_temps(Sys.time())
    depart_chrono(TRUE)
    showNotification(paste("Nouvelle partie - Niveau :", niveau(), "- Taille : 8x8"), type = "message")
    grille_init <- generer_takuzu(nRows, niveau())
    rv$grille <- grille_init
    rv$verrouillees <- !is.na(grille_init)

    output$result <- renderUI({
      HTML("<p style='font-size: 25px; font-weight: bold;'> C'est parti, bonne chance ! </p>")
    })
  })

  output$timer <- renderUI({
    req(debut_temps(), depart_chrono())
    invalidateLater(1000, session)
    temps_ecoule <- round(difftime(Sys.time(), debut_temps(), units = "secs"))

    texte <- paste0("⏱️ Temps écoulé : ", temps_ecoule, " secondes")

    if (!is.infinite(meilleur_actuel())) {
      texte <- paste0(texte, "<br>🏆 Meilleur temps : ", meilleur_actuel(), " secondes")
    }

    HTML(paste0("<p style='margin: 0;'>", texte, "</p>"))
  })

  output$grille_boutons <- renderUI({
    boutons <- lapply(1:nRows, function(i) {
      fluidRow(
        lapply(1:nCols, function(j) {
          valeur_case <- rv$grille[i, j]
          button_class <- "grid-button"  # Classe CSS personnalisée pour les boutons de la grille

          actionButton(
            inputId = paste("bouton", i, j, sep = "_"),
            label = ifelse(is.na(valeur_case), "", as.character(valeur_case)),
            style = "width: 50px; height: 50px; font-size: 18px; margin: 5px;",
            class = button_class,
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

          updateActionButton(
            session,
            paste("bouton", i, j, sep = "_"),
            label = ifelse(is.na(valeur_nouvelle), "", as.character(valeur_nouvelle))
          )

          # Vérifier automatiquement si la grille est complète
          if (all(!is.na(rv$grille))) {
            if (verifier_takuzu(rv$grille)) {
              depart_chrono(FALSE)
              delta <- round(as.numeric(difftime(Sys.time(), debut_temps(), units = "secs")))
              niv <- niveau()

              if (delta < meilleurs_temps[[niv]]) {
                meilleurs_temps[[niv]] <- delta
              }

              shinyjs::runjs("
                $('#result').hide().fadeIn(800).css({'transform': 'scale(1.1)', 'transition': 'all 0.3s ease-in-out'});
                setTimeout(function(){
                  $('#result').css({'transform': 'scale(1)'});
                }, 500);
              ")

              # Animation de victoire
              shinyjs::runjs("$('#result_zone').removeClass().addClass('zoomIn');")

              output$result <- renderUI({
                HTML("<p style='font-size: 25px; font-weight: bold; color: green;'>🎉 Grille complétée ! Bravo !</p>")
              })
            } else {
              # Animation de défaite
              shinyjs::runjs("$('#result_zone').removeClass().addClass('shake');")

              output$result <- renderUI({
                HTML("<p style='font-size: 25px; font-weight: bold; color: #C0392B;'>⛔ La grille est pleine mais incorrecte. Réessayez !</p>")
              })
            }
          }
        })
      })
    })
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

  observeEvent(input$go_home, {
    updateTabItems(session, "tabs", selected = "play")
  })
}
