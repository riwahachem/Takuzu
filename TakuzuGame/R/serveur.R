library(shiny)
source("R/code.R")
server <- function(input, output, session){
  # on la stocke dans un reactiveVal pour qu'elle puisse changer dynamiquement
  puzzle <- reactiveVal()

  observeEvent(input$generer, {
    nouveau_puzzle <- creer_puzzle(input$niveau)
    puzzle(nouveau_puzzle)
  })

  output$grillePuzzle <- renderTable({
    p <- puzzle()
    if (is.null(p)) {
      return(NULL)
    }
    p
  }, rownames = TRUE)

  observeEvent(input$verifier, {
    p <- puzzle()
    if (is.null(p)) {
      output$resultat <- renderText("Aucune grille chargée. Cliquez sur 'Générer'.")
      return()
    }
    est_valide <- verifier(p)
    if (est_valide) {
      output$resultat <- renderText("Bravo, la grille est valide !")
    } else {
      output$resultat <- renderText("Cette grille n'est pas valide (ou pas complète).")
    }
  })
}
