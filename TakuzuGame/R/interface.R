library(shiny)
library(shinydashboard)

interface <- dashboardPage(
  dashboardHeader(title = "Takuzu Game"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Jouer", tabName = "play", icon = icon("gamepad")),
      menuItem("Instructions", tabName = "instructions", icon = icon("question")),
      menuItem("À propos", tabName = "about", icon = icon("info-circle"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "play",
              fluidRow(
                box(width = 4, title = "Paramètres",
                    selectInput("grid_size", "Taille de la grille", choices = c(4, 6, 8), selected = 8),
                    selectInput("niveau", "Niveau de difficulté", choices = c("Facile", "Moyen", "Difficile", "Einstein"), selected = "Moyen"),
                    actionButton("new_game", "Nouvelle Partie"),
                    actionButton("check_grid", "Vérifier"),
                    textOutput("result"),
                    textOutput("timer")
                ),
                box(width = 8, title = "Grille de Jeu",
                    uiOutput("grille_boutons")
                )
              )
      ),
      tabItem(tabName = "instructions",
              h3("Instructions du Jeu Takuzu"),
              p("Le but du jeu est de remplir la grille en respectant les règles suivantes :"),
              tags$ul(
                tags$li("Chaque case doit contenir un 0 ou un 1."),
                tags$li("Chaque ligne et chaque colonne doivent contenir autant de 0 que de 1."),
                tags$li("Il est interdit d'avoir trois 0 ou trois 1 consécutifs dans une ligne ou une colonne."),
                tags$li("Deux lignes ou deux colonnes identiques sont interdites dans la même grille.")
              )
      ),
      tabItem(tabName = "about",
              box(
                width = 12,
                title = "À propos de cette application",
                status = "success",
                solidHeader = TRUE,
                p("Cette application a été développée par Wahel El Mazzouji et Riwa Hachem Reda dans le cadre d'un projet universitaire pour le cours de ",
                  em("Programmation R"), " sous la direction de Jean-Michel Marin.")
              )
      )
    )
  )
)

