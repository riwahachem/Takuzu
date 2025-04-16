library(shiny)
library(shinydashboard)
library(fresh)

# Création d'un thème personnalisé
mon_theme <- create_theme(
  adminlte_color(
    light_blue = "#81A1C1"
  ),
  adminlte_sidebar(
    dark_bg = "#2E3440",
    dark_hover_bg = "#3B4252",
    dark_color = "#D8DEE9"
  ),
  adminlte_global(
    content_bg = "#ECEFF4",
    box_bg = "#E5E9F0",
    info_box_bg = "#E5E9F0"
  )
)


interface <- dashboardPage(
  #dashboardHeader(title = "Takuzu Game"),
  dashboardHeader(
    title = tags$div(
      id = "go_home",  # ID pour détecter le clic
      style = "cursor: pointer; color: white;",
      "Takuzu Game"
    )
  ),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Jouer", tabName = "play"),
      menuItem("Règles", tabName = "règles"),
      menuItem("  À propos", tabName = "about", icon = icon("info-circle"))
    )
  ),
  dashboardBody(
    use_theme(mon_theme),
    tags$script( HTML("
                      $(document).on('click', '#go_home', function() {
                        Shiny.setInputValue('go_home', Math.random());
                      });
                      ")),
    tabItems(
      tabItem(tabName = "play",
              fluidRow(
                box(width = 4,
                    selectInput("niveau", "Niveau de difficulté", choices = c("Débutant", "Amateur", "Expert"), selected = "Amateur"),
                    actionButton("new_game", "Nouvelle Partie"),
                    br(), br(),
                    uiOutput("timer")
                ),
                box(width = 8,
                    div(style = "text-align: center;", uiOutput("grille_boutons")),
                    div(style = "margin-top: 20px; text-align: center;",
                        uiOutput("result")
                    ),
                    div(
                      style = "display: flex; justify-content: flex-end; gap: 10px; margin-top: 15px;",
                      actionButton("check_grid", "Vérifier", class = "btn-success"),
                      actionButton("hint", icon = icon("lightbulb"), "Indice", class = "btn-warning")
                    )
              )
            )
      ),
      tabItem(tabName = "règles",
              h3("Règles du jeu"),
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
