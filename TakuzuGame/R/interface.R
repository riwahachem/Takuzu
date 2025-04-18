library(shiny)
library(shinydashboard)
library(fresh)
library(shinyjs)
library(shinyWidgets)

# Création de deux thèmes personnalisés
theme_clair <- create_theme(
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

theme_sombre <- create_theme(
  adminlte_color(
    light_blue = "#5E81AC"
  ),
  adminlte_sidebar(
    dark_bg = "#1A1A1A",
    dark_hover_bg = "#2D2D2D",
    dark_color = "#D8DEE9"
  ),
  adminlte_global(
    content_bg = "#121212",
    box_bg = "#1E1E1E",
    info_box_bg = "#1E1E1E"
  ),
  # Styles supplémentaires pour le mode sombre
  bs4dash_status(
    primary = "#5E81AC",
    success = "#A3BE8C",
    info = "#88C0D0",
    warning = "#EBCB8B",
    danger = "#BF616A"
  )
)

# Interface utilisateur
interface <- dashboardPage(
  dashboardHeader(
    title = tags$div(
      id = "go_home",
      style = "cursor: pointer; color: white;",
      "Takuzu Game"
    ),
    # Ajouter le bouton de changement de thème
    tags$li(
      class = "dropdown",
      materialSwitch(
        inputId = "switch_theme",
        label = "Mode sombre",
        status = "primary",
        right = TRUE
      )
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
    uiOutput("theme_selector"), # Pour changer le thème dynamiquement
    useShinyjs(),
    tags$head(
      tags$style(HTML("
        .zoomIn {
          animation: zoomIn 0.5s ease forwards;
        }
        @keyframes zoomIn {
          from {transform: scale(0.8); opacity: 0;}
          to {transform: scale(1); opacity: 1;}
        }

        .shake {
          animation: shake 0.7s ease-in-out;
        }
        @keyframes shake {
          0% { transform: translateX(0px); }
          15% { transform: translateX(-10px); }
          30% { transform: translateX(10px); }
          45% { transform: translateX(-10px); }
          60% { transform: translateX(10px); }
          75% { transform: translateX(-5px); }
          90% { transform: translateX(5px); }
          100% { transform: translateX(0px); }
        }

        /* Styles pour le mode sombre */
        .dark-mode .box {
          background-color: #1E1E1E !important;
          color: #D8DEE9 !important;
        }

        .dark-mode .box-header {
          background-color: #2D2D2D !important;
          color: #D8DEE9 !important;
        }

        .dark-mode .content-wrapper {
          background-color: #121212 !important;
        }

        .dark-mode .main-header {
          background-color: #1A1A1A !important;
        }

        .dark-mode .btn {
          background-color: #3B4252;
          color: #D8DEE9;
        }

        .dark-mode .btn:hover {
          background-color: #4C566A;
        }

        .dark-mode .form-control {
          background-color: #2D2D2D !important;
          color: #D8DEE9 !important;
          border-color: #3B4252 !important;
        }

        .dark-mode .selectize-input {
          background-color: #2D2D2D !important;
          color: #D8DEE9 !important;
          border-color: #3B4252 !important;
        }

        .dark-mode .selectize-dropdown {
          background-color: #2D2D2D !important;
          color: #D8DEE9 !important;
        }

        .dark-mode .selectize-dropdown-content .option {
          background-color: #2D2D2D !important;
          color: #D8DEE9 !important;
        }

        .dark-mode .selectize-dropdown-content .option.active {
          background-color: #4C566A !important;
        }

        .dark-mode label {
          color: #D8DEE9 !important;
        }

        .dark-mode .modal-content {
          background-color: #1E1E1E !important;
          color: #D8DEE9 !important;
        }

        .dark-mode .modal-header {
          background-color: #2D2D2D !important;
          color: #D8DEE9 !important;
          border-bottom: 1px solid #3B4252 !important;
        }

        .dark-mode .modal-footer {
          border-top: 1px solid #3B4252 !important;
        }

        /* Style pour les boutons de la grille en mode sombre */
        .dark-mode .grid-button {
          background-color: #2D2D2D !important;
          color: #D8DEE9 !important;
          border: 1px solid #3B4252 !important;
        }

        .dark-mode .grid-button:hover {
          background-color: #3B4252 !important;
        }

        .dark-mode .grid-button:disabled {
          background-color: #4C566A !important;
          color: #D8DEE9 !important;
        }
      "))
    ),
    tags$script(HTML("
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
                    div(id = "result_zone", style = "margin-top: 25px; text-align: center;", uiOutput("result")),
                    div(
                      style = "display: flex; justify-content: flex-end; gap: 10px; margin-top: 15px;",
                      actionButton("hint", icon = icon("lightbulb"), "Indice", class = "btn-warning")
                    )
                )
              )
      ),
      tabItem(tabName = "règles",
              box(
                width = 12,
                title = div(style = "font-size: 22px; font-weight: bold;", "Règles du jeu"),
                status = "primary",
                solidHeader = TRUE,
                style = "font-size: 18px; line-height: 1.8; padding: 15px;",
                p("Le but du jeu est de remplir la grille en respectant les règles suivantes :"),
                tags$ul(
                  tags$li("Chaque case doit contenir un 0 ou un 1."),
                  tags$li("Chaque ligne et chaque colonne doivent contenir autant de 0 que de 1."),
                  tags$li("Il est interdit d'avoir trois 0 ou trois 1 consécutifs dans une ligne ou une colonne."),
                  tags$li("Deux lignes ou deux colonnes identiques sont interdites dans la même grille.")
                )
              )
      ),
      tabItem(tabName = "about",
              box(
                width = 12,
                title = "À propos de cette application",
                status = "success",
                solidHeader = TRUE,
                p("Cette application a été développée par ",
                  strong("Wahel El Mazzouji"), " et ",
                  strong("Riwa Hachem Reda"), " dans le cadre d'un projet universitaire pour le cours de ",
                  em("Programmation R"), " sous la direction de ",
                  strong("Jean-Michel Marin.")),
                p("L'objectif de ce projet est de fournir une interface interactive pour jouer au Takuzu tout en respectant les règles du jeu."),
                p("Technologies utilisées : ",
                  span(class = "label label-default", "R"), ", ",
                  span(class = "label label-default", "Shiny"), ", ",
                  span(class = "label label-default", "shinydashboard")),
                p("Pour toute question ou retour, veuillez nous contacter aux adresses suivantes :"),
                p(strong("E-mails : "),
                  "riwahachemreda@gmail.com", ", ",
                  "wahel.el-mazzouji@etu.umontpellier.fr")
              )
      )
    )
  )
)
