library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyalert)


# Charger les fichiers interface et serveur
source("R/interface.R")
source("R/serveur.R")

# Lancer l'application Shiny
shinyApp(ui = interface, server = server)

