library(shiny)
library(shinyjs)
library(shinydashboard)

library(scales)
library(readr)
library(dplyr)
library(plotly)
library(here)
library(shinyBS)
library(shinycssloaders)
library(DT)

#---- Data loading ----
Data <- read_rds("Data_WW.rds")

# Regio <- Data$Regio
# Tijd_Vac <- Data$Tijd_Vac
# Niveau1_Totaal <- Data$Niveau1_Totaal
# Weer_Wend <- Data$Weer_Wend
# 
# rm(Data)
# 
# save.image("~/datasets.RData")

# load(here('datasets.RData'))

#---- Layout modellen voor Plotly ----

# Verbergen van volgende buttons:
buttons <- c('zoomIn2d', 'zoomOut2d', 'pan2d', 'select2d', 'lasso2d', 'sendDataToCloud', 'resetScale2d', 'hoverClosestCartesian', 'hoverCompareCartesian', 'toggleSpikelines')

# Script voor definitie van de UI
source("WW ui.R", local = TRUE)

# Zet UI op met header, sidebar en body
ui <- dashboardPage(header = header,
                    sidebar = sidebar,
                    body = body)

server <- function(input, output) {
  
  # Info tab
  source("infoApp.R", local = TRUE)

  # Vacature Tab
  source("vacatureApp.R", local = TRUE)
  
 }

# Run the application 
shinyApp(ui = ui, server = server)

