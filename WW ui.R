library(shiny)
library(shinyjs)
library(shinydashboard)

# Data <- read_rds("Data_WW.rds")

source("infoUI.R", local = TRUE)
source("vacatureUI.R", local = TRUE)
source("css.R", local = TRUE)
#---- UI Algemeen ----

header <- dashboardHeader(title = "EVER - v0.2")

sidebar <- dashboardSidebar(id = "", sidebarMenu(
  menuItem("Info",
           tabName = "InfoTab"),
  menuItem("Vacatures",
           tabName = "Tab1")
  )
)

#---- UI Opmaak ----

body <- dashboardBody(
  shinyjs::useShinyjs(),
  tags$head(
    tags$style(
      HTML(css)
      )
    ),
  
  #---- UI Tabbladen ----
  tabItems(
    infoUI,
    vacatureUI
    )
  )


