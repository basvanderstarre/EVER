library(shiny)
library(shinyjs)
library(shinydashboard)

# Data <- read_rds("Data_WW.rds")

#---- UI Algemeen ----

header <- dashboardHeader(title = "EVER - v0.1")

sidebar <- dashboardSidebar(id = "", sidebarMenu(
  #geeft verschillende tabladen weer
  menuItem("Vacatures",
           tabName = "Tab1")
)
)

#---- UI Opmaak ----

body <- dashboardBody(
  shinyjs::useShinyjs(),
  tags$head(
    tags$style(
      HTML(
        '
/*font van de header*/
        .main-header .logo {
        font-weight: bold;
        font-size: 14px;
        }

/*kleur van de header*/
      .skin-blue .main-header .logo {
          background-color: #F89730;
      }

/*kleur van de header als je er met je muis over hovert*/
.skin-blue .main-header .logo:hover {
          background-color: #F89730;}

/*kleur van de toggle als je er met je muis over hovert*/
      .skin-blue .main-header .navbar .sidebar-toggle:hover{
                              background-color: #F89730;
                              }


/*kleur van het resterende deel van de header */
      .skin-blue .main-header .navbar {
                              background-color: #F89730;
      }

/*kleur van de sidebar*/
      .skin-blue .main-sidebar {
                              background-color: #002F49;
      }

/* kleur van active tab in de sidebar */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                                background-color: #004A6E;
                                }
/* kleur tab bij hover */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                                background-color: #004A6E;
                                }


/*kleur achtergrond dashboard*/
                                .content-wrapper, .right-side {
                                background-color: #EDEDED;
                                }



      .box{margin: 5px;}
      .box.box-solid.box-primary>.box-header{
    font-size: 10px;
  font-weight; bold;
      }
.box-header h3.box-title {
   font-weight: bold;
   font-size: 12px;
}

.box.box-solid.box-primary{
  font-size: 10px;
  text-align: left;
  font-weight: bold;
}
      .form-group, .selectize-control {
           margin-bottom: 5px;
      }
      .box-body {
          padding-bottom: 5px;
          padding-left: 5px !important;
          padding-right: 5px !important;
      }')
)
),
 


#---- UI Tabbladen ----
tabItem(
  tabName = "Tab1",
  
  fluidRow(
    box(
      title = "Selectie",
      uiOutput("PROVINCIE"),
      width = 12
    )
  ),
  
  fluidRow(
    box(
      title = "Weerbaarheid en wendbaarheid VRAAG NAAR ARBEID van gemeenten tussen 2020 Q1 en 2021 Q1 (tov Nederlands gemiddelde)",
      plotlyOutput("WW", height = 700),
      # height = "1000px",
      width = 6
      ),
    box(
      title = "Verandering in vraag naar beroepsgroepen tussen 2020 Q1 en 2021 Q1",
      plotlyOutput("TV", height = 250),
      plotlyOutput("BG", height = 450),
      width = 6
        )
      ),
  
  fluidRow(
    box(title = "NL gemiddelde verandering volume vacatures 2020 Q1 - 2021 Q1",
        htmlOutput("Weer_NL"),
        width = 3),
    box(title = "NL verandering samenstelling vacatures 2020 Q1 - 2021 Q1",
        htmlOutput("Wend_NL"),
        width = 3),
    box(title = "Verandering volume vacatures 2020 Q1 - 2021 Q1 in selectie",
        htmlOutput("Weer_Prov"),
        width = 3),
    box(title = "Verandering samenstelling vacatures 2020 Q1 - 2021 Q1 in selectie",
        htmlOutput("Wend_Prov"),
        width = 3)
  )
)
)
