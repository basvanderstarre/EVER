library(shinyBS)
library(shinycssloaders)

vacatureUI <- tabItem(
  tabName = "Tab1",
  fluidRow(
    box(
      # title = "Selectie",
      title = tags$p("Kies een provincie", style = "font-size: 150%;"),
      uiOutput("PROVINCIE"),
      width = 12,
    )
  ),
  fluidRow(column(width = 6,
    box(
      title = tags$p("Weerbaarheid en wendbaarheid VRAAG NAAR ARBEID van gemeenten tussen 2020 Q1 en 2021 Q1", style = "font-size: 150%;"),
      
      plotlyOutput("WW", height = 805),
      # height = "1000px",
      width = NULL,
      # height = 1000,
      actionButton("info_WW", "", icon = icon('info')),
      bsModal(id = "grafiek_1_info",
              title = "Info grafiek",
              trigger = "info_WW",
              size = "small",
              htmlOutput("text_for_WW")),
      div(id = "table_WW",
          style = "display:inline-block",
          actionButton("data_WW",
                       "",
                       icon = icon('table'))),
      bsModal(id = "grafiek_1_tabel",
              title = "Data grafiek",
              trigger = "data_WW",
              size = "large",
              withSpinner(dataTableOutput("table_for_WW")))
    )),
    column(width=6,
           box(
             title = tags$p("Verandering in vraag naar beroepsgroepen tussen 2020 Q1 en 2021 Q1", style = "font-size: 150%;"),
             plotlyOutput("TV", height = 315),
             width = NULL,
             actionButton("info_TV", "", icon = icon('info')),
             bsModal(id = "grafiek_2_info",
                             title = "Info grafiek",
                             trigger = "info_TV",
                             size = "small",
                             htmlOutput("text_for_TV")),
             div(id = "table_TV",
                 style = "display:inline-block",
                 actionButton("data_TV",
                              "",
                              icon = icon('table'))),
             bsModal(id = "grafiek_2_tabel",
                     title = "Data grafiek",
                     trigger = "data_TV",
                     size = "large",
                     withSpinner(dataTableOutput("table_for_TV")))),
           box(
             title = tags$p("Verandering in vraag naar beroepsgroepen tussen 2020 Q1 en 2021 Q1", style = "font-size: 150%;"),
             plotlyOutput("BG", height = 390),
             width = NULL,
             actionButton("info_BG", "", icon = icon('info')),
             bsModal(id = "grafiek_3_info",
                     title = "Info grafiek",
                     trigger = "info_BG",
                     size = "small",
                     htmlOutput("text_for_BG")),
             div(id = "table_BG",
                 style = "display:inline-block",
                 actionButton("data_BG",
                              "",
                              icon = icon('table'))),
      bsModal(id = "grafiek_3_tabel",
              title = "Data grafiek",
              trigger = "data_BG",
              size = "large",
              withSpinner(dataTableOutput("table_for_BG")))
    )
    )
  )

)
