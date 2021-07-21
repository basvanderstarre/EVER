library(shiny)
library(shinyjs)
library(shinydashboard)

library(readr)
library(dplyr)
library(plotly)
library(here)


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

buttons <- c('zoomIn2d', 'zoomOut2d', 'pan2d', 'select2d', 'lasso2d', 'sendDataToCloud', 'resetScale2d', 'hoverClosestCartesian', 'hoverCompareCartesian', 'toggleSpikelines')

# Script voor definitie van de UI
source("WW ui.R", local = TRUE)

# Zet UI op met header, sidebar en body
ui <- dashboardPage(header = header,
                    sidebar = sidebar,
                    body = body)

server <- function(input, output) {

    output$PROVINCIE <- renderUI({
        selectInput(inputId = "PROVINCIE",
                label = "Kies een provincie",
                choices = sort(unique(Data$Regio$PROVINCIE)))
    })
  
    output$WW <- renderPlotly({
        Weer_Wend <- Data$Weer_Wend %>%
          mutate(highlight = ifelse(PROVINCIE == input$PROVINCIE, "Geselecteerd", "Overig")) %>% 
          plot_ly(x = ~Delta_ExChurn, 
            y = ~Delta_groei,
            size = ~as.numeric(Inwonertal_52),
            color = ~highlight,
            colors = c("#F89730", "#FCEAD6"),
            type = "scatter", 
            mode = "markers",
            hoverinfo = "x+y+text", 
            text = ~GEMEENTE) %>%
        add_markers() %>%
        add_segments(x = 0, xend = 30, y = 1, yend = 1, line = list(dash = "dot", color = "#A6A6A6")) %>%
        add_segments(x = 1, xend = 1, y = 0, yend = 3, line = list(dash = "dot", color = "#A6A6A6")) %>%
        layout(showlegend = FALSE,
               separators = ',.',
               xaxis = list(type = "log", title = "Verschuiving samenstelling vacatures", zeroline = FALSE),
               yaxis = list(type = "log", title = "Verandering aantal vacatures", zeroline = FALSE)) %>%
        config(displaylogo = FALSE, modeBarButtonsToRemove = buttons)
      }) 
    
    output$BG <- renderPlotly({
      Data$Niveau1_Totaal %>%
        filter(PROVINCIE == input$PROVINCIE, Niveau1 != "Overig") %>%
        group_by(KWARTAAL, Niveau1) %>%
        summarise(AANTAL = round(sum(AANTAL), digits = 0)) %>%
        plot_ly(x = ~Niveau1,
          y = ~AANTAL,
          color = ~KWARTAAL,
          colors = c("#6ECEFF", "#006293"),
          name = ~KWARTAAL,
          type = "bar") %>%
        layout(barmode = "group",
          separators = ',.',
          legend = list(orientation = "h", title = "Kwartaal", x = 0.1, y = 100),
          xaxis = list(type = "category", title = ""),
          yaxis = list(title = "Aantal vacatures", separatethousands = TRUE, rangemode = "tozero")) %>%
        config(displaylogo = FALSE, modeBarButtonsToRemove = buttons)
    })
    
    output$TV <- renderPlotly({
      Data$Tijd_Vac %>%
        filter(PROVINCIE == input$PROVINCIE) %>%
        group_by(PEILDATUM) %>%
        summarise(AANTAL = sum(AANTAL)) %>%
        plot_ly(x = ~PEILDATUM,
          y = ~AANTAL,
          type = "scatter",
          mode = "lines",
          line = list(color = "#C0411D", width = 3)) %>%
        layout(separators = ',.',
          xaxis = list(title = "", type = "date", ticklabelmode = "period"),
          yaxis = list(title = "Aantal vacatures", separatethousands = TRUE)) %>%
        config(displaylogo = FALSE, modeBarButtonsToRemove = buttons)
    })
    
    output$Weer_NL <- renderText({
      paste("1 = ", round(Data$Weer_NL$NL_groei * 100, digits = 1), "%", sep = "")
    })
    
    output$Wend_NL <- renderText({
      paste("1 = ", round(Data$Wend_NL$NL_churn * 100, digits = 1), "%", sep = "")
    })
    
    output$Weer_Prov <- renderText({
      Prov <- Data$Weer_Provincie %>%
        filter(PROVINCIE == input$PROVINCIE)
      
      paste(round(Prov$Prov_groei * 100, digits = 1), "%", sep = "")
    })
    
    output$Weer_Prov <- renderText({
      Prov <- Data$Weer_Provincie %>%
        filter(PROVINCIE == input$PROVINCIE)
      
      paste(round(Prov$Prov_groei * 100, digits = 1), "%", sep = "")
    })
    
    output$Wend_Prov <- renderText({
      Prov <- Data$Wend_Provincie %>%
        filter(PROVINCIE == input$PROVINCIE)
      
      paste(round(Prov$Prov_churn * 100, digits = 1), "%", sep = "")
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)