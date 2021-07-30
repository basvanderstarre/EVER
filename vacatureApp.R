tegelSelectie <- renderUI({
  selectInput(inputId = "PROVINCIE",
              label = NULL,
              choices = sort(unique(Data$Regio$PROVINCIE)))
})

  
WW <- renderPlotly({
  Weer_Wend <- Data$Weer_Wend %>%
    mutate(highlight = ifelse(PROVINCIE %in% input$PROVINCIE, "Geselecteerd", "Overig")) %>%
    plot_ly(x = ~Reg_ExChurn,
            y = ~Reg_groei,
            size = ~as.numeric(Inwonertal_52),
            color = ~highlight,
            colors = c("#8F4C05", "#FCD5AC"),
            type = "scatter",
            mode = "markers",
            hoverinfo = "x+y+text",
            text = ~GEMEENTE,
            fill = ~'') %>%
    # add_markers() %>%
    add_segments(x = 0,
                 xend = max(Data$Weer_Wend$Reg_ExChurn) + 0.025,
                 y = mean(Data$Weer_Wend$NL_groei),
                 yend = mean(Data$Weer_Wend$NL_groei),
                 line = list(dash = "dot",
                             color = "#A6A6A6"),
                 hoverinfo='skip',
                 text = paste("NL gemiddelde weerbaarheid:", Data$Weer_Wend$NL_groei[1])) %>%
    add_segments(x = mean(Data$Weer_Wend$NL_churn)-mean(Data$Weer_Wend$NL_groei),
                 xend = mean(Data$Weer_Wend$NL_churn)-mean(Data$Weer_Wend$NL_groei),
                 y = min(Data$Weer_Wend$Reg_groei) - 0.025,
                 yend = max(Data$Weer_Wend$Reg_groei) + 0.025,
                 line = list(dash = "dot",
                             color = "#A6A6A6"),
                 hoverinfo='skip',
                 text = paste("NL gemiddelde wendbaarheid:", Data$Weer_Wend$NL_groei[1])) %>%
    add_annotations(x = 0.4,
                    y = mean(Data$Weer_Wend$NL_groei),
                    text = paste('Weerbaarheid in heel Nederland:', percent(Data$Weer_Wend$NL_groei[1])),
                    showarrow = FALSE,
                    # arrowhead = 6,
                    xref = "x",
                    yref = "y",
                    arrowsize = 0.7,
                    ax = 13,
                    ay = 40,
                    xanchor = "left",
                    yanchor = "bottom"
                    ) %>%
    add_annotations(x = mean(Data$Weer_Wend$NL_churn),
                    y = 0.7,
                    text = paste('Wenbaarheid in heel Nederland:', percent(Data$Weer_Wend$NL_churn[1]-Data$Weer_Wend$NL_groei[1])),
                    showarrow = FALSE,
                    # arrowhead = 6,
                    xref = "x",
                    yref = "y",
                    arrowsize = 0.7,
                    ax = 13,
                    ay = 0,
                    xanchor = "left",
                    yanchor = "middle") %>%
    layout(showlegend = FALSE,
           separators = ',.',
           xaxis = list(title = "Wendbaarheid: verandering in samenstelling vacatures", zeroline = FALSE, tickformat = "%", rangemode = "tozero"),
           yaxis = list(title = "Weerbaarheid: verandering in volume vacatures", zeroline = FALSE, tickformat = "%")
    ) %>%
    config(displaylogo = FALSE, modeBarButtonsToRemove = buttons)

})

tegelWWinfo <- renderText({
  paste("<b>Weerbaarheid</b> meten we door de groei of krimp van gemiddeld aantal openstaande vacatures per kwartaal te meten tussen Q1 2020 en Q1 2021. 
        Het is uitgedrukt in een procentueel verschil tussen Q1 2021 en Q1 2020.We beperken ons tot gemeenten met gemiddeld meer dan 50 openstaande vacatures in Q1 2020.
        <br><br>
        <b>Wendbaarheid</b> meten we door de groei of krimp van alle onderliggende beroepsgroepen te meten, gecorrigeerd voor de groei van het totaal aantal vacatures.
        De som van deze verschillen geeft aan hoeveel onderlinge verschuiving heeft plaatsgevonden in beroepsgroepen en laat de grootte van verandering in de vraag naar arbeid zien." )
})

tegelWWtable <- renderDataTable({
  
  tableWW <- Data$Weer_Wend %>%
    mutate(highlight = ifelse(PROVINCIE %in% input$PROVINCIE, "Geselecteerd", "Overig")) %>%
    filter(highlight == "Geselecteerd") %>%
    select(GEMEENTE, Reg_churn, Reg_groei, Inwonertal_52)
  
  datatable(tableWW, extensions = "Buttons", options=list(scrollX = TRUE, dom = 'Bfrtip', buttons = c('copy', 'csv', 'excel'), columnDefs = list(list(className = 'dt-center', targets = '_all'))),
            colnames = c(
              "Gemeente" = "GEMEENTE",
              "Wendbaarheid" = "Reg_churn",
              "Weerbaarheid" = "Reg_groei",
              "Inwoners" = "Inwonertal_52")
  )
  # ) %>%
  #   formatRound(c(3:4),2)
})

TV <- renderPlotly({
  Data$Tijd_Vac %>%
    filter(PROVINCIE %in% input$PROVINCIE) %>%
    group_by(PEILDATUM) %>%
    summarise(AANTAL = sum(AANTAL), .groups = 'drop') %>%
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



tegelTVinfo <- renderText({
  paste("Vacaturedata komt van de UWV open match dataset, die per week bijhoudt hoeveel vacatures open staan. Deze grafiek laat het verloop op provincieniveau zien.")
})

tegelTVtable <- renderDataTable({
  
  tableTV <- Data$Tijd_Vac %>%
    filter(PROVINCIE == input$PROVINCIE) %>%
    group_by(PEILDATUM) %>%
    summarise(AANTAL = sum(AANTAL))
  
  datatable(tableTV, extensions = "Buttons", options=list(scrollX = TRUE, dom = 'Bfrtip', buttons = c('copy', 'csv', 'excel'), columnDefs = list(list(className = 'dt-center', targets = '_all'))),
            colnames = c(
              "Peildatum" = "PEILDATUM",
              "Aantal" = "AANTAL")
  )
  # ) %>%
  #   formatRound(c(3:4),2)
})

BG <- renderPlotly({
  Data$Niveau1_Totaal %>%
    filter(PROVINCIE %in% input$PROVINCIE, Niveau1 != "Overig") %>%
    group_by(KWARTAAL, Niveau1) %>%
    summarise(AANTAL = round(sum(AANTAL), digits = 0), .groups = 'drop') %>%
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

tegelBGinfo <- renderText({
  paste("Deze grafiek gebruikt het gemiddelde van openstaande vacatures per kwartaal per beroepsgroep om de verschuiving in samenstelling in de provincie te laten zien. Beroepsgroepen is het hoogste niveau van aggregatie bij het UWV. Berekeningen op wendbaarheid zijn op het onderliggende niveau uitgevoerd.")
})

tegelBGtable <- renderDataTable({
    
  tableBG <- Data$Niveau1_Totaal %>%
    filter(PROVINCIE == input$PROVINCIE, Niveau1 != "Overig") %>%
    group_by(Niveau1, KWARTAAL) %>%
    summarise(AANTAL = round(sum(AANTAL), digits = 0)) %>%
    pivot_wider(names_from = KWARTAAL, values_from = AANTAL)
  
  datatable(tableBG, extensions = "Buttons", options=list(scrollX = TRUE, dom = 'Bfrtip', buttons = c('copy', 'csv', 'excel'), columnDefs = list(list(className = 'dt-center', targets = '_all'))),
            colnames = c(
              # "Soort PO" = "SOORT_PO",
              "Beroepsgroep" = "Niveau1"
              # "Gemiddelde afstand tot basisschool (km)" = "Gem_Afstand",
              # "Gemiddelde afstand tot basisschool in Nederland (km)" = "Nederland")
            )
  ) 
  # %>%
  #   formatRound(c(3:4),2)
})




