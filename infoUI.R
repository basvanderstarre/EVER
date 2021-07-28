infoUI <- tabItem(
  tabName = "InfoTab",
  fluidRow(
    box(
      width = 12,
      htmlOutput("landinginfo"),
      div(img(src="https://www.birch.nl/wp-content/uploads/Birch-logo.png",
              width=200,
              height=93.33,
              style="text-align: center; margin:5px 5px"))
      )
    )
  )