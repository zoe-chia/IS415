pacman::p_load(shiny, sf, tmap, tidyverse)

# import csv data
sgpools <- read_csv("data1/aspatial/SGPools_svy21.csv")

# convert csv to sf
sgpools_sf <- st_as_sf(sgpools,
                       coords= c("XCOORD",
                                 "YCOORD"),
                       crs = 3414)
# design
ui <- fluidPage(
  titlePanel("Static Proportional Symbol Map"),
  sidebarLayout(
    sidebarPanel(),
    mainPanel(
      plotOutput("mapPlot")
    )
  )
)

# how should the server interact with the ui
server <- function(input, output){
  output$mapPlot <- renderPlot({
    tm_shape(sgpools_sf) + 
      tm_bubbles(col = "OUTLET_TYPE",
                 size = "Gp1Gp2 Winnings",
                 border.col = "black",
                 border.lwd = 0.5)
  })
}
# Run the application 
shinyApp(ui = ui, server = server)
