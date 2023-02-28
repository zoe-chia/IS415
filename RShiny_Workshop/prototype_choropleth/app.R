pacman::p_load(shiny, sf, tmap, bslib, tidyverse)

# import data
popdata <- read_csv("data2/aspatial/respopagesextod2011to2020.csv")
mpsz <- st_read(dsn = "data2/geospatial", 
                layer = "MP14_SUBZONE_WEB_PL")

# Data wrangling: Prepare aspatial data to get variables that you need
popdata2020 <- popdata %>%
  filter(Time == 2020) %>%
  group_by(PA, SZ, AG) %>%
  summarise(`POP` = sum(`Pop`)) %>%
  ungroup()%>%
  pivot_wider(names_from=AG, 
              values_from=POP) %>%
  mutate(YOUNG = `0_to_4` + `5_to_9` + `10_to_14` + `15_to_19` + `20_to_24`) %>%
  mutate(`ECONOMY ACTIVE` = rowSums(.[9:13])+
           rowSums(.[15:17]))%>%
  mutate(`AGED`=rowSums(.[18:22])) %>%
  mutate(`TOTAL`=rowSums(.[5:22])) %>%  
  mutate(`DEPENDENCY` = (`YOUNG` + `AGED`)
         /`ECONOMY ACTIVE`) %>%
  mutate_at(.vars = vars(PA, SZ),
            .funs = funs(toupper)) %>%
  select(`PA`, `SZ`, `YOUNG`, 
         `ECONOMY ACTIVE`, `AGED`, 
         `TOTAL`, `DEPENDENCY`) %>%
  filter(`ECONOMY ACTIVE` > 0)

# join data 
mpsz_pop2020 <- left_join(mpsz, popdata2020,
                          by = c("SUBZONE_N" = "SZ"))

# ui
ui <- fluidPage(
  titlePanel("Choropleth Mapping System"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "variable",
                  label = "Mapping variable:",
                  choices = list( "Dependency" = "DEPENDENCY",
                              "Economy" = "ECONOMY ACTIVE",
                              "Aged" = "AGED",
                              "Young" = "YOUNG",
                              "Total" =  "TOTAL"),
                  selected = "DEPENDENCY", #default selection
                  multiple = TRUE),
      sliderInput(inputId = "classes",
                  label = "Number of classes:",
                  min = 6,
                  max = 12,
                  value = 8),
      selectInput(inputId = "colour_scheme",
                  label = "Colour scheme:",
                  choices = c("reds" = "reds",
                              "greens" = "greens",
                              "blues" = "Blues",
                              "Yellow-Orange-Red" = "YlOrRd"
                             ),
                  selected = "YlOrRd", #default selection
                  multiple = FALSE),
      selectInput(inputId = "classification",
                  label = "Classification method:",
                  choices = c("pretty" = "pretty",
                              "sd" = "sd",
                              "equal" = "equal",
                              "quantile" = "quantile",
                              "kmeans" = "kmeans",
                              "jenks" = "jenks",
                              "hclust" = "hclust"),
                  selected = "pretty", #default selection
                  multiple = FALSE),
    ),
    # main portion that contains maps, tables etc.
    mainPanel(   
      tmapOutput("mapPlot", # tmapOutput for interactive map. plotOutput for static map.
                 width = "100%",
                 height = 400)
    )
  )
)

#how the server interacts with the ui 
server <- function(input, output) {
  output$mapPlot <- renderTmap({
    tmap_options(check.and.fix = TRUE) + 
      tm_shape(mpsz_pop2020) + 
      tm_fill(input$variable,
              n=input$classes,
              style = input$classification,
              palette = input$colour_scheme) + 
      tm_borders(lwd = 0.1, alpha = 1) +
      tm_view(set.zoom.limits = c(11, 14))
  })
  output$aTable <- DT::renderDataTable({
    if(input$showData){
      DT::datatable(data = dataset() %>%
                      select(1:4),
                    options = list(pageLength = 10),
                    rownames = FALSE)
    }
  })
  
}


shinyApp(ui = ui, server = server)