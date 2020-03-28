## app.R ##
library(shiny)
library(shinydashboard)


#
url_confirmed <- "https://github.com/TheoVerhelst/ulb-mlg-time-series-impact/blob/master/data/Global_JohnsHopkins/time_series_covid19_confirmed_global.csv?raw=true"
url_deaths <- "https://github.com/TheoVerhelst/ulb-mlg-time-series-impact/blob/master/data/Global_JohnsHopkins/time_series_covid19_deaths_global.csv?raw=true"
url_recovered <- "https://github.com/TheoVerhelst/ulb-mlg-time-series-impact/blob/master/data/Global_JohnsHopkins/time_series_covid19_recovered_global.csv?raw=true"
url_italy <- "https://github.com/TheoVerhelst/ulb-mlg-time-series-impact/blob/master/data/Italy/dpc-covid19-ita-regioni-latest.csv?raw=true"
#

download.file(url_confirmed, destfile= "./time_series_covid19_confirmed_global.csv", mode = "wb")
download.file(url_deaths, destfile= "./time_series_covid19_deaths_global.csv", mode = "wb")
download.file(url_recovered, destfile= "./time_series_covid19_recovered_global.csv", mode = "wb")
download.file(url_italy, destfile= "./dpc-covid19-ita-regioni-latest.csv", mode = "wb")

confirmed <- read.csv("time_series_covid19_confirmed_global.csv")
deaths <- read.csv("time_series_covid19_deaths_global.csv")
recovered <- read.csv("time_series_covid19_recovered_global.csv")
italy <- read.csv("dpc-covid19-ita-regioni-latest.csv")

#The combination of Country and Province creates UNIQUE values for COUNTRIES SELECTION
confirmed <- within(confirmed,  Country.Region <- paste(Country.Region, Province.State, sep=" "))
deaths <- within(deaths,  Country.Region <- paste(Country.Region, Province.State, sep=" "))
recovered <- within(recovered,  Country.Region <- paste(Country.Region, Province.State, sep=" "))



ui <- dashboardPage(
  dashboardHeader(title = "CODE VS COVID19"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("World", tabName = "world", icon = icon("dashboard")),
      menuItem("Italy - compare", tabName = "italy_compare", icon = icon("dashboard")),
      menuItem("Italy - Statistics", tabName = "italy_statistics", icon = icon("dashboard"))
    )
  ),
  ## Body content
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "world",
              fluidRow(
                box(plotOutput("distPlot", height = 350)),
              
                box(
                  title = "Controls",
                  selectInput(inputId = "country",
                              label = "Choose a country:",
                              choices = confirmed["Country.Region"]), 
                  
                  selectInput(inputId = "dataset",
                              label = "Choose a dataset:",
                              choices = c("Confirmed", "Deaths", "Recovered")),
                  
                  sliderInput("range", 
                              label = "Days of interest (0 = last day):",
                              min = -60, max = 0, value = c(-60, 0)))                
                )
              
      ),
      
      # Second tab content
      tabItem(tabName = "italy_compare",
              
              fluidRow(
                box(plotOutput("statsItaly")
              )),
              
              box(
                  title = "Controls",
                  selectInput(inputId = "statsChosenITA",
                              label = "Choose a statistics:",
                              choices = colnames(italy)[7:16])
              
              )
      ),
      
      tabItem(tabName = "italy_statistics",
              
              box(
                title = "Controls",
                selectInput(inputId = "regChosenITA",
                            label = "Choose an Italian Region:",
                            choices = italy[,"denominazione_regione"]
                )
              ),
                
              fluidRow(  
              
                infoBoxOutput("tests"),
                infoBoxOutput("total"),
                infoBoxOutput("home"),
                infoBoxOutput("hospitalized"),
                infoBoxOutput("intensive"),
                infoBoxOutput("confirmed"),
                infoBoxOutput("recovered"),
                infoBoxOutput("deaths")

              ))
      
    )
)
)

server <- function(input, output) {
  
## FIRST TAB 
  
  datasetInput <- reactive({
    switch(input$dataset,
           "Confirmed" = confirmed,
           "Deaths" = deaths,
           "Recovered" = recovered)
  })
  
 
  output$statsItaly <- renderPlot({
    
    selection = input$statsChosenITA
    labels <- italy[,"denominazione_regione"]
    data <- italy[,selection] 
    barplot(data, names.arg=labels, horiz = TRUE, las = 1 ) 
  })
  
  
  
  output$distPlot <- renderPlot({
    dataset <- datasetInput()
    
    min = input$range[1]
    max = input$range[2]
    today = ncol(dataset)
    part  <- dataset[dataset$Country.Region == input$country,(today + min): (today +max)]
    plot(t(as.matrix(part)))
  })
  
  
## SECOND TAB 
  
  output$home <- renderInfoBox({
    num  <- italy[italy$denominazione_regione == input$regChosenITA , "isolamento_domiciliare"]
    infoBox(
      "At home, isolated: ", num, icon = icon("list"),
      color = "navy"
    )
  })
  output$hospitalized <- renderInfoBox({
    num  <- italy[italy$denominazione_regione == input$regChosenITA , "totale_ospedalizzati"]
    infoBox(
      "Hospitalized: ", num, icon = icon("list"),
      color = "aqua"
    )
  })
  
  # Same as above, but with fill=TRUE
  output$intensive <- renderInfoBox({
    num  <- italy[italy$denominazione_regione == input$regChosenITA , "terapia_intensiva"]
    infoBox(
      "Intensive Care: ", num, icon = icon("list"),
      color = "purple", fill = TRUE
    )
  })
  output$confirmed <- renderInfoBox({
    num  <- italy[italy$denominazione_regione == input$regChosenITA , "totale_attualmente_positivi"]
    infoBox(
      "Confirmed Positive: ", num , icon = icon("list"),
      color = "yellow", fill = TRUE
    )
  })
  
  output$recovered <- renderInfoBox({
    num  <- italy[italy$denominazione_regione == input$regChosenITA , "dimessi_guariti"]
    infoBox(
      "Recovered: ", num, icon = icon("list"),
      color = "green", fill = TRUE
    )
  })
  
  output$deaths <- renderInfoBox({
    num  <- italy[italy$denominazione_regione == input$regChosenITA , "deceduti"]
    infoBox(
      "Deaths: ", num, icon = icon("list"),
      color = "orange", fill = TRUE
    )
  })
  
  output$tests <- renderInfoBox({
    num  <- italy[italy$denominazione_regione == input$regChosenITA , "tamponi"]
    infoBox(
      "Total of tests: ", num, icon = icon("list"),
      color = "blue", fill = TRUE
    )
  })
  
  output$total <- renderInfoBox({
    num  <- italy[italy$denominazione_regione == input$regChosenITA , "totale_casi"]
    infoBox(
      "Total of cases: ",num, icon = icon("list"),
      color = "red", fill = TRUE
    )
  })
}



shinyApp(ui, server)

