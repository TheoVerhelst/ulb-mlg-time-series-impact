## app.R ##
library(dplyr)
library(shiny)
library(shinydashboard)


##Download and Update Data##
url_data_info <- "https://github.com/TheoVerhelst/ulb-mlg-time-series-impact/blob/master/data/COVID19_Country_Info.Rdata?raw=true"
url_data_global <- "https://github.com/TheoVerhelst/ulb-mlg-time-series-impact/blob/master/data/COVID19_Global_Italy.Rdata?raw=true"
url_data_global_wgrowth <- "https://github.com/TheoVerhelst/ulb-mlg-time-series-impact/blob/master/data/COVID19_Global_Italy_wGrowth.Rdata?raw=true"
download.file(url_data_global, destfile= "./COVID19_Global_Italy.Rdata", mode = "wb")
download.file(url_data_info, destfile= "./COVID19_Country_Info.Rdata", mode = "wb")
download.file(url_data_global_wgrowth, destfile= "./COVID19_Global_Italy_wGrowth.Rdata", mode = "wb")

info_data <- readRDS("./COVID19_Country_Info.Rdata")
global_data <- readRDS("./COVID19_Global_Italy_wGrowth.Rdata")



##Split between Italian and Global Data##
italian_data <- global_data[(global_data$Country.Region == "Italy") & (global_data$Province.State!="") ,]
global_data <- setdiff(global_data,italian_data)



##Make lists for input panels##
countries <- unique(global_data["Country.Region"])
countries_with_regions <- unique(global_data[global_data$Province.State != "","Country.Region" ])
statistics_italy <- names(global_data)[6:15]
statistics_global <- names(global_data)[6:8]
italian_regions <- unique(italian_data["Province.State"])




ui <- dashboardPage(
  
  dashboardHeader(title = "CODE VS COVID19"),
  dashboardSidebar(
    sidebarMenu(
      
      menuItem("World", tabName = "world", icon = icon("bar-chart-o"),
               
               selectInput(inputId = "country",
                           label = "Choose a country:",
                           selected = countries[0],
                           choices = countries), 
               
               
               uiOutput("region_selector"),
               
               
               
               sliderInput("range", 
                           label = "Days of interest (0 = last day):",
                           min = -60, max = 0, value = c(-7, 0))
      ),
      
      
      menuItem("Italy",tabName = "italy", icon = icon("bar-chart-o"),
               
               selectInput(inputId = "italy_region",
                           label = "Choose a region:",
                           selected = italian_regions[0],
                           choices = italian_regions), 
               
               selectInput(inputId = "italy_statistics",
                           label = "Choose a statistic:",
                           selected = statistics_italy[0],
                           choices = statistics_italy), 
               
               sliderInput("range", 
                           label = "Days of interest (0 = last day):",
                           min = -60, max = 0, value = c(-7, 0))
               
      )
    ) 
    
  ),
  
  
  
  
  ## Body content
  dashboardBody(
    
    tabItems(
      # First tab content
      tabItem(tabName = "world",
              fluidPage(
                column(width = 4,
                       box(
                         title = "Confirmed", width = NULL, solidHeader = TRUE, status = "primary",
                         plotOutput("confirmed_plot")
                       ),
                       box(
                         title = "Confirmed Growth Rate", width = NULL, solidHeader = TRUE, status = "primary",
                         plotOutput("confirmed_growth_plot")
                       )
                ),
                
                column(width = 4,
                       box(
                         title = "Recovered", width = NULL,solidHeader = TRUE, status = "primary",
                         plotOutput("recovered_plot")
                       ),
                       box(
                         title = "Recovered Growth Rate", width = NULL, solidHeader = TRUE, status = "primary",
                         plotOutput("recovered_growth_plot")
                       )
                ),
                
                column(width = 4,
                       box(
                         title = "Deaths", width = NULL, solidHeader = TRUE,status = "primary",
                         plotOutput("deaths_plot")
                       ),
                       box(
                         title = "Deaths Growth Rate", width = NULL, solidHeader = TRUE, status = "primary",
                         plotOutput("deaths_growth_plot")
                       )
                )
              )
      ),
      tabItem(tabName = "italy",
              fluidPage(
                column(width = 4,
                       box(
                         title = "Confirmed", width = NULL,solidHeader = TRUE, status = "primary",
                         plotOutput("confirmed_plot")
                       ),
                       box(
                         title = "Confirmed Growth Rate", width = NULL, solidHeader = TRUE, status = "primary",
                         plotOutput("confirmed_growth_plot")
                       )
                )
              )
        )
    )
)

)





server <- function(input, output) {
  
  
  #slice exact part of dataset
  datasetInput <- reactive({
    min_date = Sys.Date()+input$range[1]
    max_date = Sys.Date()+input$range[2]
    
    
    if (!is.null(input$region)) {
      #we assume Region names are unique
      return(global_data[(global_data$Province.State == input$region) & (global_data$Date >= min_date) & (global_data$Date <= max_date), ]  )
      
    } else {
      
      return(global_data[(global_data$Country.Region == input$country) & (global_data$Province.State == "") & (global_data$Date >= min_date) & (global_data$Date <= max_date), ])
      
    }
    
  })
  
  ## Render the region selector if a country with regions is chosen
  output$region_selector <- renderUI({
    
    if (input$country %in% countries_with_regions) {
      selectInput(inputId = 'region',
                  label = 'Choose a region:',
                  choices = unique(global_data[global_data$Country.Region == input$country ,"Province.State"])
      )
    } else {
      return(NULL)
    }
    
  })
  
  
  ########
  ##PLOTS#
  ########
  
  output$confirmed_plot <- renderPlot({
    dataset <- datasetInput()
    plot(x = dataset[,"Date"] , 
         y = dataset[,"Confirmed",],
         xlab = "Day",
         ylab = "Confirmed")
    
  })
  
  output$recovered_plot <- renderPlot({
    dataset <- datasetInput()
    plot(x = dataset[,"Date"] , 
         y = dataset[,"Recovered",],
         xlab = "Day",
         ylab = "Recovered")
    
  })
  
  output$deaths_plot <- renderPlot({
    dataset <- datasetInput()
    plot(x = dataset[,"Date"] , 
         y = dataset[,"Deaths",],
         xlab = "Day",
         ylab = "Deaths")
    
  })
  
  output$confirmed_growth_plot <- renderPlot({
    dataset <- datasetInput()
    plot(x = dataset[,"Date"] , 
         y = dataset[,"ConfirmedGrowthRate",],
         xlab = "Day",
         ylab = "Confirmed Growth Rate")
    
  })
  
  output$recovered_growth_plot <- renderPlot({
    dataset <- datasetInput()
    plot(x = dataset[,"Date"] , 
         y = dataset[,"RecoveredGrowthRate",],
         xlab = "Day",
         ylab = "Recovered Growth Rate")
    
  })
  
  output$deaths_growth_plot <- renderPlot({
    dataset <- datasetInput()
    plot(x = dataset[,"Date"] , 
         y = dataset[,"DeathsGrowthRate",],
         xlab = "Day",
         ylab = "Deaths Growth Rate")
    
  })
  
  
}



shinyApp(ui, server)

