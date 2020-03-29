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
is_regional_italy <- (global_data$Country.Region == "Italy") & (global_data$Province.State!="")
italian_data <- global_data[is_regional_italy,]
global_data <- global_data[!is_regional_italy,]



##Make lists for input panels##
countries <- unique(global_data["Country.Region"])
countries_with_regions <- unique(global_data[global_data$Province.State != "","Country.Region" ])
statistics_italy <- names(global_data)[6:15]
statistics_global <- names(global_data)[6:8]
italian_regions <- unique(italian_data["Province.State"])

compute_logscale <- function(input){
  if (input){
    logscale <- "y"
  } else {
    logscale <- ""
  }
  return(logscale)
} 

world_side_panel <- sidebarPanel( 
  selectInput(inputId = "country",
              label = "Choose a country:",
              selected = countries[83,],
              choices = countries), 
  
  
  uiOutput("region_selector"),
  
  
  sliderInput("range", 
              label = "Days of interest (0 = last day):",
              min = -60, max = 0, value = c(-14, 0)),
  
  checkboxInput("log_scale_world", "Use log scale for Y", FALSE),
  
  checkboxInput("show_dates_world", "Show important dates (WORK IN PROGRESS)", FALSE),
  
  sliderInput("smooth_growth_rate_world", 
              label = "Degree of growth-rate smoothing: (WORK IN PROGRESS)",
              min = 0, max = 10, value = 0),
  
  sliderInput("day_split_world", 
              label = "Day analyzed (0 = last day): (WORK IN PROGRESS)",
              min = 0, max = 10, value = 0)
)


world_main_panel <- mainPanel(
  tabsetPanel(
    tabPanel("Confirmed", 
             column(width = 12,
                    box(
                      title = "Confirmed", width = NULL, solidHeader = TRUE, status = "primary",
                      plotOutput("confirmed_plot")
                    ),
                    box(
                      title = "Confirmed Growth Rate", width = NULL, solidHeader = TRUE, status = "primary",
                      plotOutput("confirmed_growth_plot")
                    )
             )         
    ),
    tabPanel("Recovered", 
             column(width = 12,
                    box(
                      title = "Recovered", width = NULL,solidHeader = TRUE, status = "primary",
                      plotOutput("recovered_plot")
                    ),
                    box(
                      title = "Recovered Growth Rate", width = NULL, solidHeader = TRUE, status = "primary",
                      plotOutput("recovered_growth_plot")
                    )
             )         
    ),
    tabPanel("Death",
             column(width = 12,
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
  )
)


italy_side_panel <- sidebarPanel(
  selectInput(inputId = "italy_region",
              label = "Choose a region:",
              selected = italian_regions[0],
              choices = italian_regions), 
  
  selectInput(inputId = "italy_statistic",
              label = "Choose a statistic:",
              selected = statistics_italy[0],
              choices = statistics_italy), 
  
  sliderInput(inputId = "italy_range", 
              label = "Days of interest (0 = last day):",
              min = -60, max = 0, value = c(-7, 0)),
  checkboxInput("log_scale_world_IT", "Use log scale for Y", FALSE)
)


italy_main_panel <- mainPanel(
  column(width = 10,
         box(
           title = "Statistics", width = NULL, solidHeader = TRUE, status = "primary",
           plotOutput("chosen_stat_it_plot")
         )
  )         
)



ui <- dashboardPage(
  
  dashboardHeader(title = "CODE VS COVID19"),
    
  dashboardSidebar(
      sidebarMenu(
          menuItem("World", tabName = "world", icon = icon("bar-chart-o")),
          menuItem("Italy",tabName = "italy", icon = icon("bar-chart-o"))
      )
    ), 
        
  ## Body content
  dashboardBody(
      fluidPage(
        tabItems(
          # First tab content
          tabItem(tabName = "world",
                  world_side_panel,
                  world_main_panel),
          tabItem(tabName = "italy",
                  italy_side_panel,
                  italy_main_panel
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
    logscale <- compute_logscale(input$log_scale_world)
    dataset <- datasetInput()
    plot(x = dataset[,"Date"] , 
         y = dataset[,"Confirmed"],
         type="l",
         xlab = "Day",
         ylab = "Confirmed",
         log = logscale)
    #abline(v = as.Date("2020/03/20"))
  })
  
  output$recovered_plot <- renderPlot({
    logscale <- compute_logscale(input$log_scale_world)
    dataset <- datasetInput()
    plot(x = dataset[,"Date"] , 
         y = dataset[,"Recovered"],
         type="l",
         xlab = "Day",
         ylab = "Recovered",
         log = logscale)
    
  })
  
  output$deaths_plot <- renderPlot({
    logscale <- compute_logscale(input$log_scale_world)
    dataset <- datasetInput()
    plot(x = dataset[,"Date"] , 
         y = dataset[,"Deaths"],
         type="l",
         xlab = "Day",
         ylab = "Deaths",
         log = logscale)
    
  })
  
  output$confirmed_growth_plot <- renderPlot({
    
    dataset <- datasetInput()
    plot(x = dataset[,"Date"] , 
         y = dataset[,"ConfirmedGrowthRate"],
         type="l",
         xlab = "Day",
         ylab = "Confirmed Growth Rate"
         )
    
  })
  
  output$recovered_growth_plot <- renderPlot({
    
    dataset <- datasetInput()
    plot(x = dataset[,"Date"] , 
         y = dataset[,"RecoveredGrowthRate"],
         type="l",
         xlab = "Day",
         ylab = "Recovered Growth Rate")
    
  })
  
  output$deaths_growth_plot <- renderPlot({
    
    dataset <- datasetInput()
    plot(x = dataset[,"Date"] , 
         y = dataset[,"DeathsGrowthRate"],
         type="l",
         xlab = "Day",
         ylab = "Deaths Growth Rate")
    
  })
  
  
  #########
  ##ITALY##
  #########
  
  output$chosen_stat_it_plot <- renderPlot({
    logscale <- compute_logscale(input$log_scale_world_IT)
    min_it <- Sys.Date()+input$italy_range[1]
    max_it <- Sys.Date()+input$italy_range[2]
    
    dataset <- italian_data[(italian_data$Province.State == input$italy_region) & 
                            (italian_data$Date >= min_it) & 
                            (italian_data$Date <= max_it), ]
    
    plot(x = dataset[,"Date"] , 
         y = dataset[,input$italy_statistic],
         type="l",
         xlab = "Day",
         ylab = input$italy_statistic,
         log = logscale) 
    
  })
  

  
  
}



shinyApp(ui, server)

