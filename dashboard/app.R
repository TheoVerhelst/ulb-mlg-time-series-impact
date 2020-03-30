## app.R ##
library(dplyr)
library(shiny)
library(shinydashboard)
library(ggplot2)

source("functions.R")


##Download Data##
info_data <- readRDS(url(
  "https://github.com/TheoVerhelst/ulb-mlg-time-series-impact/blob/master/data/COVID19_Country_Info.Rdata?raw=true"))
global_data <- readRDS(url(
  "https://github.com/TheoVerhelst/ulb-mlg-time-series-impact/blob/master/data/COVID19_Global_Italy_wGrowth.Rdata?raw=true"))

##Split between Italian and Global Data##
is_regional_italy <- (global_data$Country.Region == "Italy") & (global_data$Province.State != "")
italian_data <- global_data[is_regional_italy, ]
global_data <- global_data[!is_regional_italy, ]

global_data_by_action <- to_long_format_on_action(global_data, info_data)
global_data_by_action <- global_data_by_action[is.finite(global_data_by_action$ConfirmedGrowthRate),]

##Make lists for input panels##
countries <- unique(global_data["Country.Region"])
countries_with_regions <- unique(global_data[global_data$Province.State != "", "Country.Region"])
statistics_italy <- names(global_data)[6:15]
statistics_global <- names(global_data)[6:8]
italian_regions <- unique(italian_data["Province.State"])




action_label_dict <- list("Date.Schools" = "Schools closure",
                    "Date.Public Places" = "Public places shut down",
                    "Date.Gatherings" = "Gatherings ban",
                    "Date.Stay at Home" = "Stay at home",
                    "Date.Non-essential" = "Non-essential activities ban")

# Create the same dict mapping, but reversed (for creating radio buttons)
action_label_dict_rev <- names(action_label_dict)
names(action_label_dict_rev) <- unname(action_label_dict)


stat_label_dict <- c("Confirmed cases" = "Confirmed",
                    "Deaths" = "Deaths",
                    "Recovered" = "Recovered")


world_side_panel <- sidebarPanel(
  selectInput(
    inputId = "country",
    label = "Choose a country:",
    selected = countries[83,],
    choices = countries
  ),
  
  uiOutput("region_selector"),
  
  selectInput(
    inputId = "world_stat",
    label = "Choose a statistic:",
    choices = stat_label_dict,
    selected = stat_label_dict[1]
  ),
  
  sliderInput(
    "range",
    label = "Days of interest (0 = last day):",
    min = -90,
    max = 0,
    value = c(-30, 0)
  ),
  
  checkboxInput("log_scale_world", "Use log scale for Y", FALSE),
  
  radioButtons("dates", "Show a special date:", unlist(action_label_dict_rev)),
  
  sliderInput(
    "smooth_growth_rate_world",
    label = "Degree of growth-rate smoothing: (WORK IN PROGRESS)",
    min = 0,
    max = 10,
    value = 0
  )
  
)


world_main_panel <- mainPanel(
  tabsetPanel(
    tabPanel("Time evolution",
       column(
         width = 12,
         box(
           title = "Total cases",
           width = NULL,
           solidHeader = TRUE,
           status = "primary",
           plotOutput("cases_plot")
         ),
         box(
           title = "Growth Rate",
           width = NULL,
           solidHeader = TRUE,
           status = "primary",
           plotOutput("growth_plot")
         )
       )
    ),
    
    tabPanel("Distribution comparison",
       column(
         width = 12,
         box(
           title = "Distribution of growth rate before and after action",
           width = NULL,
           solidHeader = TRUE,
           status = "primary",
           plotOutput("action_comp_plot")
         )
       )
)
  )
)


italy_side_panel <- sidebarPanel(
  selectInput(
    inputId = "italy_region",
    label = "Choose a region:",
    selected = italian_regions[0],
    choices = italian_regions
  ),
  
  selectInput(
    inputId = "italy_statistic",
    label = "Choose a statistic:",
    selected = statistics_italy[0],
    choices = statistics_italy
  ),
  
  sliderInput(
    inputId = "italy_range",
    label = "Days of interest (0 = last day):",
    min = -60,
    max = 0,
    value = c(-7, 0)
  ),
  checkboxInput("log_scale_world_IT", "Use log scale for Y", FALSE)
)


italy_main_panel <- mainPanel(column(
  width = 10,
  box(
    title = "Statistics",
    width = NULL,
    solidHeader = TRUE,
    status = "primary",
    plotOutput("chosen_stat_it_plot")
  )
))


ranking_side_panel <- sidebarPanel(
  radioButtons("ranking_date", "Confinement action used in ranking", unlist(action_label_dict_rev))
)


ranking_main_panel <- mainPanel(
  box(
    title = "Ranking",
    width = NULL,
    solidHeader = TRUE,
    status = "primary",
    plotOutput("ranking_plot")
  )
)



ui <- dashboardPage(
  dashboardHeader(title = "CODE VS COVID19"),
    dashboardSidebar(sidebarMenu(
    menuItem("World", tabName = "world", icon = icon("bar-chart-o")),
    menuItem("Italy", tabName = "italy", icon = icon("bar-chart-o")),
    menuItem("Ranking", tabName = "ranking", icon = icon("bar-chart-o"))
  )),
  
  ## Body content
  dashboardBody(fluidPage(tabItems(
    # First tab content
    tabItem(tabName = "world",
            world_side_panel,
            world_main_panel),
    tabItem(tabName = "italy",
            italy_side_panel,
            italy_main_panel),
    tabItem(tabName = "ranking",
            ranking_side_panel,
            ranking_main_panel)
  )))
)

server <- function(input, output) {
  # slice exact part of dataset
  datasetInput <- reactive({
    min_date = Sys.Date() + input$range[1]
    max_date = Sys.Date() + input$range[2]
    return(global_data[(global_data$Country.Region == input$country) &
                         (global_data$Province.State == ifelse(input$country %in% countries_with_regions, input$region, "")) &
                         (global_data$Date >= min_date) &
                         (global_data$Date <= max_date),])
  })
  
  # slice exact part of dataset
  dataset_by_action_input <- reactive({
    min_date = Sys.Date() + input$range[1]
    max_date = Sys.Date() + input$range[2]
    return(global_data_by_action[(global_data_by_action$Country.Region == input$country) &
                         (global_data_by_action$Province.State == ifelse(input$country %in% countries_with_regions, input$region, "")) &
                         (global_data_by_action$Date >= min_date) &
                         (global_data_by_action$Date <= max_date),])
  })
  
  get_country_info <- function() {
    return(info_data[(info_data$Country.Region == input$country) &
                         (info_data$Province.State == ifelse(input$country %in% countries_with_regions, input$region, "")),])
  }
  
  ## Render the region selector if a country with regions is chosen
  output$region_selector <- renderUI({
    if (input$country %in% countries_with_regions) {
      selectInput(
        inputId = 'region',
        label = 'Choose a region:',
        choices = unique(global_data[global_data$Country.Region == input$country , "Province.State"])
      )
    } else {
      return(NULL)
    }
    
  })
  
  
  make_plot <- function(colname_suffix, allow_log) renderPlot({
    stat_to_plot <- paste0(input$world_stat, colname_suffix)
    dataset <- datasetInput()
    country_info <- get_country_info()
    
      texts_to_show <- as.character(input$dates)
    dates_to_show <- do.call("c", lapply(texts_to_show, function(col) country_info[, col]))
    
    # Remove NAs to avoid a warning
    texts_to_show <- texts_to_show[!is.na(dates_to_show)]
    dates_to_show <- dates_to_show[!is.na(dates_to_show)]
    
    dates_to_show_lab <- gsub("^.*?-","",dates_to_show)
    events <- unlist(action_label_dict[texts_to_show])
    days <- unlist(dates_to_show_lab)
 
    ggplot(as.data.frame(dataset), aes_string(x = "Date", y = stat_to_plot)) +
      geom_line() +
      geom_vline(xintercept = dates_to_show) +
      xlab("Date") +
      ylab(stat_to_plot) +
      annotate("text", x = dates_to_show, y = 0, angle = 90, vjust = 1.5, hjust=-0.5, label =  paste(events,days,sep = ":")) + 
      scale_y_continuous(trans=ifelse(input$log_scale_world & allow_log, "log10", "identity")) +
      theme_bw() 
  })
  
  ########
  ##PLOTS#
  ########
  
  output$cases_plot <- make_plot("", allow_log = TRUE)
  output$growth_plot <- make_plot("GrowthRate", allow_log = FALSE)
  
  
  output$action_comp_plot <- renderPlot({
    dataset <- dataset_by_action_input()
    dataset <- dataset[dataset$Action == as.character(input$dates),]
    stat_to_plot <- paste0(input$world_stat, "GrowthRate")
    ggplot(dataset, aes_string(x = "BeforeAction", color="BeforeAction", y = stat_to_plot)) + 
      geom_boxplot(show.legend = FALSE) +
      scale_color_brewer(type = "qual", palette = 2) +
      xlab("Comparison before and after the action") +
      ylab("Growth Rate distribution")+
      scale_x_discrete(labels=c("Before the action","After the action"))+
      theme_bw()
  })
  
  #########
  ##ITALY##
  #########
  
  output$chosen_stat_it_plot <- renderPlot({
    min_it <- Sys.Date() + input$italy_range[1]
    max_it <- Sys.Date() + input$italy_range[2]
    
    dataset <-
      italian_data[(italian_data$Province.State == input$italy_region) &
                     (italian_data$Date >= min_it) &
                     (italian_data$Date <= max_it), ]
    
    ggplot(dataset, aes_string(x = "Date", y = input$italy_statistic)) +
      geom_line() +
      xlab("Date") +
      ylab(input$italy_statistic) +
      scale_y_continuous(trans=ifelse(input$log_scale_world_IT, "log10", "identity")) +
      theme_bw()
  })
  
  output$ranking_plot <- renderPlot({
    # ConfirmedGrowthRate is hardcoded because we have barely any data for the others
    z_stat <- compute_z_statistic(global_data_by_action[global_data_by_action$Province.State == "",], "ConfirmedGrowthRate")
    z_stat <- z_stat[z_stat$Action == input$ranking_date,]
    z_stat <- z_stat[!is.na(z_stat$Z),]
    ggplot(z_stat, aes(x = reorder(Country.Region, Z, sum), y = Z)) +
      geom_col(fill = "lightblue") +
      geom_text(aes(label = paste("p =", prettyNum(p.value.adj))), position = position_stack(vjust = .5)) +
      coord_flip() +
      xlab("Country") +
      ylab("Z statistic (with p-value)") +
      scale_color_brewer(type = "qual", palette = 2) +
      theme_bw()
  })
}


shinyApp(ui, server)
