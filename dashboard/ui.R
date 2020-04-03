library(shiny)
library(shinydashboard)
library(waiter)

source("common.R")

world_side_panel <- sidebarPanel(
  uiOutput("country_selector"),
  uiOutput("region_selector"),
  
  selectInput(
    inputId = "world_stat",
    label = "Choose a statistic:",
    choices = stat_global_label_dict,
    selected = stat_global_label_dict[1]
  ),
  
  sliderInput(
    "range",
    label = "Days of interest (0 = last day):",
    min = -60,
    max = 0,
    value = c(-40, 0)
  ),
  
  checkboxInput("log_scale_world", "Use log scale for Y", FALSE),
  
  radioButtons("dates", "Show a special date:", unlist(action_label_dict_rev)),
  
  sliderInput(
    "smooth_growth_rate_world",
    label = "Degree of growth-rate smoothing:",
    min = 0,
    max = 10,
    value = 0
  ),
  
  sliderInput(
    "linear_fitting_world",
    label = "Number of last days fitted",
    min = 5,
    max = 10,
    value = 5
  ),

  sliderInput(
    "clusters",
    label = "Clusters: (only for TS clustering)",
    min = 2,
    max = length(european_countries),
    value = 2,
    step = 1
  ),
  
  selectInput(
    inputId = "clustering_distance",
    label = "Choose a clustering distance: (Only for TS clustering)",
    choices = TS_CLUST_DISTANCES,
    selected = TS_CLUST_DISTANCES[4]
  )
  
)


world_main_panel <- mainPanel(
  tabsetPanel(
    tabPanel("Time evolution",
       column(
         width = 12,
		 uiOutput("help_text_panel1"),
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
		 uiOutput("help_text_panel2"),
         box(
           title = "Distribution of growth rate before and after action",
           width = NULL,
           solidHeader = TRUE,
           status = "primary",
           plotOutput("action_comp_plot")
         )
       )
    ),
    
    tabPanel("Change point detection",
       column(
         width = 12,
         uiOutput("help_text_panel3"),
         box(
           title = "Detection of change point",
           width = NULL,
           solidHeader = TRUE,
           status = "primary",
           plotOutput("change_point_plot")
         ),
         wellPanel(
           helpText("We employ an algorithm (based on the R package changepoints) to detect a shift in the distribution of the data.
                    The algorithm assumes a prior normal distribution of the data, and finds the optimal number of change points
                    (in terms of mean and variance), based on a binary segmentation algorithm (Scott and Knott (1974)).")
         )
       )
    ),
    
    tabPanel("Time series clustering",
             column(
               width = 12,
               uiOutput("help_text_panel4"),
               box(
                 title = "Time series clustering",
                 width = NULL,
                 solidHeader = TRUE,
                 status = "primary",
                 plotOutput("ts_cluster_plot", width = "100%")
               )
             )
    )
  )
)


italy_side_panel <- sidebarPanel(
  # This is a hidden field acting as a placeholder,
  # allowing to treat the Italy tab the same way as
  # the World tab
  conditionalPanel("false",
    textInput(
      inputId = "italy_country",
      label = "",
      value = "Italy"
    )
  ),
  uiOutput("italy_region_selection"),
  
  selectInput(
    inputId = "italy_statistic",
    label = "Choose a statistic:",
    choices = stat_italy_label_dict,
    selected = stat_italy_label_dict[0]
  ),
  
  sliderInput(
    inputId = "italy_range",
    label = "Days of interest (0 = last day):",
    min = -60,
    max = 0,
    value = c(-40, 0)
  ),
  checkboxInput("log_scale_world_IT", "Use log scale for Y", FALSE),
  
  
  uiOutput("italy_sliders")
)


italy_main_panel <- mainPanel(column(
  width = 10,
  box(
    title = "Statistics",
    width = NULL,
    solidHeader = TRUE,
    status = "primary",
    plotOutput("chosen_stat_it_plot")
  ),
  uiOutput("italy_growth")
))


ranking_side_panel <- sidebarPanel(
  radioButtons("ranking_date", "Confinement action used in ranking", unlist(action_label_dict_rev)),
  helpText(
    "In this section, a t-test is performed to assess the difference between the
    distribution of the growth rate of confirmed cases, before and after
    an action is taken. We report here the resulting statistic,
    the one-sided p-value and the difference (delta) between the means.
    The p-values are adjusted using Holm's method."
  )
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


dashboardPage(

  dashboardHeader(title = "COVID-19 Impact of Policies", titleWidth = 350),
  dashboardSidebar(width = 125, sidebarMenu(
    menuItem("World", tabName = "world", icon = icon("bar-chart-o")),
    menuItem("Italy", tabName = "italy", icon = icon("bar-chart-o")),
    menuItem("Ranking", tabName = "ranking", icon = icon("bar-chart-o")),
    menuItem("About", tabName = "about", icon = icon("question"))
  )),
  
  dashboardBody(fluidPage(
    # Add a loading screen
    use_waiter(include_js = FALSE),
    tabItems(
      tabItem(tabName = "world",
              world_side_panel,
              world_main_panel),
      tabItem(tabName = "italy",
              italy_side_panel,
              italy_main_panel),
      tabItem(tabName = "ranking",
              ranking_side_panel,
              ranking_main_panel),
      tabItem(tabName = "about",
        fluidPage(
          tags$iframe(src = './about.html', 
                     width = '100%', height = '800px',
                     frameborder = 0, scrolling = 'auto'
          )
        )
      )
    ),
    # Show the loading screen at launch, it has to be the last UI element 
    waiter_show_on_load(color = "#ffffff", html = loading_screen("Downloading data..."))
  ))
)