## app.R ##
library(dplyr)
library(shiny)
library(shinydashboard)
library(ggplot2)
library(zoo)
library(imputeTS)
library(scales)
library(TSclust)
library(changepoint)
library(changepoint.np)
library(waiter)

source("functions.R")
source("common.R")


server <- function(input, output) {
  # Download Data
  info_data <- readRDS(url(
    "https://github.com/TheoVerhelst/ulb-mlg-time-series-impact/blob/master/data/COVID19_Country_Info.Rdata?raw=true"))
  global_data <- readRDS(url(
    "https://github.com/TheoVerhelst/ulb-mlg-time-series-impact/blob/master/data/COVID19_Global_Italy_wGrowth.Rdata?raw=true"))
  
  waiter_update(html = loading_screen("Preprocessing data..."))
  
  time_series_colnames <- c("ConfirmedGrowthRate", "DeathsGrowthRate", "RecoveredGrowthRate")
  time_series_by_stat <- setup_time_series(global_data, time_series_colnames)
  names(time_series_by_stat) <- time_series_colnames
  # Split between Italian and Global Data
  is_regional_italy <- (global_data$Country.Region == "Italy") & (global_data$Province.State != "")
  italian_data <- global_data[is_regional_italy, ]
  global_data <- global_data[!is_regional_italy, ]
  
  global_data_by_action <- to_long_format_on_action(global_data, info_data)
  global_data_by_action <- global_data_by_action[is.finite(global_data_by_action$ConfirmedGrowthRate),]
  
  # Make lists for input panels
  countries <- unique(global_data["Country.Region"])
  countries_with_regions <- unique(global_data[global_data$Province.State != "", "Country.Region"])
  italian_regions <- unique(italian_data["Province.State"])
  
  # To use together with the function format, on dates
  date_format <- "%b %d"
  
  # Hide the loading screen
  waiter_hide()
  
  ##############
  ##RENDERINGS##
  ##############
  
  get_region <- reactive({
    if (is.null(input$country))
      return("")
    else if (input$country %in% countries_with_regions)
      return(input$region)
    else
      return("")
  })
  
  # Create the country selector for the World tab
  # It is defined here because it depends on the dataset
  output$country_selector <- renderUI({
    selectInput(
      inputId = "country",
      label = "Choose a country:",
      selected = countries[83,],
      choices = countries
    )
  })
  
  ## Render the region selector if a country with regions is chosen
  output$region_selector <- renderUI({
    # I can't get logical short circuit to work here
    if (is.null(input$country))
      return(NULL)
    else if (!input$country %in% countries_with_regions)
      return(NULL)
    else
      return(
      selectInput(
          inputId = 'region',
          label = 'Choose a region:',
          choices = unique(global_data[global_data$Country.Region == input$country , "Province.State"])
        )
    )
  })
  
  # Create the region selector for the Italy tab
  # It is defined here because it depends on the dataset
  output$italy_region_selection <- renderUI({
    italian_regions <- unique(italian_data["Province.State"])
    selectInput(
      inputId = "italy_region",
      label = "Choose a region:",
      selected = italian_regions[1],
      choices = italian_regions
    )
    
  })
  
  output$italy_sliders <- renderUI({
    if (input$italy_statistic %in% c("Confirmed","Deaths","Recovered")) {
      box(
        width = 150,
        sliderInput(
          "smooth_growth_rate_italy",
          label = "Degree of growth-rate smoothing:",
          min = 0,
          max = 10,
          value = 0
        ),
        
        sliderInput(
          "linear_fitting_italy",
          label = "Number of last days fitted",
          min = 5,
          max = 10,
          value = 5
        )
      )
    } else {
      return(NULL)
    }
  })
  
  output$italy_growth <- renderUI({
    if(input$italy_statistic %in% c("Confirmed","Deaths","Recovered")){
      box(
        title = "Growth_Rate",
        width = NULL,
        solidHeader = TRUE,
        status = "primary",
        plotOutput("italy_growth_plot")
      )
    } else {
      return(NULL)
    }
  })
  
  make_help <- function() renderUI({
    relevant_row <- (info_data$Country.Region == input$country) &
           (info_data$Province.State == get_region())
    if(sum(relevant_row) == 0) {
      HTML("<strong style='color:red'>Selected Date is not available for the specified country</strong>")
    } else if (is.na(info_data[relevant_row, input$dates])) {
      HTML("<strong style='color:red'>Selected Date is not available for the specified country</strong>")
    } else {
      return(NULL)
    }
  })
  
  ## Render the help text panel 1 if a date with NA is chosen
  output$help_text_panel1 <- make_help()
  output$help_text_panel2  <- make_help()
  output$help_text_panel3  <- make_help()
  
  make_generic_plot <- function(data, country_name, region_name, date_range_name, stat_to_plot_name, log_scale_name, linear_fitting_name, smooth_growth_rate_name, is_growth_rate, is_italy_tab)  renderPlot({
    # the req function stops the rendering if the corresponding inputs are not yet available
    # this avoids an error early in the loading of the page, when the UI is loaded after
    # this portion of code.
    country <- req(input[[country_name]])
    region <- ifelse(is_italy_tab, req(input[[region_name]]), get_region())
    min_date <- Sys.Date() + input[[date_range_name]][1]
    max_date <- Sys.Date() + input[[date_range_name]][2]
    stat_to_plot <- paste0(input[[stat_to_plot_name]], ifelse(is_growth_rate, "GrowthRate", ""))
    
    data <- data[(data$Country.Region == country) & (data$Province.State == region) &
                 (data$Date >= min_date) & (data$Date <= max_date),]
    
    country_info <- info_data[(info_data$Country.Region == country) & (info_data$Province.State == region),]
    texts_to_show <- as.character(input$dates)
    dates_to_show <- do.call("c", lapply(texts_to_show, function(col) country_info[, col]))
    
    # Remove NAs to avoid a warning
    texts_to_show <- texts_to_show[!is.na(dates_to_show)]
    dates_to_show <- dates_to_show[!is.na(dates_to_show)]
    
    dates_to_show_lab <- format(dates_to_show, date_format)
    events <- unlist(action_label_dict[texts_to_show])
    days <- unlist(dates_to_show_lab)
    
    p <- ggplot(data, aes_string(x = "Date", y = stat_to_plot)) +
      geom_line() +
      geom_vline(xintercept = dates_to_show) +
      xlab("Date") +
      ylab(stat_to_plot) +
      annotate("text", x = dates_to_show, y = 0, angle = 90, vjust = 1.5, hjust=-0.5, label =  paste(events, days, sep = ": ")) + 
      scale_y_continuous(trans=ifelse(input[[log_scale_name]] && !is_growth_rate, "log10", "identity")) +
      theme_bw()
    
    
    if (is_growth_rate) {
      # Perform smoothing
      data_smooth <- data[, c("Date", stat_to_plot)]
      data_smooth$smooth <- data_smooth[, stat_to_plot]
      data_smooth[(is.nan(data_smooth[, stat_to_plot]))|(is.infinite(data_smooth[, stat_to_plot])), stat_to_plot] <- 0
      data_smooth[(is.na(data_smooth[,"smooth"])) | (is.infinite(data_smooth[,"smooth"])), "smooth"] <- 0

      w <- input[[smooth_growth_rate_name]]
      if (w >= 1){
        for (i in seq(1:nrow(data_smooth))){
          if (i <= w) data_smooth[i,"smooth"] <- mean(data_smooth[0:i, stat_to_plot])
          if (i > w) data_smooth[i,"smooth"] <- mean(data_smooth[(i-w):i, stat_to_plot])
          #if (i >= nrow(dataset)-(w/2)) dataset[i,"smooth"] <- mean(dataset[i-w:i, stat_to_plot])
        }
      }
      p <- p + geom_line(data_smooth, mapping = aes_string(x = "Date", y = "smooth"), color = "blue")
      
      # Perform linear fitting on the last days
      min_fit_date = max_date - input[[linear_fitting_name]] - 2
      max_fit_date = max_date
      data_to_fit <- data[(data$Date >= min_fit_date) & (data$Date <= max_fit_date),]
      data_to_fit <- data_to_fit[!is.na(data_to_fit[,stat_to_plot]),]
      
      if (nrow(data_to_fit) > 3) {
        fitted <- lm(formula = paste(stat_to_plot, "~ Date"), data = data_to_fit)
        date_p_val <- summary(fitted)$coefficients["Date", 4]
    
        predicted <- data.frame(predicted = predict(fitted, newdata = data_to_fit["Date"]), Date = data_to_fit["Date"])
        
        date_coef <- summary(fitted)$coefficients["Date", 1]
        intercept <- summary(fitted)$coefficients["(Intercept)", 1]
        root <- as.Date(-intercept / date_coef)
        
        if (is.finite(root) & date_coef < 0) {
          root_text <- paste("growth rate null on", format(root, date_format))
        } else {
          root_text <- "growth rate increases"
        }
        
        p <- p +
          geom_line(data = predicted, aes(x = Date, y = predicted), color = "red", size = 1.5) +
          annotate("text",
                   x = predicted[1, "Date"], y = predicted[1, "predicted"], vjust = -1.5, hjust = 0,
                   label =  paste("p =", format(date_p_val, digits = 2), "\n", root_text))
      }
    }
    p
  })

  ########
  ##PLOTS#
  ########  
  
  output$cases_plot <- make_generic_plot(global_data, "country", "region", "range",
                                         "world_stat", "log_scale_world",
                                         "linear_fitting_world", "smooth_growth_rate_world",
                                         is_growth_rate = FALSE, is_italy_tab = FALSE)
  output$growth_plot <- make_generic_plot(global_data, "country", "region", "range",
                                          "world_stat", "log_scale_world",
                                          "linear_fitting_world", "smooth_growth_rate_world",
                                          is_growth_rate = TRUE, is_italy_tab = FALSE)
  output$chosen_stat_it_plot <- make_generic_plot(italian_data, "italy_country", "italy_region", "italy_range",
                                                "italy_statistic", "log_scale_world_IT",
                                                "linear_fitting_italy", "smooth_growth_rate_italy",
                                                is_growth_rate = FALSE, is_italy_tab = TRUE)
  output$italy_growth_plot <- make_generic_plot(italian_data, "italy_country", "italy_region", "italy_range",
                                                "italy_statistic", "log_scale_world_IT",
                                                "linear_fitting_italy", "smooth_growth_rate_italy",
                                                is_growth_rate = TRUE, is_italy_tab = TRUE)
  
  output$action_comp_plot <- renderPlot({
    min_date = Sys.Date() + input$range[1]
    max_date = Sys.Date() + input$range[2]
    
    dataset <- global_data_by_action[(global_data_by_action$Country.Region == input$country) &
                           (global_data_by_action$Province.State == get_region()) &
                           (global_data_by_action$Date >= min_date) &
                           (global_data_by_action$Date <= max_date),]
    dataset <- dataset[dataset$Action == as.character(input$dates),]
    stat_to_plot <- paste0(input$world_stat, "GrowthRate")
    ggplot(dataset, aes_string(x = "BeforeAction", color="BeforeAction", y = stat_to_plot)) + 
      geom_boxplot(show.legend = FALSE) +
      scale_color_brewer(type = "qual", palette = 2) +
      xlab("Comparison before and after the action") +
      ylab("Growth Rate distribution")+
      scale_x_discrete(labels=c("Before the action", "After the action"), limits = c(T, F)) +
      theme_bw()
  })
  
  
  
  output$change_point_plot <- renderPlot({
    # Select the right statistic
    to_plot.ts <- time_series_by_stat[paste0(input$world_stat, "GrowthRate")][[1]]
    # Select the right country
    country_pair <- paste0(input$country, "_", get_region())
    to_plot.ts <- to_plot.ts[,country_pair]
    # Select the right date range
    min_date = Sys.Date() + input$range[1]
    max_date = Sys.Date() + input$range[2]
    to_plot.ts <- to_plot.ts[(index(to_plot.ts) >= min_date) & (index(to_plot.ts) <= max_date)]
    to_plot.ts <- to_plot.ts[!is.na(to_plot.ts)]
    to_plot.ts <- to_plot.ts[is.finite(to_plot.ts)]
    to_plot.df <- data.frame(Date = index(to_plot.ts), Value = coredata(to_plot.ts))
    
    # Show lines with important dates
    country_info <- info_data[(info_data$Country.Region == input$country) &
                              (info_data$Province.State == get_region()),]
    texts_to_show <- as.character(input$dates)
    dates_to_show <- do.call("c", lapply(texts_to_show, function(col) country_info[, col]))
    # Remove NAs to avoid a warning
    texts_to_show <- texts_to_show[!is.na(dates_to_show)]
    dates_to_show <- dates_to_show[!is.na(dates_to_show)]
    
    p <- ggplot(to_plot.df, aes(x = Date, y = Value)) +
      geom_line() +
      geom_vline(xintercept = dates_to_show) +
      xlab("Date") +
      ylab(paste(input$world_stat, "growth rate")) +
      annotate("text", x = dates_to_show, y = 0, angle = 90, vjust = 1.5, hjust=-1.5, label = unlist(action_label_dict[texts_to_show])) +
      theme_bw()
    
    # Compute change point detection
    Q <- 10
    if (length(to_plot.ts) > Q) {
      to_plot.cpt <- cpt.meanvar(to_plot.ts, test.stat='Normal', method='BinSeg', Q=Q, penalty="SIC")
      changepoints <- cpts(to_plot.cpt)
      # Plot them if there is any change point at all
      if (length(changepoints) > 0) {
        means_cp <- sapply(1:length(changepoints), function(i)
            mean(to_plot.ts[ifelse(i == 1, 1, changepoints[i - 1]):changepoints[i]]))
        means_cp <- c(means_cp, mean(to_plot.ts[tail(changepoints, 1):length(to_plot.ts)]))
        endpoint_cp_date <- c(index(to_plot.ts)[changepoints], tail(index(to_plot.ts), 1))
        segments.df <- data.frame(
            x=c(index(to_plot.ts)[1], endpoint_cp_date[1:length(endpoint_cp_date) - 1]),
            xend=endpoint_cp_date,
            y=means_cp,
            yend=means_cp)
        
        p <- p  + geom_segment(data = segments.df,
           aes(x = x, xend = xend, y = y, yend = yend),
           color="blue")
      }
    }
    p
  })
  
  output$ts_cluster_plot <- renderPlot({
    # Select the right statistic
    to_plot.ts <- time_series_by_stat[paste0(input$world_stat, "GrowthRate")][[1]]
    # Select the right country
    selected_countries <- grepl(paste0(european_countries,sep="_$",collapse="|"),
                                colnames(to_plot.ts))
    to_plot.ts <- to_plot.ts[,selected_countries]
    # Select the right date range
    min_date = Sys.Date() + input$range[1]
    max_date = Sys.Date() + input$range[2]
    to_plot.ts <- to_plot.ts[(index(to_plot.ts) >= min_date) & (index(to_plot.ts) <= max_date)]
    # to_plot.ts <- to_plot.ts[!is.na(to_plot.ts)]
    # to_plot.ts <- to_plot.ts[is.finite(to_plot.ts)]
    to_plot.df <- data.frame(Date = index(to_plot.ts), Value = coredata(to_plot.ts))
    
    # Compute distance matrix
    to_plot.ts[which(!is.finite(to_plot.ts))] <- 0
    to_plot.dm <- diss(data.frame(to_plot.ts),input$clustering_distance) # distance matrix
    to_plot.dm[is.na(to_plot.dm)] <- max(to_plot.dm,na.rm = T) + 1
    
    # Compute clustering
    to_plot.hclust <- hclust(to_plot.dm, method="ward.D")
    
    # Cut dendrogram to find clusters
    to_plot_clusters <- data.frame(cutree(to_plot.hclust, k = input$clusters))
    colnames(to_plot_clusters) <- c("Cluster")
    to_plot_clusters$Series <- rownames(to_plot_clusters)
    
    p <- ggplot(merge(fortify(to_plot.ts,melt=T),to_plot_clusters),
                aes(x=Index,y=Value,color=Series)) +
      geom_line() +
      facet_grid(Cluster ~ ., labeller=label_both) +
      scale_x_date(labels = date_format("%m/%d"),
                   breaks=date_breaks("days")) +
      labs(y=paste(input$world_stat, "Growth Rate"),
           x="Time",title="European Countries") +
      theme_bw() +
      theme(legend.position = 'bottom',
            axis.text.x = element_text(angle=45))
    
    p
  })
  
  output$ranking_plot <- renderPlot({
    # ConfirmedGrowthRate is hardcoded because we have barely any data for the others
    t_stat <- compute_t_statistic(global_data_by_action[global_data_by_action$Province.State == "",], "ConfirmedGrowthRate")
    t_stat <- t_stat[t_stat$Action == input$ranking_date,]
    t_stat <- t_stat[!is.na(t_stat$t.value),]
    ggplot(t_stat, aes(x = reorder(Country.Region, t.value, sum), y = t.value)) +
      geom_col(fill = "lightblue") +
      geom_text(aes(label = paste("p =", prettyNum(p.value.adj), ", delta =", format(delta, digits = 2))), position = position_stack(vjust = .5)) +
      coord_flip() +
      xlab("Country") +
      ylab("T-test statistic (with adjusted p-value and mean difference)") +
      scale_color_brewer(type = "qual", palette = 2) +
      theme_bw()
  })
}
