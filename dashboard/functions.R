# The output of this functions contains one row for each original row and for each action,
# along with a logical column indicating whether this day was before or after the action,
# and a column with the name of the action. So by subsetting the result by action name,
# we have a clean view of before / after distribution for each country, easy to plot. 
to_long_format_on_action <- function(counts.df, country.df) {
  actions <- c("Date.Schools", "Date.Public Places", "Date.Gatherings", "Date.Stay at Home", "Date.Lockdown", "Date.Non-essential")
  res.df <- do.call("rbind", lapply(unique(counts.df$Country.Region), function(country) {
    do.call("rbind", lapply(actions, function(action) {
      action_date <- country.df[country.df$Country.Region == country &
                                country.df$Province.State == "", action]
      # If we have no date for the given action in this country
      if (length(action_date) != 1)
        return(NULL)
      if (is.na(action_date))
        return(NULL)
      country_counts.df <- counts.df[counts.df$Country.Region == country,]
      country_counts.df$BeforeAction <- country_counts.df$Date <= action_date
      country_counts.df$Action <- action
      return(country_counts.df)
    }))
  }))
  res.df <- res.df[!is.na(res.df$BeforeAction),]
  return(res.df)
}

# Computes a Z-value, that characterizes the difference of
# the mean of value_column before and after the lockdown
# action is taken. This function computes the statistic for
# every lockdown action, for every country, in every region.
# It also returns a one-sided p-value, with null hypothesis
# the mean of value_column after lockdown action is larger or
# equal to that before the action. So a low p-value indicates
# that value_column is probably lower after the action.
# The value is then adjusted using Holm's method (the default
# method from p.adjust).
compute_z_statistic <- function(actions.df, value_column) {
  # Create a list of dataframes, for each of these three functions.
  # Each resulting dataframe contains a column with the result of applying
  # the function for each country, province, action and whether we are before or after the action
  aggregates.list <- lapply(list(list(fn = mean, fn_name = "Mean"),
                                 list(fn = length, fn_name = "Length"),
                                 list(fn = sd, fn_name = "Sd")),
                            function(func_pair) {
                              results <- aggregate(x = actions.df[,value_column],
                                                   by = list(Country.Region = actions.df$Country.Region,
                                                             Province.State = actions.df$Province.State,
                                                             BeforeAction = actions.df$BeforeAction,
                                                             Action = actions.df$Action),
                                                   FUN = func_pair$fn)
                              # Aggregate names the new column "x", so we rename it properly
                              names(results)[names(results) == "x"] <- paste0(value_column, func_pair$fn_name)
                              return(results)
                            })
  
  # Merge the three dataframes, so we have one column per relevant statistic
  results <- merge(aggregates.list[[1]],
                   merge(aggregates.list[[2]],
                         aggregates.list[[3]],
                         by=c("Country.Region", "Province.State", "BeforeAction", "Action")),
                   by=c("Country.Region", "Province.State", "BeforeAction", "Action"))
  
  # Convert to wide format, so that we have the statistic before and after the action in separate columns
  results <- reshape(results,
                     idvar = c("Country.Region", "Province.State", "Action"),
                     timevar = "BeforeAction",
                     direction = "wide")
  
  # Compute the Z statistic
  results$Z <- (results[,paste0(value_column, "Mean.FALSE")] - results[,paste0(value_column, "Mean.TRUE")]) /
    ((results[,paste0(value_column, "Sd.FALSE")] / results[,paste0(value_column, "Length.FALSE")]) -
       (results[,paste0(value_column, "Sd.TRUE")] / results[,paste0(value_column, "Length.TRUE")]))
  
  
  results$p.value <- 1 - pnorm(results$Z, 0, 1)
  results$p.value.adj <- p.adjust(results$p.value)
  
  return(results[, c("Country.Region", "Province.State", "Action", "Z", "p.value", "p.value.adj")])
}


split_data <- function(province_country_pair, data.df) {
  data.df[data.df$Country.Region == province_country_pair[1] &
          data.df$Province.State == province_country_pair[2],]
}

setup_time_series <- function(global_data, value_column) {
  unique_pairs.df <- unique(global_data[,c("Country.Region", "Province.State")])
  unique_pairs.list <- mapply(c, unique_pairs.df$Country.Region, unique_pairs.df$Province.State, SIMPLIFY = FALSE)
  split_data.list <- lapply(unique_pairs.list, split_data, global_data)
  cases_gr_ts.list <- lapply(split_data.list,
                                 function(data.df) {zoo(data.df[,c(value_column)],
                                                        order.by = data.df$Date)})
  cases_gr_ts.zoo <- Reduce(merge, cases_gr_ts.list)
  colnames(cases_gr_ts.zoo) <- lapply(unique_pairs.list, paste, collapse="_")
  return(cases_gr_ts.zoo)
}