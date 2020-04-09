library(reshape)
library(plyr)

horizontaldf2verticaldf <- function(horizontal.df, value_column_name){
  colnames(horizontal.df) <- c(c("Province.State", "Country.Region", "Lat", "Long"), seq(as.Date("2020-01-22"), length = ncol(horizontal.df) - 4, by = "days"))
  vertical.df <- melt(horizontal.df,
                      id = c("Province.State","Country.Region","Lat","Long"))
  colnames(vertical.df) <- c(c("Province.State", "Country.Region", "Lat", "Long"), "Date", value_column_name)
  vertical.df$Date <- as.Date(as.integer(vertical.df$Date), origin = "21/01/2020", format = "%d/%m/%Y")
  return(vertical.df)
}

# Given a dataframe df of data and a list of country / province pairs,
# this functions adds a row to df for each element of pairs not present in df.
# This is used because we have regional data for deaths and confirmed cases,
# but not for recovery in some countries, and thus the merge discard these countries
add_missing_rows <- function(df, pairs, value_column_name) {
  for (i in nrow(pairs)) {
    confirmed_row <- df[(df$Country.Region == pairs[i, "Country.Region"]) &
                        (df$Province.State == pairs[i, "Province.State"]),]
    if (nrow(confirmed_row) == 0) {
      new_row <- df[df$Country.Region == pairs[i, "Country.Region"],][1,]
      new_row[,value_column_name] <- NA
      new_row[,"Province.State"] <- pairs[i, "Province.State"]
      df <- rbind(df, new_row)
    }
  }
  return(df)
}

growth_rate_normalized <- function(ts){
  # x(t) = head(x,-1)
  # x(t+1) = tail(x,-1)
  res <- c(NaN, (tail(ts,-1) - head(ts, -1)) / head(ts,-1))
  res[!is.finite(res)] <- NA
  return(res)
}

split_data <- function(province_country_pair,data.df) {
  data.df[data.df$Province.State == province_country_pair[1] &
            data.df$Country.Region == province_country_pair[2],]
}

compute_growth_rate_country <- function(country.df, growth_rate_function) {
  country.df$ConfirmedGrowthRate <- growth_rate_function(country.df$Confirmed)
  country.df$RecoveredGrowthRate <- growth_rate_function(country.df$Recovered)
  country.df$DeathsGrowthRate <- growth_rate_function(country.df$Deaths)
  return(country.df)
}

compute_growth_rate_global <- function(data.df) {
  # Split data accoring to unique Country-Province Pair
  unique_pairs.df <- unique(data.df[,c("Province.State", "Country.Region")])
  unique_pairs.list <- mapply(c, as.character(unique_pairs.df$Province.State), unique_pairs.df$Country.Region, SIMPLIFY = FALSE)
  split_data.list <- lapply(unique_pairs.list, split_data,data.df)
  augmented_data.list <- lapply(split_data.list, compute_growth_rate_country, growth_rate_normalized)
  Reduce(rbind, augmented_data.list)
}


###############################
# Data source update
###############################

# First, download the JHU CSSE data
jhu_csse_url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/"
jhu_csse_dir <- "data/Global_JohnsHopkins/"

for (file in paste0("time_series_covid19_", c("confirmed", "deaths", "recovered"), "_global.csv")) {
  download.file(
    url = paste0(jhu_csse_url, file),
    destfile = paste0(jhu_csse_dir, file),
    mode = "wb")
}

# Then Italy data
download.file(
  url = "https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-regioni/dpc-covid19-ita-regioni.csv",
  destfile = "data/Italy/dpc-covid19-ita-regioni.csv",
  mode = "wb")

# We can't automate the download from Kaggle, since it requires authentication

###############################
# Global data preprocessing
###############################
# Load global data
global_confirmed.df <- read.csv(paste0(jhu_csse_dir, "time_series_covid19_confirmed_global.csv"))
global_deaths.df <- read.csv(paste0(jhu_csse_dir, "time_series_covid19_deaths_global.csv"))
global_recovered.df <- read.csv(paste0(jhu_csse_dir, "time_series_covid19_recovered_global.csv"))

# Transform horizontal to vertical
global_confirmed_vertical.df <- horizontaldf2verticaldf(global_confirmed.df, "Confirmed")
global_deaths_vertical.df <- horizontaldf2verticaldf(global_deaths.df, "Deaths")
global_recovered_vertical.df <- horizontaldf2verticaldf(global_recovered.df, "Recovered")

global_confirmed_vertical.df[,"Country.Region"] <- as.character(global_confirmed_vertical.df[,"Country.Region"])
global_confirmed_vertical.df[,"Province.State"] <- as.character(global_confirmed_vertical.df[,"Province.State"])
global_deaths_vertical.df[,"Country.Region"] <- as.character(global_deaths_vertical.df[,"Country.Region"])
global_deaths_vertical.df[,"Province.State"] <- as.character(global_deaths_vertical.df[,"Province.State"])
global_recovered_vertical.df[,"Country.Region"] <- as.character(global_recovered_vertical.df[,"Country.Region"])
global_recovered_vertical.df[,"Province.State"] <- as.character(global_recovered_vertical.df[,"Province.State"])

# Get the list of all country / province pairs in all global dataframes (not necessarily present in all of them)
pairs <- unique(rbind(
  global_confirmed_vertical.df[,c("Country.Region", "Province.State")],
  global_deaths_vertical.df[,c("Country.Region", "Province.State")],
  global_recovered_vertical.df[,c("Country.Region", "Province.State")]))

global_confirmed_vertical.df <- add_missing_rows(global_confirmed_vertical.df, pairs, "Confirmed")
global_deaths_vertical.df <- add_missing_rows(global_deaths_vertical.df, pairs, "Deaths")
global_recovered_vertical.df <- add_missing_rows(global_recovered_vertical.df, pairs, "Recovered")

global_merged.df <- merge(global_confirmed_vertical.df,
                          merge(global_recovered_vertical.df,global_deaths_vertical.df,
                                by = c("Province.State","Country.Region","Date")),
                          by = c("Province.State","Country.Region","Date"))


###############################
# Italy data preprocessing
###############################
# Load italy data
italy.df <- read.csv("data/Italy/dpc-covid19-ita-regioni.csv")

# Translate columns
colnames(italy.df) <- c("Date","Country.Region","RegionCode","Province.State","Lat","Long","HospitalizedWSymptoms","ICU","TotalHospitalized","HomeIsolation","Confirmed","DailyConfirmed","Recovered","Deaths","Total","Tests")
italy.df$Country.Region <- "Italy"
italy.df$Date <- as.Date(italy.df$Date)

# Rearrange columns and drop useless columns
italy.df <- italy.df[,c(4,2,5,6,1,11,13,14,7,8,9,10,12,15,16)]

# Merge with global dataset and fill with NA
output.df <- rbind.fill(italy.df, global_merged.df)

# Sort data by date and then by country in alphabetic order
output.df <- output.df[order(output.df$Country.Region, output.df$Date),]

# Add growth rate data
output.df <- compute_growth_rate_global(output.df)

saveRDS(output.df, "data/COVID19_Global_Italy_wGrowth.Rdata", version = 2)

###############################
# Country info preprocessing
###############################
# Preprocess extra country data
restrictions.df <- read.csv("data/Kaggle_CountryInfo/restrictions_columnwise_updated.csv")
countryinfo.df <- read.csv("data/Kaggle_CountryInfo/covid19countryinfo.csv")

# Rename columns for consistency
names(restrictions.df)[names(restrictions.df) == "Date.Public.Places"] <- "Date.Public Places"
names(restrictions.df)[names(restrictions.df) == "Date.Stay.at.Home"] <- "Date.Stay at Home"
names(restrictions.df)[names(restrictions.df) == "Date.Non.essential"] <- "Date.Non-essential"
names(restrictions.df)[names(restrictions.df) == "notes"] <- "Notes"
names(countryinfo.df)[names(countryinfo.df) == "country"] <- "Country.Region"

# Remove unused columns
# We will have this data after we merge it with restrictions.df
countryinfo.df[c("quarantine", "schools", "restrictions")] <- NULL
restrictions.df[c("mandatory", "Gatherings.limit", "Date.Date.Gatherings")] <- NULL

# Convert all date column to proper R date type
for (colname in c("Date.Schools", "Date.Public Places", "Date.Gatherings", "Date.Stay at Home", "Date.Lockdown", "Date.Non-essential")) {
  dates <- as.character(restrictions.df[,colname])
  dates[dates == ""] <- NA
  restrictions.df[,colname] <- as.Date(dates)
}

# Various other small fixes
for (col in names(restrictions.df)) {
  if (is.factor(restrictions.df[,col]))
    restrictions.df[,col] <- as.character(restrictions.df[,col])
}
countryinfo.df$pop <- as.numeric(countryinfo.df$pop)
restrictions.df[restrictions.df$Country.Region == restrictions.df$Province.State, "Province.State"] <- ""

# Merge country extra data and lockdown dates
# We have two sources of data for stay at home / lockdown dates,
# we will keep in priority the ones from the online dataset.
restrictions.df[is.na(restrictions.df[,"Date.Stay at Home"]), "Date.Stay at Home"] <- restrictions.df[is.na(restrictions.df[,"Date.Stay at Home"]), "Date.Lockdown"]
restrictions.df[,"Date.Lockdown"] <- NULL

# Save the result
country_info_output.df <- merge(restrictions.df, countryinfo.df, by = c("Country.Region"))
saveRDS(country_info_output.df, "data/COVID19_Country_Info.Rdata", version = 2)