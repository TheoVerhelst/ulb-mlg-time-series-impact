library(shiny)

european_countries <- c("Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czechia",
                        "Denmark", "Estonia", "Finland", "Germany", "Greece",
                        "Hungary", "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg", "Malta",
                        "Netherlands", "Poland", "Portugal", "Romania", "Slovakia", "Slovenia", "Spain", "Sweden")


action_label_dict <- c("Date.Schools" = "Schools closure",
                       "Date.Public Places" = "Public places shut down",
                       "Date.Gatherings" = "Gatherings ban",
                       "Date.Stay at Home" = "Stay at home",
                       "Date.Non-essential" = "Non-essential activities ban")

# Create the same dict mapping, but reversed (for creating radio buttons)
action_label_dict_rev <- names(action_label_dict)
names(action_label_dict_rev) <- unname(action_label_dict)


stat_global_label_dict <- c("Confirmed cases" = "Confirmed",
                            "Deaths" = "Deaths",
                            "Recovered" = "Recovered")

stat_italy_label_dict <- c("Confirmed cases" = "Confirmed",
                           "Recovered cases" = "Recovered",
                           "Deaths" = "Deaths",
                           "Hospitalized with symptoms" = "HospitalizedWSymptoms",
                           "In intensive care unit" = "ICU",
                           "Total hospitalized" = "TotalHospitalized",
                           "In home isolation" = "HomeIsolation",
                           "Daily confirmed cases" = "DailyConfirmed",
                           "Total cases" = "Total",
                           "Tests performed" = "Tests")

RAWDATA_BASED <- c("DTWARP", "FRECHET", "EUCL")
AUTOCORRELATION_BASED <- c("ACF", "PACF")
CORRELATION_BASED <- c("COR")
PROXIMITYVALUE_BASED <- c("CORT")
PERIODOGRAM_BASED <- c("PER", "INT.PER")
SPECTRAL_BASED <- c("SPEC.LLR", "SPEC.GLK", "SPEC.ISD")
MODEL_BASED <- c("AR.MAH", "AR.PIC", "AR.LPC.CEPS")
COMPLEXITY_BASED <- c("PDC","CDM","CID", "NCD")
SYMBOLIC_BASED <- c("MINDIST.SAX")
PREDICTION_BASED <- c("PRED")
WAVELET_BASED <- c("DWT")
TS_CLUST_DISTANCES <- c(RAWDATA_BASED,
                        AUTOCORRELATION_BASED,
                        CORRELATION_BASED,
                        PROXIMITYVALUE_BASED,
                        PERIODOGRAM_BASED,
                        SPECTRAL_BASED,
                        MODEL_BASED,
                        COMPLEXITY_BASED,
                        SYMBOLIC_BASED,
                        PREDICTION_BASED,
                        WAVELET_BASED)

loading_screen <- function(text) {
  tagList(
      spin_loaders(id = 24, color = "grey"),
      h4(text, style = "color:gray;")
    )
}