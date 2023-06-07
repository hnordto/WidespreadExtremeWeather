
# Create a suitable discharge data set
source("dev/discharge_intervals.R")

thresholds = extreme_threshold(discharge.data)

uniquestations = unique(discharge.data$stat_id)

extreme_events = extreme_events(discharge.data, thresholds)
