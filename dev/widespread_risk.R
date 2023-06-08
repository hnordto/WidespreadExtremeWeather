
# Create a suitable discharge data set
source("dev/discharge_intervals.R")

# Declare functions needed
source("R/main_functions.R")
source("R/plot_functions.R")

# Compute extreme thresholds
thresholds = extreme_threshold(discharge.data, probs = .9)

# Identify extreme events
extreme.events = extreme_events(discharge.data, thresholds)


plot_discharge(discharge.data, thresholds = thresholds)

# MAIN EVENT IDENTIFICATION

extreme_events |> 
  group_by(date) |> 
  summarise(Freq = n()) |> 
  arrange(desc(Freq)) -> test

day_threshold = 7

# Does not work, only for testing!
for (i in 1:nrow(test)) {
  if (i == 1) {
    main_events = test[i,]
  } else {
    last_event = tail(main_events, n = 1)
    main_events_potential = test[i,]
    
    if (abs(difftime(main_events_potential$date, last_event$date, units = "days")) > day_threshold) {
      main_events = rbind(main_events, main_events_potential)
    }
    
  }
}

test2 = merge(extreme_events, test, by = "date", all.x = T)
test2 = test2 |> arrange(desc(Freq)) |> as.data.table()

uniqueevents = as.character(unique(test2$date))




