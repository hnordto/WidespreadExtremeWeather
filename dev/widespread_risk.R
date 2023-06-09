
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

extreme.events |> 
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

test2 = merge(extreme.events, test, by = "date", all.x = T)
test2 = test2 |> arrange(desc(Freq)) |> as.data.table()

uniqueevnts = as.character(unique(test$date))

day_threshold = 7

main.events <- c()
for (i in 1:length(uniqueevents)) {
  event = test[date == uniqueevents[i]]
  
  event.freq = event$Freq
  
  event.range.lower = as.Date(event$date) - days(day_threshold)
  event.range.upper = as.Date(event$date) + days(day_threshold)
  event.range = seq(event.range.lower, event.range.upper, by = 1)
  
  proximity.events = test[date %in% event.range]
  alternative.event = proximity.events[Freq == event.freq]
  
  event.to.keep = quantile(alternative.event$date, p = 0.5, type = 1)
  event.to.keep = as.Date(event.to.keep)
  
  if (i == 1) {
    main.events <- as.Date(event.to.keep)
  } else {
    if (!any(abs(difftime(event.to.keep, main.events, units = "days")) < day_threshold)) {
      main.events = c(main.events, event.to.keep)
    }
  }
  
}

### Binary matrix

deltaT = 2

allstations = unique(extreme.events$stat_id)

mat = matrix(data = NA, 
             nrow = length(allstations),
             ncol = length(main.events))

rownames(mat) = allstations
colnames(mat) = as.character(main.events)

for (i in 1:length(main.events)) {
  this.event = main.events[i]
  event.range.lower = as.Date(this.event) - days(deltaT)
  event.range.upper = as.Date(this.event) + days(deltaT)
  event.range = seq(event.range.lower, event.range.upper, by = 1)
  
  for (j in 1:length(allstations)) {
    this.station = extreme.events[stat_id == allstations[j]]
    
    if (any(this.station$date %in% event.range)) {
      mat[j,i] = 1
    } else {
      mat[j,i] = 0
    }
  }
  
}

