
extreme_threshold = function(data, probs = .9) {
  
  # Group by location and compute threshold
  thresholds = data[,.(us = quantile(qt, probs = probs)), by = stat_id]
  return(as.data.table(thresholds))
}

extreme_events = function(data,
                          extreme_thresholds = NULL,
                          probs = .9) {
  
  # Retrieve unique locations
  uniquestations = unique(data$stat_id)
  
  # If thresholds are not provided, run extreme_threshold()
  if (is.null(extreme_thresholds)) {
    extreme_thresholds = extreme_threshold(data = data, probs = probs)
  }
  
  # Iterate through all locations
  for (i in 1:length(uniquestations)) {
    
    # Retrieve location and the location-specific extreme threshold
    this.station = data[stat_id == uniquestations[i]]
    this.threshold = extreme_thresholds[stat_id == uniquestations[i]]$us
    
    # Subset observations with measurements >= the threshold
    extreme.events.temp = this.station[qt >= this.threshold]
    
    # Keep only location, date and measurement
    extreme.events.temp = extreme.events.temp[,c("stat_id", "date", "qt")]
    
    # Bind all location-wise extreme events
    if (i == 1) {
      extreme.events = extreme.events.temp
    } else {
      extreme.events = rbind(extreme.events, extreme.events.temp)
    }
  }
  
  return(as.data.table(extreme.events))
  
}

time_threshold = function(deltaT) {
  return(deltaT)
}

main_events = function(extreme.events,
                       day_threshold = 7) {
  event.frequencies = extreme.events[,.(Freq = .N), by = date]
  setorder(event.frequencies, cols = - Freq)
  
  uniqueevents = as.character(event.frequencies$date)
  
  for (i in 1:length(uniqueevents)) {
    event = event.frequencies[date == uniqueevents[i]]
    
    event.freq = event$Freq
    
    event.range.lower = as.Date(event$date) - days(day_threshold)
    event.range.upper = as.Date(event$date) + days(day_threshold)
    event.range = seq(event.range.lower, event.range.upper, by = 1)
    
    proximity.events = event.frequencies[date %in% event.range]
    alternative.event = proximity.events[Freq == event.freq]
    
    event.to.keep = quantile(alternative.event$date, p = .5, type = 1)
    event.to.keep = as.Date(event.to.keep)
    
    if (i == 1) {
      main.events = event.to.keep
    } else {
      if (!any(abs(difftime(event.to.keep, main.events, units = "days")) < day_threshold)) {
        main.events = c(main.events, event.to.keep)
      }
    }
  }
  
  return(main.events)
  
}

event_matrix = function(main.events,
                        extreme.events,
                        deltaT = 2) {
  
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
        mat[j, i] = 1
      } else {
        mat[j, i] = 0
      }
    }
  }
  
  return(mat)
  
}
