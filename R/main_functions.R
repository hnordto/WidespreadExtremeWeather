
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
