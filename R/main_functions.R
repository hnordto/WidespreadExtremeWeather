
extreme_threshold = function(data, probs = .9) {
  
  # Group by location and compute threshold
  thresholds = data[,.(us = quantile(qt, probs = probs)), by = stat_id]
  return(as.data.table(thresholds))
}

extreme_events = function(data,
                          extreme_thresholds = NULL,
                          probs = .9,
                          type = "discharge",
                          precip_fixed = 5) {
  
  if (type == "discharge") {
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
  } else if (type == "precip_fixed") {
    extreme.events = data[,c("stat_id", "date", "qt")]
    extreme.events = extreme.events[qt >= precip_fixed]
  }
  
  return(as.data.table(extreme.events))
}


main_events = function(extreme.events,
                       day_threshold = 7,
                       type = "discharge",
                       timepoint_threshold = 48) {
  
  # Compute location-frequencies of each extreme event
  event.frequencies = extreme.events[,.(Freq = .N), by = date]
  setorder(event.frequencies, cols = - Freq)
  
  if (type == "discharge") {
    # Retrieve all unique events
    uniqueevents = as.character(event.frequencies$date)
    
    # Iterate over all unique events
    # The order of the data frame ensures that events with the highest location-
    # -frequencies are prioritized when appending to main events list
    for (i in 1:length(uniqueevents)) {
      # Retrieve event
      event = event.frequencies[date == uniqueevents[i]]
      
      event.freq = event$Freq
      
      # Create a time interval around the event corresponding to day_threshold
      event.range.lower = as.Date(event$date) - days(day_threshold)
      event.range.upper = as.Date(event$date) + days(day_threshold)
      event.range = seq(event.range.lower, event.range.upper, by = 1)
      
      # Check if multiple events in the time interval have equal location-frequencies
      proximity.events = event.frequencies[date %in% event.range]
      alternative.event = proximity.events[Freq == event.freq]
      
      # Keep event according to 50th-quantile criteria
      event.to.keep = quantile(alternative.event$date, p = .5, type = 1)
      event.to.keep = as.Date(event.to.keep)
      
      # Add to main event list
      if (i == 1) {
        main.events = event.to.keep
      } else {
        # Only add if the event is further away from an existing main event than day_threshold
        if (!any(abs(difftime(event.to.keep, main.events, units = "days")) < day_threshold)) {
          main.events = c(main.events, event.to.keep)
        }
      }
    }    
  } else if (type == "precip") {
    uniqueevents = as_datetime(event.frequencies$date)
    
    for (i in 1:length(uniqueevents)) {
      event = event.frequencies[date == uniqueevents[i]]
      
      event.freq = event$Freq
      
      event.range.lower = as_datetime(event$date) - hours(timepoint_threshold)
      event.range.upper = as.Date(event$date) + hours(timepoint_threshold)
      event.range = seq(event.range.lower, event.range.upper, by = 60*60)
      
      proximity.events = event.frequencies[date %in% event.range]
      alternative.event = proximity.events[Freq == event.freq]
      
      event.to.keep = quantile(alternative.event$date, p = .5, type = 1)
      event.to.keep = as_datetime(as.character(event.to.keep))
      
      if (i == 1) {
        main.events = event.to.keep
      } else {
        if (!any(abs(difftime(event.to.keep, main.events, units = "hours")) < timepoint_threshold)) {
          main.events = c(main.events, event.to.keep)
        }
      }
    }
  }
  return(main.events)
  
}

event_matrix = function(main.events,
                        extreme.events,
                        deltaT = 2,
                        type = "discharge") {
  
  # Retrieve all unique locations
  allstations = unique(extreme.events$stat_id)
  
  # Create empty matrix with dimension: location x main events
  mat = matrix(data = NA,
               nrow = length(allstations),
               ncol = length(main.events))
  
  # Set row- and column names
  rownames(mat) = allstations
  colnames(mat) = as.character(main.events)
  
  if (type == "discharge") {
    # Iterate over all main events (columns)
    for (i in 1:length(main.events)) {
      
      # Retrieve main event
      this.event = main.events[i]
      
      # Create a time interval around the event corresponding to deltaT
      event.range.lower = as.Date(this.event) - days(deltaT)
      event.range.upper = as.Date(this.event) + days(deltaT)
      event.range = seq(event.range.lower, event.range.upper, by = 1)
      
      # Iterate over all locations (row)
      for (j in 1:length(allstations)) {
        
        # Retrieve location
        this.station = extreme.events[stat_id == allstations[j]]
        
        # Set matrix entries according to whether the location has an extreme event
        # plus-minus deltaT away from the main event date
        if (any(this.station$date %in% event.range)) {
          mat[j, i] = 1
        } else {
          mat[j, i] = 0
        }
      }
    } 
  } else if (type == "precip") {
    for (i in 1:length(main.events)) {
      this.event = main.events[i]
      
      event.range.lower = as_datetime(this.event) - hours(deltaT)
      event.range.upper = as_datetime(this.event) + hours(deltaT)
      event.range = seq(event.range.lower, event.range.upper, by = 60*60)
      
      for (j in 1:length(allstations)) {
        this.station = extreme.events[stat_id == allstations[j]]
        
        if (any(this.station$date %in% event.range)) {
          mat[j, i] = 1 
        } else {
          mat[j, i] = 0
        }
      }
    }
  }
  return(mat)
}

# ---- SUSCEPTIBILITY INDEX ----

sus_index = function(mat, r.list) {
  n = ncol(mat)
  
  s.ind = c()
  
  for (i in 1:length(r.list)) {
    r = r.list[i]
    
    event.freq = as.vector(colMeans(mat))
    
    x = sum(event.freq >= r)
    
    s = x/(n+1)
    
    s.ind = c(s.ind, s)
  }
  
  return (s.ind)
}




# ---- CLUSTERING ----

require(cluster)

pam.func = function(dta, k) {
  dst = dist(dta, method = "euclidean")
  pam.obj = pam(dst, k, nstart = 10)
  
  return(list(cluster = pam.obj$cluster))
}

cluster_optimum = function(data, K.max = 10, B = 50) {
  
  clusGap.obj = clusGap(data, pam.func, K.max = K.max, B = B)
  
  num_clusters = maxSE(clusGap.obj$Tab[,3], clusGap.obj$Tab[,4], method = "firstSEmax")
  
  return(num_clusters)
}

# Helper function for separating event matrix according to a clustering result
subset_mat_on_cluster = function(km.obj,mat) {
  cluster.df = data.table(stat_id = as.integer(names(km.obj$cluster)),
                          clus = as.integer(km.obj$cluster))
  uniqueclusters = unique(cluster.df$clus)
  
  l = list()
  for (i in 1:length(uniqueclusters)) {
    cluster.num = uniqueclusters[i]
    
    cluster = cluster.df[clus == cluster.num]
    
    mat.subset = mat[rownames(mat) %in% cluster$stat_id,]
    mat.subset = mat.subset[,colSums(mat.subset)>0]
    
    events = as.data.table(colnames(mat.subset))
    setnames(events, "V1", "date")
    events$month = month(events$date)
    
    events |> group_by(month) |> summarise(n.events = n()) -> events.df
    
    events.df$month = month.abb[events.df$month]
    events.df$month = factor(events.df$month,
                             levels = c("Jan","Feb","Mar","Apr","May","Jun",
                                        "Jul","Aug","Sep","Oct","Nov","Dec"))
    
    l = append(l, list(events.df))
  }
  return(l)
}

# Function for plotting the monthly distribution of events per cluster
cluster_events_monthly = function(km.obj, mat, extreme_threshold = "") {
  
  all = as.data.table(colnames(mat))
  setnames(all, "V1", "date")
  all$month = month(all$date)
  
  all = all |> group_by(month) |> summarise(n.events = n())
  
  all$month = month.abb[all$month]
  all$month = factor(all$month,
                     levels = c("Jan","Feb","Mar","Apr","May","Jun",
                                "Jul","Aug","Sep","Oct","Nov","Dec"))
  plot_all = plot_events_monthly(all, title = paste0("Seasonal event distribution at percentile ",extreme_threshold))
  
  plots = list()
  plots = append(plots, list(plot_all))
  
  l = subset_mat_on_cluster(km.obj, mat)
  
  
  
  for (i in 1:length(l)) {
    this.cluster = l[[i]]
    
    plot = plot_events_monthly(this.cluster, title = paste0("Cluster ",i))
    
    plots = append(plots, list(plot))
    
  }
  
  return(plots)
  
}
