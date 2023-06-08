# VARIOUS FUNCTIONS FOR PRE-PROCESSING AND SUBSETTING NVE DISCHARGE DATA
# See DOCUMENTATION file at the top level

require(ggplot2)
require(data.table)
require(tidyr)
require(dplyr)
require(lubridate)


filter_region <- function(nvedat,
                          region = "east") {
  
  # Filter data based on regine_area
  if (region == "east") {
    ids = c(seq(1 , 16, by = 1), 311)
    data = nvedat[regine_area %in% ids]
  } else if (region == "south") {
    ids = c(seq(17,28, by = 1))
    data = nvedat[regine_area %in% ids]
  } else if (region == "west") {
    ids = c(seq(29, 119, by = 1))
    data = nvedat[regine_area %in% ids]
  } else if (region == "mid") {
    ids = c(seq(120, 189, by = 1), 307, 308)
    data = nvedat[regine_area %in% ids]
  } else if (region == "north") {
    ids = c(seq(190, 247, by = 1))
    data = nvedat[regine_area %in% ids]
  } else {
    stop("Invalid region. Must be east/south/west/mid/north")
  }
  
  return(data)
  
}


make_rec_discharge = function(nvedat,
                              bool = TRUE,
                              reshape = FALSE) {
  
  # Select needed columns
  data = nvedat[, c("stat_id", "qt", "date")]
  
  # Retrieve all unique station IDs
  uniquestations = unique(data$stat_id)
  
  # Create a data table containing all possible dates in the range of the data set
  alldates = data.table("date" = seq(min(data$date), max(data$date), by = 1))
  alldates$date = as.IDate(lubridate::ymd(alldates$date))
  
  # Iterate over all stations and store qt values if recorded
  # This creates a data frame with a column corresponding to each station and rows corresponding to discharge 
  for (j in 1:length(uniquestations)) {
    this_station = data[stat_id == uniquestations[j]]
    setnames(this_station, "qt", paste0("stat_", uniquestations[j]))
    this_station[, stat_id := NULL]
    
    if (j == 1) {
      savestation = this_station
    } else {
      savestation = merge(savestation, this_station, by = "date", 
                          all.x = TRUE, all.y = TRUE)
    }
  }
  
  # Merge the newly created data frame with all possible dates
  # This will create NAs for dates where a station has no records
  savestation = merge(alldates, savestation, by = c("date"), all.x = TRUE)
  
  # Remove ducplicates
  savestation = savestation[!duplicated(savestation),]
  
  # Convert to numeric boolean (0s and 1s) instead of qts and NAs
  if (bool == TRUE) {
    savestation_dates = savestation[,1]
    savestation = savestation[,2:ncol(savestation)]
    savestation[is.na(savestation)] <- 0
    savestation[savestation > 0] <- 1
    savestation = cbind(savestation_dates, savestation)
  }
  
  # Melt to long format and create year variable
  if (reshape == TRUE) {
    if (bool == FALSE) {
      stop("Cannot reshape without boolean transformation")
    } else {
      savestation = reshape_rec_discharge(savestation)
    }
  }
  
  return(as.data.table(savestation))
  
}

reshape_rec_discharge = function(nvedat.rec) {
  # Melt to long format and create year variable
  data.long = pivot_longer(nvedat.rec, !date, names_to = "stat", values_to = "val")
  data.long$year = year(data.long$date)
  
  return(data.long)
}

plot_rec_discharge = function(nvedat.long,
                               x_break = "2 year",
                               x_lab = "%Y") {
  
  # Convert boolean integers to factor before plotting
  nvedat.long$val = factor(nvedat.long$val)
  
  # Plot
  p <- ggplot(nvedat.long, aes(x = date, y = stat)) +
    geom_raster(aes(fill = val)) +
    scale_fill_manual(values = c("lightgray", "blue"), labels = c("NO", "YES")) +
    scale_x_date(breaks = x_break, date_labels = x_lab) +
    labs(title = "Discharge measured (YES/NO)",
         x = "Date",
         y = "Station",
         fill = "Value") +
    theme_bw() +
    theme(legend.position = "bottom",
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
  
  return(p)
}

subset_discharge <- function(nvedat,
                             nvedat.long,
                             min_year = 1985,
                             day_threshold = 350,
                             remove_missing = FALSE) {
  
  # Retrieve all unique stations
  uniquestations = as.vector(unique(nvedat.long[,c("stat")]))
  
  # Calculate the yearly number of measurements on each station
  station.freq = nvedat.long[,.(Freq = sum(val)), by=.(year, stat)]
  station.freq = station.freq[year > min_year]
  
  # Create list to store which stations to keep
  stations.to.keep = c()
  
  # Iterate over all stations
  for (i in 1:length(uniquestations$stat)) {
    keep = TRUE # As long as this is TRUE, the station is kept
    
    this.station = station.freq[stat == uniquestations$stat[i]]
    
    # Iterate over all years
    for (j in 1:nrow(this.station)) {
      this.year = this.station[j,]
      
      # If a station has less measurements than the day threshold a given year
      if (this.year$Freq < day_threshold) {
        keep = FALSE # Set to FALSE, i.e. discard station
      }
    }
    
    # As long as keep remains unchanged, add station to keep-list
    if (keep == TRUE) {
      stations.to.keep = c(stations.to.keep, uniquestations$stat[i])
    }
  }
  
  # Retrieve station IDs on the same format as nvedat
  station.indexer = gsub("stat_", "", stations.to.keep)
  
  # Subset the desired year and stations to keep
  data = nvedat[year > min_year]
  data = data[stat_id %in% station.indexer]
  
  # Merge with all possible dates in interval
  alldates = data.table("date"=seq(min(data$date), max(data$date), by = 1))
  data = merge(alldates, data, by = c("date"), all.x = TRUE)
  
  # Remove ducplicates
  data = data[!duplicated(data),]
  
  return(data)
}
