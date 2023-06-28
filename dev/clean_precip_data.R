library(readr)
library(data.table)
library(tidyverse)

# ---- CLEAN METADATA ----

#prec_meta <- read_delim("//ad.nr.no/shares/samba_shared/Sommerstudenter/Henrik/Data/Precipitation/Hourly/meta_NR_sommerjobb_jun_23.csv", 
#                        delim = ";", escape_double = FALSE, col_names = FALSE, 
#                        trim_ws = TRUE)
#stat_coord = prec_meta[,c("X1","X5","X6")]
#setnames(stat_coord, "X1", "stat_id")
#setnames(stat_coord, "X5", "mean_utmx")
#setnames(stat_coord, "X6", "mean_utmy")
#stat_coord = head(stat_coord, -1)


# ---- CLEAN PRECIPITATION DATA ----
prec = read_delim("//ad.nr.no/shares/samba_shared/Sommerstudenter/Henrik/Data/Precipitation/Hourly/rr_1_kdvh_sommerjobb_NR_01012010_31122022_v01.csv", 
                  delim = ";", escape_double = FALSE, col_names = FALSE, 
                  locale = locale(decimal_mark = ",", grouping_mark = ""), 
                  trim_ws = TRUE, skip = 3)


# Remove rows where all values are NA
# Such rows are caused by multiple header-rows in the original csv file
prec = prec[rowSums(is.na(prec)) != ncol(prec),]

# Convert to data frame and set variable names
prec = data.table("date" = as_datetime(prec$X2),
                  qt = prec$X3,
                  stat_id = prec$X1)

# Create column for month
prec$month = month(prec$date)

# Define summer months
summer.months = seq(5, 9, by = 1)

# Subset summer months
prec = prec[month %in% summer.months]

# Remove month columns
prec[,month := NULL]

# Create column for year
prec$year = year(prec$date)

# Subset years > 2015
prec = prec[year > 2015]

# Remove column for year
prec[, year := NULL]

# Handle negative values: Set to 0

prec$qt = ifelse(prec$qt < 0, 0, prec$qt)

if (FALSE) {
  save(prec, file = "data/prec_clean.RData")
}






# ---- DEVELOPMENT OF FUNCTIONS FOR WIDESPREAD RISK ---

extreme.thresholds = extreme_threshold(prec2, probs = .99)

extreme.events = extreme_events(prec2, extreme.thresholds)

main_events_prec = function(extreme.events,
                            timepoint_threshold = 48) {
  event.frequencies = extreme.events[,.(Freq = .N), by = date]
  setorder(event.frequencies, cols = - Freq)
  
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
  
  return(main.events)
}

main.events = main_events_prec(extreme.events)

event_matrix_prec = function(main.events,
                            extreme.events,
                            deltaT = 12) {
  allstations = unique(extreme.events$stat_id)
  
  mat = matrix(data = NA,
               nrow = length(allstations),
               ncol = length(main.events))
  
  rownames(mat) = allstations
  colnames(mat) = as.character(main.events)
  
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
  
  return(mat)
}

mat = event_matrix_prec(main.events, extreme.events)

# Plot stations

plot_stations_prec = function(data,
                              norway.lonlat) {
  utm.x = data$mean_utmx
  utm.y = data$mean_utmy
  
  coords.lonlat = UTMToLongLat(utm.x, utm.y, 33)
  
  data.converted = cbind(data, coords.lonlat)
  data.converted.sub = data.converted[,head(.SD, 1), by = stat_id]
  
  p = ggplot() + 
    geom_polygon(aes(x = long, y = lat, group = id), data = norway.lonlat,
                 fill = "grey90", colour = "grey40") +
    geom_point(aes(x = X, y = Y), data = data.converted.sub) +
    theme_bw() +
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank(),
          legend.position = "right")
  
  return(p)
}


