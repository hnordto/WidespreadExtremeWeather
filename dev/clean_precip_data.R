proj_wd = getwd()
wd="//ad.nr.no/shares/samba_shared/Sommerstudenter/Henrik/Data/Precipitation"
setwd(wd)

library(readr)
library(data.table)
library(tidyverse)

prec_meta <- read_delim("//ad.nr.no/shares/samba_shared/Sommerstudenter/Henrik/Data/Precipitation/Hourly/meta_NR_sommerjobb_jun_23.csv", 
                        delim = ";", escape_double = FALSE, col_names = FALSE, 
                        trim_ws = TRUE)
stat_coord = prec_meta[,c("X1","X5","X6")]
setnames(stat_coord, "X1", "stat_id")
setnames(stat_coord, "X5", "mean_utmx")
setnames(stat_coord, "X6", "mean_utmy")
stat_coord = head(stat_coord, -1)



prec = read_delim("//ad.nr.no/shares/samba_shared/Sommerstudenter/Henrik/Data/Precipitation/Hourly/rr_1_kdvh_sommerjobb_NR_01012010_31122022_v01.csv", 
                  delim = ";", escape_double = FALSE, col_names = FALSE, 
                  locale = locale(decimal_mark = ",", grouping_mark = ""), 
                  trim_ws = TRUE, skip = 3)

setwd(proj_wd)

prec = prec[rowSums(is.na(prec)) != ncol(prec),]

prec = data.table("date" = as_datetime(prec$X2),
                  qt = prec$X3,
                  stat_id = prec$X1)

prec$month = month(prec$date)

summer.months = seq(5, 9, by = 1)

prec = prec[month %in% summer.months]

prec[,month := NULL]

prec$year = year(prec$date)

prec = prec[year > 2015]

prec[, year := NULL]

# Remove SV stations

stations.to.remove = c(3730, 3990, 4455, 4543, 4725, 4825, 4827, 17400, 
                       17550, 17640, 17650, 17820, 17875, 17895, 18240,
                       18265, 18280, 18410, 18690, 19430, 19815, 19830,
                       19923, 26970)

test = prec[!(stat_id %in% stations.to.remove)]

# Handle negative values

prec$qt = ifelse(prec$qt < 0, 0, prec$qt)

# Gap sizes

uniquestations = unique(prec$stat_id)
allhours = data.table("date"=seq(min(prec$date),max(prec$date),by=60*60))
allhours$month = month(allhours$date)
allhours = allhours[month %in% summer.months]
allhours[, month := NULL]

for (j in 1:length(uniquestations)) {
  this_station = prec[stat_id == uniquestations[j]]
  setnames(this_station, "qt", paste0("stat_", uniquestations[j]))
  this_station[, stat_id := NULL]
  
  if (j == 1) {
    savestation = this_station
  } else {
    savestation = merge(savestation, this_station, by = "date", 
                        all.x = TRUE, all.y = TRUE)
  }
}

savestation = merge(allhours, savestation, by = c("date"), all.x = TRUE)

savestation = savestation[!duplicated(savestation),]

savestation_dates = savestation[,1]
savestation = savestation[,2:ncol(savestation)]
savestation[savestation >= 0] <- 1
savestation[is.na(savestation)] <- 0
savestation = cbind(savestation_dates, savestation)

data.long = pivot_longer(savestation, !date, names_to = "stat", values_to = "val")
data.long$year = year(data.long$date)
data.long$month = month(data.long$date)
data.long$day = day(data.long$date)

library(scales)

ggplot(data.long, aes(x = date, y = stat)) +
  geom_raster(aes(fill = factor(val))) +
  labs(title = "Precipitation measured /YES/NO",
       x = "Date",
       y = "Station",
       fill = "Value") +
  scale_fill_manual(values = c("lightgray", "blue"), labels = c("NO", "YES")) +
  theme_bw() +
  theme(legend.position = "bottom")



data.long = as.data.table(data.long)

# Subsetting

uniquestations = as.vector(unique(data.long[,c("stat")]))

station.freq = data.long[,.(Freq = sum(val)), by=.(year, stat)]

stations.to.keep = c()

for (i in 1:length(uniquestations$stat)) {
  keep = TRUE
  
  this.station = station.freq[stat == uniquestations$stat[i]]
  
  for (j in 1:nrow(this.station)) {
    this.year = this.station[j,]
    
    if (this.year$Freq == 0) {
      keep = FALSE
    }
  }
  
  print(keep)
  
  if (keep == TRUE)
    stations.to.keep = c(stations.to.keep, uniquestations$stat[i])
}

data.long.2 = data.long[stat %in% stations.to.keep]

ggplot(data.long.2, aes(x = date, y = stat)) +
  geom_raster(aes(fill = factor(val))) +
  labs(title = "Precipitation measured /YES/NO",
       x = "Date",
       y = "Station",
       fill = "Value") +
  scale_fill_manual(values = c("lightgray", "blue"), labels = c("NO", "YES")) +
  theme_bw() +
  theme(legend.position = "bottom")

uniquestations = as.vector(unique(data.long.2[,c("stat")]))
uniquemeasurements = as.vector(unique(data.long.2[,c("date")]))

for (i in 1:length(uniquemeasurements$date)) {
  this.timepoint = uniquemeasurements$date[i]
  
  timepoint.data = data.long.2[date == this.timepoint]
  
  flagged.stat = timepoint.data[val == 0]
  
  
}

flag.stat.full = c()
flag.date.full = c()
for (i in 1:length(uniquestations$stat)) {
  flag.stat = c()
  flag.date = c()
  
  this.station = uniquestations$stat[i]
  
  data.sub = data.long.2[stat == this.station]
  
  x = data.sub$val
  
  res = rle(x == 0)
  gap_sizes = rep(res$values*res$lengths,res$lengths)
  
  data.sub$gap_size = gap_sizes
  
  uniquemeasurements = as.vector(unique(data.sub[,c("date")]))
  
  if (any(data.sub$gap_size > 744)) {
    flag.stat = c(flag.stat, this.station)
  } else if (any(data.sub$gap_size > 48)) {
    
    dates.to.flag = as.character(data.sub[gap_size > 48]$date)
    
    flag.date = c(flag.date, dates.to.flag)
  }
  
  flag.stat.full = c(flag.stat.full, flag.stat)
  flag.date.full = c(flag.date.full, flag.date)
  
}

new.dat = data.long.2[!(stat) %in% flag.stat.full]
new.dat = new.dat[!(date %in% flag.date.full)]

station.indexer = gsub("stat_", "", flag.stat.full)

prec2 = merge(allhours, prec, by = c("date"), all.x = T)
prec2 = prec2[!(stat_id %in% station.indexer)]
prec2 = prec2[!(date %in% flag.date.full)]
prec2[is.na(prec2)] = 0

ggplot(new.dat, aes(x = date, y = stat)) +
  geom_raster(aes(fill = factor(val))) +
  labs(title = "Precipitation measured /YES/NO",
       x = "Date",
       y = "Station",
       fill = "Value") +
  scale_fill_manual(values = c("lightgray", "blue"), labels = c("NO", "YES")) +
  theme_bw() +
  theme(legend.position = "bottom")


# Widespread risk

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


