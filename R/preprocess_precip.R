
make_rec_precip = function(metdat,
                           bool = TRUE,
                           reshape = FALSE) {
  data = metdat[, c("stat_id", "qt", "date")]
  
  uniquestations = unique(data$stat_id)
  
  allhours = data.table("date" = seq(min(data$date), max(data$date), by = 60*60))
  
  min.month = min(month(data$date))
  max.month = max(month(data$date))
  months.in.data = seq(min.month, max.month, by = 1)
  
  allhours$month = month(allhours$date)
  allhours = allhours[month %in% months.in.data]
  allhours[, month := NULL]
  
  for (j in 1:length(uniquestations)) {
    this.station = data[stat_id == uniquestations[j]]
    setnames(this.station, "qt", paste0("stat_", uniquestations[j]))
    this.station[, stat_id := NULL]
    
    if (j == 1) {
      savestation = this.station
    } else {
      savestation = merge(savestation, this.station, by = "date",
                          all.x = TRUE, all.y = TRUE)
    }
  }
  
  savestation = merge(allhours, savestation, by = c("date"), all.x = TRUE)
  
  savestation = savestation[!duplicated(savestation),]
  
  if (bool == TRUE) {
    savestation_dates = savestation[,1]
    savestation = savestation[,2:ncol(savestation)]
    savestation[savestation >= 0] <- 1
    savestation[is.na(savestation)] <- 0
    savestation = cbind(savestation_dates, savestation)
  }
  
  if (reshape == TRUE) {
    if (bool == FALSE) {
      stop("Cannot reshape without boolean transformation")
    } else {
      savestation = reshape_rec_precip(savestation)
    }
  }
  
  return(as.data.table(savestation))
  
}

reshape_rec_precip = function(metdat.rec) {
  data.long = pivot_longer(metdat.rec, !date, names_to = "stat", values_to = "val")
  data.long$year = year(data.long$date)
  data.long$month = month(data.long$date)
  data.long$day = day(data.long$date)
  
  return(data.long)
}

plot_rec_precip = function(metdat.long) {
  
  metdat.long$val = factor(metdat.long$val)
  
  p = ggplot(metdat.long, aes(x = date, y = stat)) +
    geom_raster(aes(fill = val)) +
    scale_fill_manual(values = c("lightgray", "blue"), labels = c("NO", "YES")) +
    labs(title = "Precipitation measured (YES/NO)",
         x = "Timepoint",
         y = "station",
         fill = "Value") +
    theme_bw() +
    theme(legend.position = "bottom",
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
  
  return(p)
}


subset_precip = function(metdat,
                         metdat.long,
                         remove_stat_on_gap = 744,
                         remove_date_on_gap = 48) {
  
  uniquestations = as.vector(unique(metdat.long[,c("stat")]))
  
  flag.stat.full = c()
  flag.date.full = c()
  
  for (i in 1:length(uniquestations$stat)) {
    flag.stat = c()
    flag.date = c()
    
    this.station = uniquestations$stat[i]
    
    data.sub = metdat.long[stat == this.station]
    
    x = data.sub$val
    
    res = rle(x == 0)
    gap_sizes = rep(res$values*res$lengths,res$lengths)
    
    data.sub$gap_size = gap_sizes
    
    uniquemeasurements = as.vector(unique(data.sub[, c("date")]))
    
    if (any(data.sub$gap_size > remove_stat_on_gap)) {
      flag.stat = c(flag.stat, this.station)
    } else if (any(data.sub$gap_size > remove_date_on_gap)) {
      dates.to.flag = as_datetime(data.sub[gap_size > remove_date_on_gap]$date)
      
      flag.date = c(flag.date, dates.to.flag)
    }
    
    flag.stat.full= c(flag.stat.full, flag.stat)
    flag.date.full = c(flag.date.full, flag.date)
    
  }
  
  station.indexer = gsub("stat_", "", flag.stat.full)
  
  data = metdat[!(stat_id %in% station.indexer)]
  data = data[!(date %in% flag.date.full)]
  data[is.na(data)] <- 0
  
  return(data)
}





































