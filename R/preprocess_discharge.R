require(ggplot2)
require(data.table)
require(tidyr)
require(dplyr)
require(lubridate)

filter_region <- function(nvedat,
                          region = "east") {
  
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
  
}

make_rec_discharge = function(nvedat,
                              bool = TRUE,
                              reshape = FALSE) {
  data = nvedat[, c("stat_id", "qt", "date")]
  
  uniquestations = unique(data$stat_id)
  alldates = data.table("date" = seq(min(data$date), max(data$date), by = 1))
  alldates$date = as.IDate(lubridate::ymd(alldates$date))
  
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
  
  savestation = merge(alldates, savestation, by = c("date"), all.x = TRUE)
  
  savestation = savestation[!duplicated(savestation),]
  
  if (bool == TRUE) {
    savestation_dates = savestation[,1]
    savestation = savestation[,2:ncol(savestation)]
    savestation[is.na(savestation)] <- 0
    savestation[savestation > 0] <- 1
    savestation = cbind(savestation_dates, savestation)
  }
  
  if (reshape == TRUE) {
    if (bool == FALSE) {
      stop("Cannot reshape without boolean transformation")
    } else {
      savestation = reshape_rec_discharge(savestation)
    }
  }
  
  return(savestation)
  
}

reshape_rec_discharge = function(nvedat.rec) {
  data.long = pivot_longer(nvedat.rec, !date, names_to = "stat", values_to = "val")
  data.long$year = year(data.long$date)
  
  return(data.long)
}

plot_rec_discharge = function(nvedat.long,
                               x_break = "2 year",
                               x_lab = "%Y") {

  nvedat.long$val = factor(nvedat.long$val)
  
  p <- ggplot(nvedat.long, aes(x = date, y = stat)) +
    geom_raster(aes(fill = val)) +
    scale_fill_manual(values = c("white", "blue"), labels = c("NO", "YES")) +
    scale_x_date(breaks = x_break, date_labels = x_lab) +
    labs(title = "Discharge measured (YES/NO)") +
    theme_bw() +
    theme(axis.text.y = element_blank(),
          legend.position = "bottom",
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
  
  return(p)
}

subset_discharge <- function(nvedat) {
  stations_to_keep <- c()
  
}