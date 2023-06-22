wd="//ad.nr.no/shares/samba_shared/Sommerstudenter/Henrik/Data/Precipitation"
setwd(wd)

library(readr)
library(data.table)
library(tidyverse)

prec_meta <- read_delim("//ad.nr.no/shares/samba_shared/Sommerstudenter/Henrik/Data/Precipitation/Hourly/meta_NR_sommerjobb_jun_23.csv", 
                        delim = ";", escape_double = FALSE, col_names = FALSE, 
                        trim_ws = TRUE)

prec = read_delim("//ad.nr.no/shares/samba_shared/Sommerstudenter/Henrik/Data/Precipitation/Hourly/rr_1_kdvh_sommerjobb_NR_01012010_31122022_v01.csv", 
                  delim = ";", escape_double = FALSE, col_names = FALSE, 
                  locale = locale(decimal_mark = ",", grouping_mark = ""), 
                  trim_ws = TRUE, skip = 3)
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

library(scales)

ggplot(data.long, aes(x = date, y = stat)) +
  geom_raster(aes(fill = factor(val))) +
  scale_fill_manual(values = c("lightgray", "blue"), labels = c("NO", "YES"))
