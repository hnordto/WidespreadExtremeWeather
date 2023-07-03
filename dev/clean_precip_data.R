library(readr)
library(data.table)
library(tidyverse)

# ---- CLEAN METADATA ----

prec_meta <- read_delim("//ad.nr.no/shares/samba_shared/Sommerstudenter/Henrik/Data/Precipitation/Hourly/meta_NR_sommerjobb_jun_23.csv", 
                        delim = ";", escape_double = FALSE, col_names = FALSE, 
                        trim_ws = TRUE)
stat_coord = prec_meta[,c("X1","X5","X6")]
setnames(stat_coord, "X1", "stat_id")
setnames(stat_coord, "X5", "mean_utmx")
setnames(stat_coord, "X6", "mean_utmy")
stat_coord = head(stat_coord, -1)


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


