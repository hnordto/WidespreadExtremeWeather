require(ggplot2)
require(rgdal)
require(sp)

# zone = 33
UTMToLongLat <- function(x,y,zone){
  
  xy <- data.frame(ID = 1:length(x), X = x, Y = y)
  
  coordinates(xy) <- c("X", "Y")
  
  proj4string(xy) <- CRS(paste("+proj=utm +zone=",zone," ellps=WGS84",sep='')) ## for example
  
  res <- spTransform(xy, CRS("+proj=longlat +datum=WGS84"))
  
  return(as.data.frame(res))
}

# Helper function for reading spatial data for Norway
read_spatial_norway = function(path) {
  current_wd = getwd()
  setwd(path)
  
  norge = readOGR(dsn = ".", layer = "norge")
  projection = proj4string(norge)
  
  norway.lonlat = spTransform(norge, CRS("+proj=longlat +datum=WGS84"))
  
  setwd(current_wd)
  
  return(norway.lonlat)
}

plot_stations = function(nvedat,
                         norway.lonlat) {
  utm.x = nvedat$mean_utmx
  utm.y = nvedat$mean_utmy
  
  coords.lonlat = LongLatToUTM(utm.x, utm.y, 33)
  
  nvedat.converted = cbind(nvedat, coords.lonlat)
  nvedat.converted.sub = nvedat.converted[, head(.SD, 1), by = stat_id]
  
  p = ggplot() +
    geom_polygon(aes(x = long, y = lat, group = id), data = norway.lonlat,
                 fill = "grey90", colour = "grey40") +
    geom_point(aes(x = X, y = Y, size = area_total, 
                   colour = reguleringsgrad_magasin),
               data = nvedat.converted.sub) +
    labs(title = "Overview of Catchments Included in Data Set",
         subtitle = "Discharge Data",
         x = element_blank(),
         y = element_blank(),
         colour = "Degree of Regulation",
         size = "Total Area") +
    viridis::scale_color_viridis(option = "turbo") +
    theme_bw() +
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank(),
          legend.position = "right")
  
  return(p)
}


plot_discharge = function(nvedat,
                          stations = c(200025, 200145, 200604, 1200097),
                          thresholds = NULL) {
  
  # Subset desired stations
  d = nvedat[stat_id %in% stations]
  
  # If extreme thresholds are not inputted
  if (is.null(thresholds)) {
    p = ggplot(d, aes(x = date, y = qt, colour = factor(stat_id))) +
      geom_line() +
      facet_wrap(~factor(stat_id)) +
      labs(title = paste0("Recorded discharge ", 
                          min(year(d$date)), 
                          "-", 
                          max(year(d$date))),
           x = "Date",
           y = "Discharge (m^3/s)",
           colour = "Station ID") +
      theme_bw() +
      theme(legend.position = "bottom")
    
  # If extreme thresholds are inputted
  } else {
    
    # Map thresholds to their respective stations
    d = merge(d, thresholds, by = "stat_id", all.x = TRUE)
    
    p = ggplot(d, aes(x = date, y = qt, colour = factor(stat_id))) +
      geom_line() +
      facet_wrap(~factor(stat_id)) +
      geom_line(aes(y = us), colour = "black", linetype = "dashed") +
      labs(title = paste0("Recorded discharge ", 
                          min(year(d$date)), 
                          "-", 
                          max(year(d$date))),
           x = "Date",
           y = "Discharge (m^3/s)",
           colour = "Station ID") +
      theme_bw() +
      theme(legend.position = "bottom")
  }
  
  return(p)
}


plot_event_matrix = function(mat) {
  
  # Convert matrix to data frame
  mat.df = as.data.frame(mat)
  
  # Create column with station IDs from rownames
  mat.df = rownames_to_column(mat.df, "stat_id")
  
  # Melt data frame to a more suitable format for plotting
  mat.df = pivot_longer(mat.df, -stat_id, names_to = "date", values_to = "val")
  
  # Plot
  p = ggplot(mat.df, aes(x = date, y = stat_id)) +
    geom_raster(aes(fill = factor(val))) +
    scale_fill_manual(values = c("gray90", "dodgerblue"), labels = c("NO", "YES")) +
    labs(title = "Extreme event (YES/NO)",
         subtitle = paste0("Per station and main event in time period ", 
                           min(year(mat.df$date)),
                           "-",
                           max(year(mat.df$date))),
         x = "Timepoint of main event",
         y = "Station",
         fill = "Extreme event") +
    theme_bw() +
    theme(legend.position = "bottom",
          axis.text.x = element_blank())
  
  return(p)
  
}
