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

plot_stations = function(data,
                         norway.lonlat,
                         station_ids = FALSE,
                         zoom = TRUE) {
  
  
  utm.x = data$mean_utmx
  utm.y = data$mean_utmy
  
  coords.lonlat = UTMToLongLat(utm.x, utm.y, 33)
  
  data.converted = cbind(data, coords.lonlat)
  data.converted.sub = data.converted[, head(.SD, 1), by = stat_id]
  
  if (zoom == FALSE) {
    if (station_ids == TRUE) {
      p = ggplot() +
        geom_polygon(aes(x = long, y = lat, group = id), data = norway.lonlat,
                     fill = "grey90", colour = "grey40") +
        geom_text(aes(x = X, y = Y, label = stat_id, colour = reguleringsgrad_magasin), 
                  data = data.converted.sub) +
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
    } else {
      p = ggplot() +
        geom_polygon(aes(x = long, y = lat, group = id), data = norway.lonlat,
                     fill = "grey90", colour = "grey40") +
        geom_point(aes(x = X, y = Y, size = area_total, 
                       colour = reguleringsgrad_magasin),
                   data = data.converted.sub) +
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
    }
  } else if (zoom == TRUE) {

    min.X = min(data.converted.sub$X) - 0.1
    max.X = max(data.converted.sub$X) + 0.1
    min.Y = min(data.converted.sub$Y) - 0.1
    max.Y = max(data.converted.sub$Y) + 0.1
    
    p = ggplot() + 
      coord_sf(xlim = c(min.X, max.X), ylim = c(min.Y, max.Y)) +
      geom_polygon(aes(x = long, y = lat, group = id), data = norway.lonlat,
                   fill = "grey90", colour = "grey40") +
      geom_point(aes(x = X, y = Y), data = data.converted.sub, colour = "dodgerblue", size = 1.5) +
      labs(title = "Overview of Weather Stations Included in Data Set",
           subtitle = "Precipitation Data",
           x = element_blank(),
           y = element_blank()) +
      theme_bw() +
      theme(axis.text = element_blank(),
            axis.ticks = element_blank(),
            panel.grid = element_blank(),
            legend.position = "right")
  }

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

plot_precipitation = function(metdat,
                              stations = c(3720, 4260, 27015, 18180)) {
  d = metdat[stat_id %in% stations]
  
  p = ggplot(d, aes(x = date, y = qt, colour = factor(stat_id))) +
    geom_point() +
    facet_wrap(~factor(stat_id)) +
    labs(title = paste0("Recorded discharge ",
                        min(year(d$date)),
                        "-",
                        max(year(d$date))),
         x = "Timepoint",
         y = "Precipitation (mm)",
         colour = "Station ID") +
    theme_bw() +
    theme(legend.position = "bottom")
  
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


plot_clusters = function(km.obj,
                         data,
                         norway.lonlat,
                         zoom = FALSE) {
  
  stat.metadata = data[, head(.SD, 1), by = stat_id]
  
  clusters.df = data.table(stat_id = as.integer(names(km.obj$cluster)),
                           clus = as.integer(km.obj$cluster))
  
  clusters.stat = merge(clusters.df, stat.metadata, by = "stat_id")
  
  coords.lonlat = UTMToLongLat(clusters.stat$mean_utmx,
                               clusters.stat$mean_utmy, 33)
  
  clusters.stat = cbind(clusters.stat, coords.lonlat)
  
  if (zoom == TRUE) {
    min.X = min(clusters.stat$X) - 0.1
    max.X = max(clusters.stat$X) + 0.1
    min.Y = min(clusters.stat$Y) - 0.1
    max.Y = max(clusters.stat$Y) + 0.1
    
    p = ggplot() +
      coord_sf(xlim = c(min.X, max.X), ylim = c(min.Y, max.Y)) +
      geom_polygon(aes(x = long, y = lat, group = id), data = norway.lonlat,
                   fill = "grey90", colour = "grey40") +
      geom_point(aes(x = X, y = Y, colour = factor(clus)), data = clusters.stat, size = 2) +
      labs(title = "Geographical distribution of cluster members",
           y = element_blank(),
           x = element_blank(),
           colour = "Group") +
      theme_bw() +
      theme(axis.text = element_blank(),
            axis.ticks = element_blank(),
            panel.grid = element_blank(),
            legend.position = "bottom")
  } else {
    p = ggplot() +
      geom_polygon(aes(x = long, y = lat, group = id), data = norway.lonlat,
                   fill = "grey90", colour = "grey40") +
      geom_point(aes(x = X, y = Y, colour = factor(clus)), data = clusters.stat, size = 2) +
      labs(title = "Geographical distribution of cluster members",
           y = element_blank(),
           x = element_blank(),
           colour = "Group") +
      #scale_color_brewer(palette = "Dark2") +
      theme_bw() +
      theme(axis.text = element_blank(),
            axis.ticks = element_blank(),
            panel.grid = element_blank(),
            legend.position = "bottom")
  }
  
  return(p)
}

# Helper function for plotting the monthly number of main events per cluster
plot_events_monthly = function(data, title = "") {
  p = ggplot(data, aes(x = month, y = n.events)) +
    geom_bar(stat = "identity", fill = "steelblue", colour = "black") +
    scale_x_discrete(breaks = c("Jan","Feb","Mar","Apr","May","Jun",
                                "Jul","Aug","Sep","Oct","Nov","Dec")) +
    coord_polar(start = pi/12) +
    labs(title = title) +
    theme_bw() +
    theme(axis.title = element_blank(),
          panel.ontop = TRUE, # change to FALSE for grid lines below the wind rose
          panel.background = element_blank())
  
  return(p)
}

plot_widespread_hazard = function(hazard.estimation) {
  
  mats.labs = unique(hazard.estimation$mu)
  
  p = ggplot(hazard.estimation, aes(group = r)) +
    geom_line(aes(x = mu, y = n, colour = factor(r)), linewidth = 1.2) +
    geom_line(aes(x = mu, y = n.tot), colour = "black", linewidth = 1.2, linetype = 2) +
    geom_point(aes(x = mu, y = n), colour = "gray50", size = 1.5) +
    geom_point(aes(x = mu, y = n.tot),colour = "gray30", size = 1.7) +
    labs(title = "Number of Main Events",
         subtitle = "By extreme event threshold and where r% of the locations where affected",
         x = "Extreme event threshold (quantile probability)",
         y = "Number of main events",
         colour = "r% of locations affected") +
    scale_x_continuous(breaks = mats.labs) +
    theme_bw() +
    theme(legend.position = "bottom")
  
  return(p)
}
