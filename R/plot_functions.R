require(ggplot2)

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
