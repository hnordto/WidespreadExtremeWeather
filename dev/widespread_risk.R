
library(data.table)
library(tidyverse)

# Create a suitable discharge data set
source("dev/discharge_intervals.R")

# Declare functions needed
source("R/preprocess_discharge.R")
source("R/main_functions.R")
source("R/plot_functions.R")

# ---- Load and preprocess data ----

discharge = load_data()
discharge = discharge$discharge

discharge.east = filter_region(discharge)
discharge.east.long = make_rec_discharge(discharge.east, reshape = T)

discharge.data = subset_discharge(discharge.east,
                                  discharge.east.long,
                                  day_threshold = 364)

# ---- Identify extreme events ----
# Compute extreme thresholds
thresholds = extreme_threshold(discharge.data, probs = .9)

# Identify extreme events
extreme.events = extreme_events(discharge.data, thresholds)


plot_discharge(discharge.data, thresholds = thresholds)

# ---- Identify main events ----

main.events = main_events(extreme.events)

# ---- Create binary event matrix ---

mat = event_matrix(main.events, extreme.events)

# Note to self: Make this process a pipeline??


# ---- Test how different day thresholds affect number of main events ----


day_thresholds = seq(1, 31, by = 1)
deltaTs = seq(1, 10, by = 1)


n.main.events.df = data.table(threshold = integer(),
                              n = integer())
for (i in 1:length(day_thresholds)) {
  threshold = day_thresholds[i]
  main.events = main_events(extreme.events, day_threshold = threshold)
  n.main.events = length(main.events)
  
  n.main.events.df.temp = data.table(threshold = threshold,
                                     n = n.main.events)
  
  n.main.events.df = rbind(n.main.events.df, n.main.events.df.temp)
  
}

ggplot(n.main.events.df, aes(x = threshold, y = n)) +
  geom_col(fill = "dodgerblue", colour = "black") +
  geom_text(aes(label=n), colour = "gray90", position = position_dodge(width=0.9), vjust=2) +
  theme_bw()


# ---- Test different extreme thresholds ----
# Test different thresholds

extreme.thresholds.range = c(0.5, 0.6, 0.7, 0.8, 0.9, 0.925, 0.95, 0.975, 0.99)

store = data.table(mu = numeric(),
                   r = numeric(),
                   n = integer())

# Do not run. Load "store.RData" instead.
if (FALSE) {
  for (i in 1:length(extreme.thresholds.range)) {
    this.threshold = extreme.thresholds.range[i]
    
    extreme.threshold = extreme_threshold(discharge.data, probs = this.threshold)
    
    extreme.events = extreme_events(discharge.data, extreme_thresholds = extreme.threshold)
    
    main.events = main_events(extreme.events)
    
    mat = event_matrix(main.events, extreme.events)
    
    r.to.test = c(0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)
    
    for (j in 1:length(r.to.test)) {
      this.r = r.to.test[j]
      
      event.freq = colMeans(mat) |> as.vector()
      n.over.r = sum(event.freq >= this.r)
      
      store.temp = data.table(mu = this.threshold,
                              r = this.r,
                              n = n.over.r)
      store = rbind(store, store.temp)
      
    }
  } 
}


# Plot n main events per r% locations affected and different extreme thresholds
# Not suitable to put this in a separate function

ggplot(store, aes(x = mu, y = n, group = r)) +
  geom_line(aes(colour = factor(r)), linewidth = 1.2) +
  geom_point(colour = "gray50", size = 1.5) +
  theme_bw() +
  theme(legend.position = "bottom")

# ---- Maps ----

geo = read_spatial_norway("//ad.nr.no/shares/samba_shared/Sommerstudenter/Henrik/Nyttig/Norgeomriss/")

stations_plot = plot_stations(discharge.east, geo)

# ---- susceptibility index ----

# p(%) = 100 (x/(n+1)) 
# n is the total number of events affecting at least one of the stations in the region
# x is the number of events where r% of the stations were affected.

n = ncol(mat)

# Development of helper functions to calculate the number of events where r% of the stations where affected

r = c(0.5)

for (i in 1:length(r)) {
  this.r = r[i]
  
  event.freq = as.vector(colMeans(mat))
  
  x.over.r = sum(event.freq >= this.r)
  
  x = x.over.r
}

s.ind = x/(n+1)
