
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
thresholds = extreme_threshold(discharge.data, probs = .7)

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
  labs(title = "Number of Identified Main Events",
       subtitle = "For different main event separation intervals",
       y = "Number of Main Events",
       x = "Main Event Separation Value (days)") +
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
  labs(title = "Number of Main Events",
       subtitle = "By extreme event threshold and where r% of the locations where affected",
    x = "Extreme event threshold (quantile probability)",
    y = "Number of main events",
    colour = "r% of stations affected") +
  theme_bw() +
  theme(legend.position = "bottom")

# ---- Maps ----

geo = read_spatial_norway("//ad.nr.no/shares/samba_shared/Sommerstudenter/Henrik/Nyttig/Norgeomriss/")

stations_plot = plot_stations(discharge.data, geo, station_ids = T)

# ---- Fourieranalyse ----
# discharge::fourierAnalysis

discharge.sub = discharge.data[stat_id == 200011]


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


discharge.sub = discharge.data[stat_id == 200011]
discharge.sub.qt = discharge.sub$qt

# ---- ACF ----

stations = c(200303, 1600051, 300022, 12000137)

acf1 = acf(discharge.data[stat_id == 200303, qt])
acf2 = acf(discharge.data[stat_id == 1600051, qt])
acf3 = acf(discharge.data[stat_id == 300022, qt])
acf4 = acf(discharge.data[stat_id == 1200137, qt])

par(mfrow = c(2, 2))
plot(acf1)
plot(acf2)
plot(acf3)
plot(acf4)

all.stations = unique(discharge.data$stat_id)
acf.lag7 = c()

for (i in 1:length(all.stations)) {
  this.station = all.stations[i]
  discharge.sub = discharge.data[stat_id == this.station]
  
  acf = acf(discharge.sub[,qt])
  
  acf.lag7 = c(acf.lag7, acf$acf[7])
}

# ---- Clustering ----

# PCA: Does this really make sense when applied to the binary matrix? 

mat |> as.data.frame() |> rownames_to_column("stat_id") -> mat.df
stat_metadata = discharge.data[, head(.SD, 1), by = stat_id]

pca.obj = prcomp(mat)
score.tbl = as.data.frame(pca.obj$x) |> rownames_to_column("stat_id")
score.tbl = merge(score.tbl, stat_metadata, by = "stat_id")

ggplot(score.tbl) +
  geom_point(aes(x = PC1, y = PC2, colour = stat_id))

# k-means

library(cluster)

pam.func = function(dta, k) {
  dst = dist(dta, method = "euclidean")
  pam.obj = pam(dst, k, nstart = 10)
  return(list(cluster = pam.obj$cluster))
}             

gs.obj = clusGap(mat, pam.func, K.max = 10, B = 50)

maxSE(gs.obj$Tab[,3],gs.obj$Tab[,4], method = "firstSEmax")

km.obj = kmeans(mat, centers = 3, nstart = 10)

clusters = data.table(stat_id = as.integer(names(km.obj$cluster)),
                      clus = as.integer(km.obj$cluster))

clusters = merge(clusters, stat_metadata, by = "stat_id")
coords.lonlat = LongLatToUTM(clusters$mean_utmx, clusters$mean_utmy, 33)
cluster.coords = cbind(clusters, coords.lonlat)

ggplot() +
  geom_polygon(aes(x = long, y = lat, group = id), data = geo,
               fill = "grey90", colour = "grey40") +
  geom_point(aes(x = X, y = Y, colour = factor(clus)), data = cluster.coords) +
  theme_bw()
