load("data/mat.RData")

colnames(mat) -> test
test = as.data.table(test)
setnames(test, "test", "date")

test$day = day(test$date)
test$month = month(test$date)
test$year = year(test$date)

test |> group_by(month) |> summarise(n.events = n()) -> test2

test2 = test2 |> arrange(month)

test2$month = month.abb[test2$month]
test2$month = factor(test2$month,
                        levels = c("Jan","Feb","Mar","Apr","May","Jun",
                                   "Jul","Aug","Sep","Oct","Nov","Dec"))

n <- 37
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))

rose.diag(test2)

ggplot(test2, aes(x = month, y = n.events)) +
  geom_bar(stat = "identity", fill = "steelblue", colour = "black") +
  coord_polar(start = pi/12) +
  theme_bw() +
  theme(axis.title = element_blank(),
        panel.ontop = TRUE, # change to FALSE for grid lines below the wind rose
        panel.background = element_blank())

# ---- CREATE PLOTS ----

discharge = load_data()
discharge = discharge$discharge

discharge.east = filter_region(discharge)
discharge.east.long = make_rec_discharge(discharge.east, reshape = T)

discharge.data = subset_discharge(discharge.east,
                                  discharge.east.long,
                                  day_threshold = 364)
quantiles = c(0.9, 0.95, 0.975, 0.99)

for (i in 1:length(quantiles)) {
  this.quantile = quantiles[i]
  
  thresholds = extreme_threshold(discharge.data, probs = this.quantile)
  
  extreme.events = extreme_events(discharge.data, thresholds)
  
  main.events = main_events(extreme.events)
  
  mat = event_matrix(main.events, extreme.events)
  
  mat.name = paste0("mat",this.quantile)
  
  assign(mat.name, mat)
}

n.clusters.090 = cluster_optimum(mat0.9)
n.clusters.095 = cluster_optimum(mat0.95)
n.clusters.0975 = cluster_optimum(mat0.975)
n.clusters.099 = cluster_optimum(mat0.99)

km.obj.09 = kmeans(mat, n.clusters.090)
km.obj.095 = kmeans(mat, n.clusters.095)
km.obj.0975 = kmeans(mat, n.clusters.0975)
km.obj.099 = kmeans(mat, n.clusters.099)

create_diagrams = function(km.obj,mat) {
  cluster.df = data.table(stat_id = as.integer(names(km.obj$cluster)),
                          clus = as.integer(km.obj$cluster))
  uniqueclusters = unique(cluster.df$clus)
  
  l = list()
  for (i in 1:length(uniqueclusters)) {
    cluster.num = uniqueclusters[i]
    
    cluster = cluster.df[clus == cluster.num]
    
    mat.subset = mat[rownames(mat) %in% cluster$stat_id,]
    
    events = as.data.table(colnames(mat.subset))
    setnames(events, "V1", "date")
    events$month = month(events$date)
    
    events |> group_by(month) |> summarise(n.events = n()) -> events.df
    
    events.df$month = month.abb[events.df$month]
    events.df$month = factor(events.df$month,
                             levels = c("Jan","Feb","Mar","Apr","May","Jun",
                                        "Jul","Aug","Sep","Oct","Nov","Dec"))
    
    l = append(l, list(events.df))
  }
  return(l)
}

# For threshold = 0.9

cluster.df = data.table(stat_id = as.integer(names(km.obj.09$cluster)),
                        clus = as.integer(km.obj.09$cluster))
cluster1 = cluster.df[clus == 1]

mat1 = mat[rownames(mat) %in% cluster1$stat_id,]

events = colnames(mat1)
events = as.data.table(events)

test = as.data.table(test)
setnames(events, "events", "date")

events$month = month(events$date)
events$year = year(events$date)

events |> 
  group_by(month) |> 
  summarise(n.events = n()) -> events.df

events.df$month = month.abb[events.df$month]
events.df$month = factor(events.df$month,
                     levels = c("Jan","Feb","Mar","Apr","May","Jun",
                                "Jul","Aug","Sep","Oct","Nov","Dec"))


n <- 37
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))



ggplot(events.df, aes(x = month, y = n.events)) +
  geom_bar(stat = "identity", fill = "steelblue", colour = "black") +
  coord_polar(start = pi/12) +
  theme_bw() +
  theme(axis.title = element_blank(),
        panel.ontop = TRUE, # change to FALSE for grid lines below the wind rose
        panel.background = element_blank())
