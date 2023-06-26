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
                                  min_year = 1986,
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

create_diagrams = function(km.obj,mat) {
  cluster.df = data.table(stat_id = as.integer(names(km.obj$cluster)),
                          clus = as.integer(km.obj$cluster))
  uniqueclusters = unique(cluster.df$clus)
  
  l = list()
  for (i in 1:length(uniqueclusters)) {
    cluster.num = uniqueclusters[i]
    
    cluster = cluster.df[clus == cluster.num]
    
    mat.subset = mat[rownames(mat) %in% cluster$stat_id,]
    mat.subset = mat.subset[,colSums(mat.subset)>0]
    
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

plot_diagram = function(data, title = "") {
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

cluster_events_monthly = function(km.obj, mat, extreme_threshold = "") {
  
  all = as.data.table(colnames(mat))
  setnames(all, "V1", "date")
  all$month = month(all$date)
  
  all = all |> group_by(month) |> summarise(n.events = n())
  
  all$month = month.abb[all$month]
  all$month = factor(all$month,
                     levels = c("Jan","Feb","Mar","Apr","May","Jun",
                                "Jul","Aug","Sep","Oct","Nov","Dec"))
  plot_all = plot_diagram(all, title = paste0("Seasonal event distribution at percentile ",extreme_threshold))
  
  plots = list()
  plots = append(plots, list(plot_all))
  
  l = create_diagrams(km.obj, mat)
  
  
  
  for (i in 1:length(l)) {
    this.cluster = l[[i]]
    
    plot = plot_diagram(this.cluster, title = paste0("Cluster ",i))
    
    plots = append(plots, list(plot))
    
  }
  
  return(plots)
  
}

# 0.9
n.clusters.090 = cluster_optimum(mat0.9)
n.clusters.095 = cluster_optimum(mat0.95)
n.clusters.0975 = cluster_optimum(mat0.975)
n.clusters.099 = cluster_optimum(mat0.99)

km.obj.09 = kmeans(mat0.9, n.clusters.090)
km.obj.095 = kmeans(mat0.95, n.clusters.095)
km.obj.0975 = kmeans(mat0.975, n.clusters.0975)
km.obj.099 = kmeans(mat0.99, n.clusters.099)

# 0.9

events.monthly.09 = cluster_events_monthly(km.obj.09, mat0.9, "0.9")

ggarrange(events.monthly.09[[1]],
          ggarrange(events.monthly.09[[2]],
                    events.monthly.09[[3]],
                    events.monthly.09[[4]],
                    events.monthly.09[[5]],
                    ncol = 2, nrow = 2))

# 0.95

events.monthly.095 = cluster_events_monthly(km.obj.095, mat0.95, "0.95")

ggarrange(events.monthly.095[[1]],
          ggarrange(events.monthly.095[[2]],
                    events.monthly.095[[3]],
                    events.monthly.095[[4]],
                    events.monthly.095[[5]],
                    events.monthly.095[[6]],
                    events.monthly.095[[7]],
                    ncol = 3, nrow = 2))

# 0.975

events.monthly.0975 = cluster_events_monthly(km.obj.0975, mat0.975, "0.975")

ggarrange(events.monthly.0975[[1]],
          ggarrange(events.monthly.0975[[2]],
                    events.monthly.0975[[3]],
                    events.monthly.0975[[4]],
                    events.monthly.0975[[5]],
                    events.monthly.0975[[6]],
                    events.monthly.0975[[7]],
                    ncol = 3, nrow = 2))

# 0.99

events.monthly.099 = cluster_events_monthly(km.obj.099, mat0.99, "0.99")

ggarrange(events.monthly.099[[1]],
          ggarrange(events.monthly.099[[2]],
                    events.monthly.099[[3]],
                    events.monthly.099[[4]],
                    ncol = 2, nrow = 2))
