source("R/data_interfaces.R")
source("R/preprocess_discharge.R")

discharge <- load_data()
discharge <- discharge$discharge

discharge.east = filter_region(discharge)
discharge.east.long = make_rec_discharge(discharge.east, reshape = T)

# A lot of missing data points
plot_rec_discharge(discharge.east.long)

# Subset
discharge.data = subset_discharge(discharge.east,
                                  discharge.east.long,
                                  day_threshold = 364)

# No missing data points!
plot_rec_discharge(make_rec_discharge(discharge.data, reshape = T))





