source("R/data_interfaces.R")
source("R/preprocess_discharge.R")

discharge <- load_data()
discharge <- discharge$discharge

discharge.east = filter_region(discharge)
discharge.east.long = make_rec_discharge(discharge.east, reshape = T)

# A lot of missing data points
p = plot_rec_discharge(discharge.east.long)
p

ggsave(p,
       filename = "discharge_rec_east.png",
       path = "//ad.nr.no/shares/samba_shared/Sommerstudenter/Henrik/figures")

# Subset
discharge.data = subset_discharge(discharge.east,
                                  discharge.east.long,
                                  day_threshold = 364)

# No missing data points!
p = plot_rec_discharge(make_rec_discharge(discharge.data, reshape = T))
p

ggsave(p,
       filename = "discharge_rec_east_processed.png",
       path = "//ad.nr.no/shares/samba_shared/Sommerstudenter/Henrik/figures")






