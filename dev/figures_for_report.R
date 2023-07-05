# Discharge workflow

discharge = load_data(type = "discharge")
discharge = discharge$discharge

discharge.east = filter_region(discharge)
discharge.east.long = make_rec_discharge(discharge.east, reshape = T)

discharge.data = subset_discharge(discharge.east,
                                  discharge.east.long,
                                  day_threshold = 365)

data = discharge.data[stat_id == 200604]
data$year = year(data$date)
data = data[year %in% c(2013)]
data$month = month(data$date)
data = data[month %in% c(4,5,6,7)]

threshold = extreme_threshold(discharge.data, probs = .99, type = "discharge")
threshold = threshold[stat_id == 200604]

extremes = extreme_events(discharge.data, probs = .99)
extremes = extremes[stat_id == 200604]
extremes$year = year(extremes$date)
extremes = extremes[year %in% c(2013)]
extremes$month = month(extremes$date)
extremes = extremes[month %in% c(4,5,6,7)]
#mains = main_events(extremes)

#extremes = extremes[date %in% mains]

ggplot() +
  geom_line(aes(x = date, y = qt), data = data, colour = "steelblue", size = 1.5) +
  geom_point(aes(x = date, y = qt), data = extremes, size = 5, colour= "orange") +
  geom_hline(aes(yintercept = us), data = threshold, linetype = "dashed", size = 1.5, colour = "red") +
  labs(x = element_blank(),
       y = element_blank()) +
  theme_bw() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank())


