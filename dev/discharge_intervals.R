source("R/data_interfaces.R")

discharge <- load_data()
discharge <- discharge$discharge

discharge_east <- discharge[regine_area < 17]
discharge_east <- discharge_east[,c("stat_id","qt","date")]

uniquestations <- unique(discharge_east$stat_id)
alldates = data.table("date"=seq(min(discharge_east$date), max(discharge_east$date),by=1))
alldates$date = lubridate::ymd(alldates$date)
savestations = list()

for (j in 1:length(uniquestations)) {
  this_station = discharge_east[stat_id == uniquestations[j]]
  setnames(this_station, "qt", paste0("obs_",uniquestations[j]))
  this_station[,stat_id:=NULL]
  
  if (j == 1) {
    savestation = this_station
  } else {
    savestation = merge(savestation, this_station, by = "date", all.x = T, all.y = T)
  }
}

savestation = merge(alldates, savestation, by = c("date"),all.x=TRUE)
savestation = savestation[!duplicated(savestation),]


library(tidyverse)
savestation |> 
  pivot_longer(!date,names_to = "obs", values_to = "val") -> savestation_long 
savestation_long$val <- if_else(is.na(savestation_long$val),0,1)
savestation_long$year <- year(savestation_long$date)

savestation_long |> 
  group_by(obs) |> 
  mutate(nSum = sum(val)) |> 
  arrange(-nSum, -val) |> 
  select(-nSum) -> savestation_long

savestation_long$val <- factor(savestation_long$val)
savestation_long |> 
  group_by(obs, year) |> 
  mutate(Freq = sum(val)) |> 
  filter(Freq > 350) |> 
  filter(year > 1985) |> 
  select(-Freq) |> 
  ungroup() |> 
  mutate(val = factor(val)) |> 
  ggplot(aes(x = date, y = obs)) +
  geom_raster(aes(fill = val)) +
  scale_fill_manual(values = c("white", "blue")) +
  scale_x_date(breaks = "2 year", date_labels = "%Y") +
  labs(title = "Recorded discharge per station and date") +
  theme_bw() +
  theme(axis.text.y = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
  
savestation_long$val <- as.integer(savestation_long$val)
savestation_long |> 
  group_by(year, obs) |> 
  summarise(Freq = sum(val)) |> 
  filter(year > 1985) |> 
  filter(Freq > 350) |> 
  group_by(year) |> 
  summarise(nstat = length(unique(obs))) |> 
  ggplot(aes(x = year, y = nstat)) +
  geom_col()


savestation_long |> 
  filter(year > 1985) |> 
  group_by(obs) |> 
  summarise(Freq = sum(val)) |> 
  filter(Freq > 350*year_diff) |> 
  ungroup() |> 
  select(obs) |> 
  unique() |> 
  as.vector() -> stations_to_keep

savestation_obs <- savestation_long |> 
  select(obs) |> unique() |> as.vector()

savestation_long |> 
  group_by(year, obs) |> 
  summarise(Freq = sum(val)) |> 
  filter(year > 1985) |> 
  ungroup() |> as.data.table() -> test

year_threshold <- 1985
stations_to_keep <- c()
for (i in 1:length(savestation_obs$obs)) {
  bool <- FALSE
  this_station <- test[obs == savestation_obs$obs[i]]
  for (j in 1:nrow(this_station)) {
    this_year <- this_station[j,]
    if (this_year$Freq < 350) {
      bool <- TRUE
    }
  }
  if (bool == FALSE) {
    stations_to_keep <- c(stations_to_keep, savestation_obs$obs[i])
  }
}

savestation_long |> 
  filter(year > 1985) |> 
  filter(obs %in% stations_to_keep) |> 
  mutate(val = factor(val)) |> 
  ggplot(aes(x = date, y = obs)) +
  geom_raster(aes(fill = val)) +
  scale_fill_manual(values = c("white", "blue")) +
  scale_x_date(breaks = "2 year", date_labels = "%Y") +
  labs(title = "Recorded discharge per station and date") +
  theme_bw() +
  theme(axis.text.y = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

