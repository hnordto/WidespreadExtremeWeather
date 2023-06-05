source("R/data_interfaces.R")

discharge <- load_data()
discharge <- discharge$discharge

discharge_east <- discharge[regine_area < 16]
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
  mutate(Freq = n()) |> 
  filter(Freq > 350) |> 
  filter(year > 1985) |> 
  select(-Freq) |> 
  ungroup() |> 
  filter(year > 1985) |> 
  ggplot(aes(x = date, y = obs)) +
  geom_raster(aes(fill = val)) +
  scale_fill_manual(values = c("white", "blue")) +
  scale_x_date(breaks = "2 year", date_labels = "%Y") +
  labs(title = "Recorded discharge per station and date") +
  theme_bw() +
  theme(axis.text.y = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
  

savestation_long |> 
  group_by(obs, year) |> 
  mutate(Freq = n())


