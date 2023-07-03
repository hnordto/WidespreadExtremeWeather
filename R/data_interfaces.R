load_data <- function(type = "both") {
  if (type == "discharge") {
    load("data/nvedat.RData")
    l = list(discharge = nvedat)
    return (l)
  } else if (type == "precip") {
    load("data/metdat.RData")
    l = list(precip = metdat)
    return (l)
  } else if (type == "both") {
    load("data/nvedat.RData")
    load(data/metdat.RData)
    l = list(discharge = nvedat,
             precip = metdat)
    return (l)
  } else {
    return (NULL)
  }
}

