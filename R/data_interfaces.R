load_data <- function(type = "discharge") {
  if (type == "discharge") {
    load("data/nvedat.RData")
    l = list(discharge = nvedat)
    return (l)
  } else {
    return (NULL)
  }
}

