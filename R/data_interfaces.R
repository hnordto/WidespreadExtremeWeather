load_data <- function(type = "discharge") {
  if (type == "discharge") {
    data(nvedat.RData)
    l = list(nvedat)
    return(l)
  } else {
    return (NULL)
  }
}
