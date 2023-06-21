wd="//ad.nr.no/shares/samba_shared/Sommerstudenter/Henrik/Data/Precipitation"
setwd(wd)

library(readr)

prec = read_delim("//ad.nr.no/shares/samba_shared/Sommerstudenter/Henrik/Data/Precipitation/Hourly/rr_1_kdvh_sommerjobb_NR_01012010_31122022_v01.csv", 
                  delim = ";", escape_double = FALSE, col_names = FALSE, 
                  locale = locale(decimal_mark = ",", grouping_mark = ""), 
                  trim_ws = TRUE, skip = 3)
prec = prec[rowSums(is.na(prec)) != ncol(prec),]

prec = data.table("date" = as_datetime(prec$X2),
                  qt = prec$X3,
                  stat_id = prec$X1)

# Gap sizes

res = rle(is.na(prec$qt))
rep(res$values*res$lengths, res$lengths)
