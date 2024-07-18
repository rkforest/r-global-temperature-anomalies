setwd("/Users/rkforest/R-projects/R-global-temperature-anomalies")
proj_dir <- getwd()
proj_dir

data_dir = "/Users/rkforest/R-projects/R-global-temperature-anomalies/data/csv"
setwd(data_dir)
getwd()

# import csv file
df = read.table("GLB.Ts+dSSt.csv")
