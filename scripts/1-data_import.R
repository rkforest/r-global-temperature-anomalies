library(tidyverse)

save_csv_dir <- file.path("data", "raw-data", "csv")
save_rds_dir <- file.path("data", "raw-data", "rds")

urls <- c("https://data.giss.nasa.gov/gistemp/tabledata_v4/GLB.Ts+dSST.csv",
          "https://data.giss.nasa.gov/gistemp/tabledata_v4/NH.Ts+dSST.csv",
          "https://data.giss.nasa.gov/gistemp/tabledata_v4/SH.Ts+dSST.csv",
          "https://data.giss.nasa.gov/gistemp/tabledata_v4/ZonAnn.Ts+dSST.csv"
          )
skip_header_recs <- c(1, 1, 1, 0)

get_df <- function(url, skip_header_recs) {
  df <- read_csv(file=url,
                 skip=skip_header_recs,
                 na = "***",
                 show_col_types = FALSE)
}

# download from urls into list of data frames
df_list <- list()
for (i in 1:length(urls)) {
 df <- get_df(urls[i], skip_header_recs[i])
 df_list[[i]] <- df
}

# build rds file name
rds_file_names <- character()
for (i in 1:length(df_list)) {
  base_name <- basename(urls[i])
  file_without_suffix <- substr(base_name,1,nchar(base_name)-4)
  file_with_rds_suffix <- paste(file_without_suffix, ".rds", sep = "")
  rds_file_names <- c(rds_file_names,file_with_rds_suffix)
}

for (i in 1:length(df_list)) {
  df <- df_list[[i]]
  base_name <- basename(urls[i])
  
  # save csv data into csv directory 
  csv_file_path <- file.path(save_csv_dir, base_name)
  write_csv(df, csv_file_path)
  
  # save rds data into rds directory
  rds_file_path <- file.path(save_rds_dir, rds_file_names[i])
  write_rds(df, rds_file_path)
}

