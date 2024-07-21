library(tidyverse)

save_csv_dir <- file.path("data", "csv")
save_rds_dir <- file.path("data", "rds")

urls <- c("https://data.giss.nasa.gov/gistemp/tabledata_v4/GLB.Ts+dSST.csv",
          "https://data.giss.nasa.gov/gistemp/tabledata_v4/NH.Ts+dSST.csv",
          "https://data.giss.nasa.gov/gistemp/tabledata_v4/SH.Ts+dSST.csv",
          "https://data.giss.nasa.gov/gistemp/tabledata_v4/ZonAnn.Ts+dSST.csv"
          )
skip_header_recs <- c(1, 1, 1, 0)
data_ids <- c('GLB','NH','SH')

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

# save raw data into csv directory
for (i in 1:length(df_list)) {
  file_path <- file.path(save_csv_dir, basename(urls[i]))
  df <- df_list[[i]]
  write_csv(df, file_path)
}

# add Id to the global, northern, and southern data frames before combining
for (i in 1:3) {
  df <- df_list[[i]]
  df$Id = data_ids[i]
  df_list[[i]] <- df
}
global_df <- dplyr::bind_rows (df_list[[1]],
                               df_list[[2]],
                               df_list[[3]])
# save rds files
file_path = file.path(save_rds_dir, "global_anomalies.rds")
write_rds(global_df, file_path)

zonal_df <- df_list[[4]]
file_path = file.path(save_rds_dir, "zonal_anomalies.rds")
write_rds(zonal_df, file_path)
