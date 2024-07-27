library(tidyverse)
transformed_data_dir <- file.path("data", "2-transformed-data")
aggregated_data_dir <- file.path("data", "3-aggregated-data")
file_names <- list.files(transformed_data_dir)
file_names

file_path <- file.path(transformed_data_dir, file_names[1])
global_monthly_df <- read_rds(file_path)

file_path <- file.path(transformed_data_dir, file_names[2])
zonal_annual_df <- read_rds(file_path)

head(global_monthly_df,1)
tail(global_monthly_df,1)

head(zonal_annual_df,1)
tail(zonal_annual_df,1)

  

