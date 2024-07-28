library(tidyverse)
transformed_data_dir <- file.path("data", "2-transformed-data")
aggregated_data_dir <- file.path("data", "3-aggregated-data")

file_path <- file.path(transformed_data_dir, "global_monthly_df.rds")
global_monthly_df <- read_rds(file_path)
file_path <- file.path(transformed_data_dir, "northern_monthly_df.rds")
northern_monthly_df <- read_rds(file_path)
file_path <- file.path(transformed_data_dir, "southern_monthly_df.rds")
southern_monthly_df <- read_rds(file_path)

file_path <- file.path(transformed_data_dir, "zonal_annual_df.rds")
zonal_annual_df <- read_rds(file_path)

head(global_monthly_df,1)
tail(global_monthly_df,1)


global_monthly_df |> 
  group_by(Climate) |>
  summarize(
    avg_anomaly = mean(Anomaly),
    count = n()
  )

global_monthly_df |> 
  group_by(Decade) |>
  summarize(
    avg_anomaly = mean(Anomaly),
    count = n()
  )

global_monthly_df |> 
  group_by(Month) |>
  summarize(
    avg_anomaly = mean(Anomaly),
    count = n()
  )

head(northern_monthly_df,1)
tail(northern_monthly_df,1)
head(southern_monthly_df,1)
tail(southern_monthly_df,1)

northern_monthly_df |> 
  group_by(Month) |>
  summarize(
    avg_anomaly = mean(Anomaly),
    count = n()
  )

head(zonal_annual_df,1)
tail(zonal_annual_df,1)

zonal_annual_df |> 
  group_by(Zone) |>
  summarize(
    avg_anomaly = mean(Anomaly),
    count = n()
  )
