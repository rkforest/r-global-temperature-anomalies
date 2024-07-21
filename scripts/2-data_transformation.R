library(tidyverse)

raw_data_rds_dir <- file.path("data", "raw-data", "rds")
sav_tidy_rds_dir <- file.path("data", "tidy-data")

file_names <- list.files(raw_data_rds_dir)
file_prefix <- character()
for (i in 1:length(file_names)) {
  file_prefix <- c(file_prefix, substr(file_names[i],1,2))
}

df_list <- list()
for (i in 1:length(file_names)) {
  rds_file_path <- file.path(raw_data_rds_dir, file_names[i])
  df <- read_rds(rds_file_path)
  df <- cbind(Area = file_prefix[i], df)
  df_list[[i]] <- df
}

df1 <-df_list[[1]]
df2 <-df_list[[2]]
df3 <-df_list[[3]]
df4 <-df_list[[4]]

# global,northern and southern data frames
#   select 12 monthly columns
#   tidy to column of month names and column of month anomaly values
#   remove missing values
#   change new month names column type to factor

global_monthly_df <- df1 |> 
  pivot_longer(
    cols=c(Jan,Feb,Mar,Apr,May,Jun,Jul,Aug,Sep,Oct,Nov,Dec), 
    names_to = "Month",
    values_to = "Anomaly",
    values_drop_na = TRUE) |> 
  mutate(Area = factor(Area)) |> 
  mutate(Month = factor(Month)) |> 
  select(Area, Year, Month, Anomaly)

northern_monthly_df <- df2 |> 
  pivot_longer(
    cols=c(Jan,Feb,Mar,Apr,May,Jun,Jul,Aug,Sep,Oct,Nov,Dec), 
    names_to = "Month",
    values_to = "Anomaly",
    values_drop_na = TRUE) |> 
  mutate(Area = factor(Area)) |> 
  mutate(Month = factor(Month)) |> 
  select(Area, Year, Month, Anomaly)

southern_monthly_df <- df3 |> 
  pivot_longer(
    cols=c(Jan,Feb,Mar,Apr,May,Jun,Jul,Aug,Sep,Oct,Nov,Dec), 
    names_to = "Month",
    values_to = "Anomaly",
    values_drop_na = TRUE) |> 
  mutate(Area = factor(Area)) |> 
  mutate(Month = factor(Month)) |> 
  select(Area, Year, Month, Anomaly)

# zonal annual anomalies
#   create vector of zone column names
#   tidy to 1 named column and 1 values column
#   remove missing values
#   change zone column type to factor
z_col_names = colnames(df4[,6:16]) 
zonal_annual_df <- df4 |>
  pivot_longer(cols = all_of(z_col_names),
               names_to = "Zone",
               values_to = "Anomaly",
               values_drop_na = TRUE) |> 
  mutate(Area = factor(Area)) |> 
  mutate(Zone = factor(Zone)) |> 
  select(Area, Year, Zone, Anomaly)

# global seasonal anomalies
#   select 4 seasonal anomaly columns
#   rename columns 
#   tidy to 1 named column and 1 values column
#   remove missing values
#   change season column type to factor
global_seasonal_df <- df1 |> 
  rename(Winter = DJF) |> 
  rename(Spring = MAM) |> 
  rename(Summer = JJA) |> 
  rename(Autumn = SON) |> 
  pivot_longer(cols=c(Winter,Spring,Summer,Autumn), 
               names_to = "Season",
               values_to = "Anomaly",
               values_drop_na = TRUE) |> 
  mutate(Area = factor(Area)) |> 
  mutate(Season = factor(Season)) |> 
  select(Area, Year, Season, Anomaly)
head(global_seasonal_df,2)
tail(global_seasonal_df,2)

# save rds data into tidy data directory
rds_file_path <- file.path(sav_tidy_rds_dir, "global_monthly_df.rds")
write_rds(global_monthly_df, rds_file_path)
rds_file_path <- file.path(sav_tidy_rds_dir, "northern_monthly_df.rds")
write_rds(northern_monthly_df, rds_file_path)
rds_file_path <- file.path(sav_tidy_rds_dir, "southern_monthly_df.rds")
write_rds(southern_monthly_df, rds_file_path)

rds_file_path <- file.path(sav_tidy_rds_dir, "zonal_annual_df.rds")
write_rds(zonal_annual_df, rds_file_path)

rds_file_path <- file.path(sav_tidy_rds_dir, "global_seasonal_df.rds")
write_rds(global_seasonal_df, rds_file_path)

