# to do:
# 1) add climate period 30 year averages column to global_monthly_df
# 2) 
# 3) 
# 4) 



library(tidyverse)

data_dir <- file.path("data", "rds")

# read global rda file
# change Id column type to factor
file_path <- file.path(data_dir, "global_anomalies.rds")
global_df <- read_rds(file_path) |>
  mutate(Id = factor(Id)) 

# read zonal rda file
file_path <- file.path(data_dir, "zonal_anomalies.rds")
zonal_df <- read_rds(file_path)

# global annual anomalies
#   select annual anomaly column
#   rename column
#   remove missing values
global_annual_df <- global_df |> 
  rename(Anomaly = `J-D`) |> 
  filter(!is.na(Anomaly)) |>
  select(Year, Id, Anomaly)
head(global_annual_df,2)
tail(global_annual_df,2)

# global monthly anomalies
#   select 12 monthly anomaly columns
#   tidy to 1 named column and 1 values column
#   remove missing values
#   change month column type to factor
global_monthly_df <- global_df |> 
  pivot_longer(
    cols=c(Jan,Feb,Mar,Apr,May,Jun,Jul,Aug,Sep,Oct,Nov,Dec), 
    names_to = "Month",
    values_to = "Anomaly",
    values_drop_na = TRUE) |> 
  mutate(Month = factor(Month)) |> 
  select(Year, Id, Month, Anomaly)
head(global_monthly_df,2)
tail(global_monthly_df,2)

# global seasonal anomalies
#   select 4 seasonal anomaly columns
#   rename columns 
#   tidy to 1 named column and 1 values column
#   remove missing values
#   change season column type to factor
global_seasonal_df <- global_df |> 
  rename(Winter = DJF) |> 
  rename(Spring = MAM) |> 
  rename(Summer = JJA) |> 
  rename(Autumn = SON) |> 
  pivot_longer(cols=c(Winter,Spring,Summer,Autumn), 
               names_to = "Season",
               values_to = "Anomaly",
               values_drop_na = TRUE) |> 
  mutate(Season = factor(Season)) |> 
  select(Year, Id, Season, Anomaly)
head(global_seasonal_df,2)
tail(global_seasonal_df,2)

# zonal annual anomalies
#   create vector of zone column names
#   tidy to 1 named column and 1 values column
#   remove missing values
#   change zone column type to factor
z_col_names = colnames(zonal_df[5:15])
zonal_annual_df <- zonal_df |> 
  pivot_longer(cols = all_of(z_col_names),
               names_to = "Zone",
               values_to = "Anomaly",
               values_drop_na = TRUE) |> 
  mutate(Zone = factor(Zone)) |> 
  select(Year, Zone, Anomaly)
head(zonal_annual_df,2)
tail(zonal_annual_df,2)

#
file_path = file.path(data_dir, "global_annual.rds")
write_rds(global_annual_df, file_path)
file_path = file.path(data_dir, "global_monthly.rds")
write_rds(global_monthly_df, file_path)
file_path = file.path(data_dir, "global_seasonal.rds")
write_rds(global_seasonal_df, file_path)
file_path = file.path(data_dir, "zonal_annual.rds")
write_rds(zonal_annual_df, file_path)