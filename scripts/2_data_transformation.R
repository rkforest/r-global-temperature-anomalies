library(tidyverse)

raw_data_dir <- file.path("data", "1-raw-data", "rds")
transformed_data_dir <- file.path("data", "2-transformed-data")
file_names <- list.files(raw_data_dir)

df_list <- list()
for (i in 1:length(file_names)) {
  file_path <- file.path(raw_data_dir, file_names[i])
  df <- read_rds(file_path)
  df_list[[i]] <- df
}

df1 <-df_list[[1]]
df2 <-df_list[[2]]
df3 <-df_list[[3]]
df4 <-df_list[[4]]

climate_levels <- c("1881-1910", "1911-1940", "1941-1970",
                    "1971-2000", "2001-2030")

month_levels <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

season_levels <- c("Winter","Spring","Summer","Autumn")

# functions
fn_transform_monthly <- function(df, area_id) {
  # global, northern and southern data frames
  #   filter for years > 1880
  #   select 12 monthly columns
  #   tidy to column of month names and column of month anomaly values
  #   remove missing values
  #   add 30 year Climate_Period based on year
  #   add season column based on new month column
  #   change Area and Month columns to factors
  #   change Year and Decade columns to integers
  #   change Climate_Period column to factor and add labels
  #   change new month names column type to factor 

  df_transformed <- df |> 
    filter(Year>1880) |>
    pivot_longer(
      cols=c(Jan,Feb,Mar,Apr,May,Jun,Jul,Aug,Sep,Oct,Nov,Dec), 
      names_to = "Month",
      values_to = "Anomaly",
      values_drop_na = TRUE) |> 
    mutate(Area = area_id) |>
    mutate(Climate = NA) |>
    mutate(Climate = case_when(
      Year <= 1910 ~ climate_levels[1],
      Year <= 1940 ~ climate_levels[2],
      Year <= 1970 ~ climate_levels[3],
      Year <= 2000 ~ climate_levels[4],
      Year <= 2030 ~ climate_levels[5])) |>
    mutate(Season = NA) |>
    mutate(Season = case_when(
      Month %in% c("Dec","Jan","Feb") ~ season_levels[1],
      Month %in% c("Mar","Apr","May") ~ season_levels[2],
      Month %in% c("Jun","Jul","Aug") ~ season_levels[3],
      Month %in% c("Oct","Nov","Dec") ~ season_levels[4])) |>
    mutate(Area = factor(Area)) |> 
    mutate(Year = as.integer(Year)) |>
    mutate(Decade = as.integer(Year - (Year %% 10) + 10)) |>
    select(Area, Climate, Decade, Year, Season, Month, Anomaly) 
  
  df_transformed$Season <- factor(df_transformed$Season,
                                  levels=season_levels)
  df_transformed$Month <- factor(df_transformed$Month,
                                 levels=month_levels)
  df_transformed$Climate<- factor(df_transformed$Climate,
                                  levels=climate_levels,
                                  labels=climate_levels)
  return(df_transformed)
}

global_monthly_df <- fn_transform_monthly(df1,"G")
northern_monthly_df <- fn_transform_monthly(df2,"N")
southern_monthly_df <- fn_transform_monthly(df3,"S")

global_df <- dplyr::bind_rows(global_monthly_df,
                              northern_monthly_df,
                              southern_monthly_df)

# zonal annual anomalies
#   create vector of zone column names
#   tidy to 1 named column and 1 values column
#   remove missing values
#   change zone column type to factor
z_col_names = colnames(df4[,5:15]) 
zonal_df <- df4 |>
  pivot_longer(cols = all_of(z_col_names),
               names_to = "Zone",
               values_to = "Anomaly",
               values_drop_na = TRUE) |> 
  mutate(Climate = NA) |>
  mutate(Climate = case_when(
    Year <= 1910 ~ climate_levels[1],
    Year <= 1940 ~ climate_levels[2],
    Year <= 1970 ~ climate_levels[3],
    Year <= 2000 ~ climate_levels[4],
    Year <= 2030 ~ climate_levels[5])) |>
  mutate(Zone = factor(Zone)) |> 
  mutate(Year = as.integer(Year)) |>
  mutate(Decade= as.integer(Year - (Year %% 10) + 10)) |> 
  mutate(Climate_Period = factor(Climate, labels=climate_levels)) |> 
  select(Zone, Climate, Decade, Year, Anomaly)

# save rds data into tidy data directory
rds_file_path <- file.path(transformed_data_dir, "global_monthly_df.rds")
write_rds(global_df, rds_file_path)

rds_file_path <- file.path(transformed_data_dir, "zonal_annual_df.rds")
write_rds(zonal_df, rds_file_path)

