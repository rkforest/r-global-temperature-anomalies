suppressMessages(library(tidyverse))
suppressMessages(library(ncdf4))

raw_data_tabular_dir <- file.path("data", "1-raw-data", "tabular")
raw_data_gridded_dir <- file.path("data", "1-raw-data", "gridded")
transformed_tabular_data_dir <- file.path("data", "2-transformed-data", "tabular")
transformed_gridded_data_dir <- file.path("data", "2-transformed-data", "gridded")

# remove previous files
f <- list.files(transformed_tabular_data_dir, full.names=TRUE)
status <- file.remove(f)
f <- list.files(transformed_gridded_data_dir, full.names=TRUE)
status <- file.remove(f)

# factors
climate_levels <- c("1891-1920", "1921-1950", "1951-1980",
                    "1981-2010", "2011-2024")

month_levels <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

season_levels <- c("Winter","Spring","Summer","Autumn")

hemisphere_levels <- c("S","N")
hemisphere_labels <- c("Southern", "Northern")

zone_x3_levels <- c( "90S-24S", "24S-24N", "24N-90N")
zone_x8_levels <- c( "90S-64S", "64S-44S", "44S-24S", "24S-EQU",
                     "EQU-24N", "24N-44N", "44N-64N", "64N-90N")
zone_x11_levels <- c(zone_x3_levels,zone_x8_levels)

##### csv files #####

skip_header_recs <- c(1, 1, 1, 0)
file_names <- list.files(raw_data_tabular_dir)

df_list <- list()
for (i in 1:length(file_names)) {
  file_path <- file.path(raw_data_tabular_dir, file_names[i])
  df <- read_csv(file=file_path,
                 skip=skip_header_recs[i],
                 na = "***",
                 show_col_types = FALSE)
  df_list[[i]] <- df
}

df1 <-df_list[[1]]
df2 <-df_list[[2]]
df3 <-df_list[[3]]
df4 <-df_list[[4]]

# functions

fn_add_climate_period <- function(df) {
  #   add 30 year climate identifier variable based on year
  #   change climate variable to factor and add labels
  dfc <- df |> 
    mutate(climate = case_when(
      year <= 1920 ~ climate_levels[1],
      year <= 1950 ~ climate_levels[2],
      year <= 1980 ~ climate_levels[3],
      year <= 2010 ~ climate_levels[4],
      year <= 2040 ~ climate_levels[5]))
  dfc$climate<- factor(dfc$climate,
                       levels=climate_levels,
                       labels=climate_levels)
  return(dfc)
} 

fn_transform_monthly <- function(df, id) {
  #   filter for years > 1890
  #   tidy 12 monthly columns to column of month names and column of anomaly values
  #   remove missing values
  #   change identifier and month columns to factors
  #   change year and decade columns to integers
  #   change new month names column type to factor 
  #.  call function to add climate period identifier based on year
  dft <- df |> 
    filter(Year > 1890) |>
    pivot_longer(
      cols=c(Jan,Feb,Mar,Apr,May,Jun,Jul,Aug,Sep,Oct,Nov,Dec), 
      names_to = "month",
      values_to = "anomaly",
      values_drop_na = TRUE) |> 
    mutate(identifier = id) |>
    mutate(identifier = factor(identifier)) |> 
    mutate(year = as.integer(Year)) |>
    mutate(decade = as.integer(year - (year %% 10) + 10))
  dft$month <- factor(dft$month, levels=month_levels)
  dfc <- fn_add_climate_period(dft) |>
    select(identifier, climate, decade, year, month, anomaly) 
  return(dfc)
}

fn_add_seasons_nh <- function(df) {
  # northern and southern data frames
  #   add season column based on new month column
  #   seasons will differ by hemisphere
  dfs <- df |> 
    rename(hemisphere=identifier) |>
    mutate(season = case_when(
      month %in% c("Dec","Jan","Feb") ~ season_levels[1],
      month %in% c("Mar","Apr","May") ~ season_levels[2],
      month %in% c("Jun","Jul","Aug") ~ season_levels[3],
      month %in% c("Sep","Oct","Nov") ~ season_levels[4])) 
  dfs$hemisphere <- factor(dfs$hemisphere, 
                          levels=hemisphere_levels,
                          labels=hemisphere_labels)
  dfs$season <- factor(dfs$season,
                       levels=season_levels)
  return(dfs)
}

fn_add_seasons_sh <- function(df) {
  # northern and southern data frames
  #   add season column based on new month column
  #   seasons will differ by hemisphere
  dfs <- df |> 
    rename(hemisphere=identifier) |>
#    mutate(Season = NA) |>
    mutate(season = case_when(
      month %in% c("Dec","Jan","Feb") ~ season_levels[3],
      month %in% c("Mar","Apr","May") ~ season_levels[4],
      month %in% c("Jun","Jul","Aug") ~ season_levels[1],
      month %in% c("Sep","Oct","Nov") ~ season_levels[2])) 
  dfs$hemisphere <- factor(dfs$hemisphere, 
                           levels=hemisphere_levels,
                           labels=hemisphere_labels)
  dfs$season <- factor(dfs$season,
                       levels=season_levels)
  return(dfs)
}

# transform monthly data using function
global_monthly_df <- fn_transform_monthly(df1,id="G")
glimpse(global_monthly_df)
northern_monthly_df <- fn_transform_monthly(df2,id="N")
glimpse(northern_monthly_df)
southern_monthly_df <- fn_transform_monthly(df3,id="S")
glimpse(southern_monthly_df)

# add seasons to hemisphere data
northern_monthly_df <- fn_add_seasons_nh(northern_monthly_df)
southern_monthly_df <- fn_add_seasons_sh(southern_monthly_df)

# combine northern and southern hemisphere into one data frame
hemisphere_monthly_df <- dplyr::bind_rows(northern_monthly_df,
                                          southern_monthly_df)


# #### zonal annual anomalies #####


# create separate zonal dfs for 3 zones version and 8 zones version
#   create vector of zone column names
#   tidy to 1 named column and 1 values column
#   remove missing values
#   change zone column type to factor
z_col_names = colnames(df4[,5:7]) 
z3_df <- df4 |>
  pivot_longer(cols =  all_of(z_col_names),
               names_to = "zone",
               values_to = "anomaly",
               values_drop_na = TRUE) |> 
  filter(Year > 1890) |>
  mutate(year = as.integer(Year)) |>
  mutate(decade= as.integer(year - (year %% 10) + 10)) 
z3_df <- fn_add_climate_period(z3_df) |> 
  select(zone, climate, decade, year, anomaly)
z3_df$zone <- factor(z3_df$zone, levels=zone_x3_levels)

z_col_names = colnames(df4[,8:15]) 
z8_df <- df4 |>
  pivot_longer(cols =  all_of(z_col_names),
               names_to = "zone",
               values_to = "anomaly",
               values_drop_na = TRUE) |> 
  filter(Year > 1890) |>
  mutate(year = as.integer(Year)) |>
  mutate(decade= as.integer(year - (year %% 10) + 10)) 
z8_df <- fn_add_climate_period(z8_df) |> 
  select(zone, climate, decade, year, anomaly)
z8_df$zone <- factor(z8_df$zone, levels=zone_x8_levels)
                        
# save rds data into transformed data directory
rds_file_path <- file.path(transformed_tabular_data_dir, "global_monthly_df.rds")
write_rds(global_monthly_df, rds_file_path)
rds_file_path <- file.path(transformed_tabular_data_dir, "hemisphere_monthly_df.rds")
write_rds(hemisphere_monthly_df, rds_file_path)

rds_file_path <- file.path(transformed_tabular_data_dir, "zonal_3x_annual_df.rds")
write_rds(z3_df, rds_file_path)
rds_file_path <- file.path(transformed_tabular_data_dir, "zonal_8x_annual_df.rds")
write_rds(z8_df, rds_file_path)


##### netcdf #####

library(lattice)
library(RColorBrewer)

fn_transform_netcdf <- function(df) {
  #   extract year and month from chr date
  #   filter for years > 1890
  #   add decade column based on year
  #   change year, month, decade, lon, lat to integers
  #   change anomaly to numeric
  dft <- df |> 
    mutate(year = as.integer(str_sub(chrdate,1,4))) |>
    mutate(month = as.integer(str_sub(chrdate,6,7))) |>
    filter(year > 1890) |>
    mutate(decade = as.integer(year - (year %% 10) + 10)) |>
    mutate(lon = as.integer(lon)) |>
    mutate(lat = as.integer(lat)) |>
    mutate(anomaly = as.numeric(anomaly))
 dfc <- fn_add_climate_period(dft) |> 
   select(climate, decade, year, month, lat, lon, anomaly) 
  return(dfc)
}

file_name_nc = "gistemp1200_GHCNv4_ERSSTv5.nc"
#file_name_nc = "gistemp250_GHCNv4.nc"
netcdf_path <- file.path("data", "1-raw-data", "gridded", file_name_nc)
ncin <- ncdf4::nc_open(netcdf_path)

# get longitude, latitude and time variables
lon <- ncvar_get(ncin,"lon")
lat <- ncvar_get(ncin,"lat")
time <- ncvar_get(ncin,"time")
anomaly <- ncvar_get(ncin,"tempanomaly")

# get time variables and convert to dates
date_var <- as.Date(time, origin = '1800-01-01')

# Create matrix of long, lat, date
lon_lat_date <- as.matrix(expand.grid(lon,lat,date_var))

# store temperature anomaly variable as vector
anomaly_v <- as.vector(anomaly)
length(anomaly_v) # 180 * 90 * 1734 = 28090800

# Create data.frame
df <- data.frame(cbind(lon_lat_date, anomaly_v))
df <- na.omit(df)
colnames(df) <- c("lon", "lat", "chrdate", "anomaly")
glimpse(df)

# transform using function
gridded_df <- fn_transform_netcdf(df)
colnames(gridded_df)

# calculate average anomaly by climate period, lon, and lat
gridded_climate_df <- gridded_df |>
  group_by(climate, lon, lat) |>
  summarize(avg_anomaly= mean(anomaly))
glimpse(gridded_climate_df)

# calculate average anomaly by decade, lon, and lat
gridded_decade_df <- gridded_df |>
  group_by(decade, lon, lat) |>
  summarize(avg_anomaly= mean(anomaly))
glimpse(gridded_decade_df)

rds_file_path <- file.path(transformed_gridded_data_dir,"global_by_climate_period_df.rds")
write_rds(gridded_climate_df, rds_file_path)
rds_file_path <- file.path(transformed_gridded_data_dir,"global_by_decade_df.rds")
write_rds(gridded_decade_df, rds_file_path)


# get a single slice or layer (January)
m <- 1734
tmp_slice <- anomaly[,,m]
dim(tmp_slice)
glimpse(tmp_slice )

# levelplot
grid <- expand.grid(lon=lon, lat=lat)
cutpts <- c(-5, -4,-3,-2,-1,0,1,2,3,4,5)
levelplot(tmp_slice ~ lon * lat, data=grid, at=cutpts, cuts=11, pretty=T, 
          col.regions=(rev(brewer.pal(10,"RdBu"))))

