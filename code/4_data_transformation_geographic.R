# libraries
suppressMessages(library(tidyverse))
library(ncdf4)
suppressMessages(library(sf)) 
suppressMessages(library("rnaturalearth"))

# variables
continents <-  c("Africa", "Antartica", "Asia", "Europe",
                 "North America", "Oceania", "South America")
mollweide_crs <- "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m" 

# functions
fn_create_grid_polygon <- 
  function(nelat, nelng, swlat, swlng) {
    m <- matrix(c(swlng, nelng, nelng, swlng, swlng,
                  swlat, swlat, nelat, nelat, swlat), nrow = 5)
    return(st_polygon(list(m)))
  } 

# use rnaturalearth country polygon to build continent multipolygons
countries_sf <- ne_countries()
colnames(countries_sf)
continents_sf <- countries_sf |> 
  select(continent) |> 
  filter(continent %in% continents) |> 
  st_transform( "+proj=eqc") |> # convert from spherical geometry
  group_by(continent) |> summarize()
continents_sf

# project directories
raw_data_dir <- file.path("data", "1-raw-data", "geographic")
transformed_data_dir <- file.path("data", "2-transformed-data", "geographic")

# remove previous files
f <- list.files(transformed_data_dir, full.names=TRUE)
status <- file.remove(f)

# netcdf file path
file_name_nc = "gistemp1200_GHCNv4_ERSSTv5.nc"
netcdf_path <- file.path("data", "1-raw-data", "geographic", file_name_nc)

# read netcdf
nc_data <- ncdf4::nc_open(netcdf_path)
vname <- "tempanomaly"
fillvalue <- ncatt_get(nc_data,vname,"_FillValue")

# extract variables
lon <- ncvar_get(nc_data,"lon")
lat <- ncvar_get(nc_data,"lat")
time <- ncvar_get(nc_data,"time")
values <- ncvar_get(nc_data, vname)
values1980 = values[,,1201:1735]

# close netcdf file
nc_close(nc_data)

# replace netCDF fill values with NA's
values[values==fillvalue$value] <- NA

# convert time variable to dates
dates = as.Date(time, origin = '1800-01-01')
length(dates)
dates1980 = dates[dates>="1980-01-15"]
dates1980
length(dates1980)

# dimensions
nlon <- dim(lon) # 180
nlat <- dim(lat) # 90
ndate = dim(dates1980) # 535

# convert values to single long vector
values_vector <- as.vector(values1980)

# create matrix of lon, lat, and dates
lon_lat_date_matrix <- matrix(NA, nrow = length(values_vector), ncol = 3)
lon_lat_date_matrix <- as.matrix(expand.grid(lon,lat,dates1980))

# build data frame using lon/lat/date matrix and values vector
df <- data.frame(cbind(lon_lat_date_matrix, values_vector))
names(df) <- c("lon", "lat", "date", "anomaly")

# transform df to have month, year, decade columns
#   set data types for all columns
#   filter for year > 1980
monthly_df <- df |>
  mutate(lon = as.integer(lon)) |>
  mutate(lat = as.integer(lat)) |>
  mutate(month = as.integer(str_sub(date,6,7))) |> 
  mutate(year = as.integer(str_sub(date,1,4))) |> 
  mutate(decade = as.integer(year - (year %% 10) + 10)) |>
  mutate(anomaly = as.numeric(anomaly)) |>
  filter(year>1980) |>
  select(lon,lat,month,year,decade,anomaly)

# group into new yearly and decade dfs
yearly_df <- monthly_df |>
  group_by(lon, lat, year) |>
  summarize(avg_anomaly = mean(anomaly))
glimpse(yearly_df) #2332800

decade_df <- monthly_df |>
  group_by(lon, lat, decade) |>
  summarize(avg_anomaly = mean(anomaly))
glimpse(decade_df) #243000

# convert yearly and decade data frames to points spatial data frames
yearly_sf <-
  st_as_sf(yearly_df, coords = c('lon', 'lat'), crs =4326)
decade_sf <- 
  st_as_sf(decade_df, coords = c('lon', 'lat'), crs =4326)

# change to equal area (mollweide) projection
yearly_equal_area_sf <- st_transform(yearly_sf, crs=mollweide_crs)
decade_equal_area_sf <- st_transform(decade_sf, crs=mollweide_crs)
continents_equal_area_sf <- st_transform(continents_sf, crs=mollweide_crs)

# calculate grid polygons for decade df
#. (for use in calculating continent weighted average anomalies)
decade_ungroup_df <- ungroup(decade_df)
decade_grid_df <- decade_ungroup_df %>%
  mutate(nelat=lat+1,
         nelng=lon+1,
         swlat=lat-1,
         swlng=lon-1)  |>
  select(decade,avg_anomaly,nelat,nelng,swlat,swlng)

decade_grid_sf <- decade_grid_df |>
  rowwise() |>
  mutate(geometry = 
           list(fn_create_grid_polygon(nelat, nelng, swlat, swlng))) |>
  st_as_sf(sf_column_name = "geometry", crs =4326) |>
  select(decade ,avg_anomaly)
glimpse(decade_grid_sf)

decade_grid_equal_area_sf <- st_transform(decade_grid_sf, crs=mollweide_crs)
decade_grid_equal_area_sf <- decade_grid_equal_area_sf|> drop_na()
decade_grid_equal_area_sf$area_sqkm = 
  as.numeric(sf::st_area(decade_grid_equal_area_sf) / 1000000)

# spatial join decade area data with continents
decade_joined_equal_area_sf = st_join(decade_grid_equal_area_sf,
                                      continents_equal_area_sf) |> drop_na()

# calculate continent weighted average temperature anomaly
continent_area_sf = decade_joined_equal_area_sf |>
  group_by(continent,decade) |>
  summarize(continent_total_area = sum(area_sqkm),
            continent_total_anomaly = sum(avg_anomaly*area_sqkm)) |>
  mutate(avg_anomaly = continent_total_anomaly/continent_total_area )  

# replace point geometry with multipolygon geometry from rnaturalearth
continents_area_sf_drop_geom <- st_drop_geometry(continent_area_sf)
continents_area_weighted_avg_sf <- st_as_sf(continents_area_sf_drop_geom |>
                                              left_join(continents_sf, by="continent"))

# write month, year, decade, and continents spatial data frames
rds_file_path <- file.path(transformed_data_dir,"yearly_sf.rds")
write_rds(yearly_equal_area_sf, rds_file_path)

rds_file_path <- file.path(transformed_data_dir,"decade_sf.rds")
write_rds(decade_equal_area_sf, rds_file_path)

rds_file_path <- file.path(transformed_data_dir,"continents_sf.rds")
write_rds(continents_area_weighted_avg_sf,  rds_file_path)
