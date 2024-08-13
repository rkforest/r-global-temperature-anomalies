suppressMessages(library(tidyverse))
suppressMessages(library(sf))
suppressMessages(library("rnaturalearth"))
suppressMessages(library(paletteer))
suppressMessages(library(patchwork))

climate_periods <- c("1891-1920", "1921-1950", "1951-1980",
                     "1981-2010", "2011-2024")

# read global coastline
world_sf <- ne_coastline(scale = "medium", returnclass = "sf")
class(world_sf)

# read temperature anomaly data
data_dir <- file.path("data", "2-transformed-data", "gridded")
# by decade
file_path <- file.path(data_dir, "global_by_decade_df.rds")
global_by_decade_df <- read_rds(file_path) 
glimpse(global_by_decade_df)
# by climate period
file_path <- file.path(data_dir, "global_by_climate_period_df.rds")
global_by_climate_period_df <- read_rds(file_path) 
glimpse(global_by_climate_period_df)

# convert temperature anomaly data to sf
decade_sf <- st_as_sf(global_by_decade_df, coords = c('lon', 'lat'), crs =4326)
climate_sf <- st_as_sf(global_by_climate_period_df, coords = c('lon', 'lat'), crs =4326)

# calculate temperature limts for colorbar
decade_limit <- max(abs(decade_sf$avg_anomaly)) * c(-1, 1)
climate_limit <- max(abs(climate_sf$avg_anomaly)) * c(-1, 1)

# change to robinson projection
crs <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m"
decade_sf_robinson <- st_transform(decade_sf, crs=crs)
climate_sf_robinson <- st_transform(climate_sf, crs=crs)
world_sf_robinson <- st_transform(world_sf, crs=crs)

# create climate period plots
plot_data_list <- list()
for (i in 1:length(climate_periods)) {
  plot_data <- climate_sf_robinson |> filter(climate == climate_periods[i])
  p <- ggplot() + 
    geom_sf(data=plot_data , aes(color=avg_anomaly)) +
    geom_sf(data = world_sf_robinson,  color="black") +
    theme_void()
  p <- p +
    labs(subtitle=climate_periods[i], color = "°C")  +
    scale_color_paletteer_c("ggthemes::Red-Blue-White Diverging", 
                            direction = -1,
                            limits = climate_limit)
  plot_data_list[[i]] <- p
}

plot_data_list[[1]] + plot_data_list[[2]] + plot_data_list[[4]] + plot_data_list[[5]] +
  plot_layout(guides = 'collect') +
  plot_annotation(
    title = "Average Temperature Anomaly for 30 Year Climate Periods",
    subtitle = "Compared with Climate Period 1951-1980",
    caption = "Source: NASA Goddard Institute for Space Studies\nhttps://data.giss.nasa.gov/gistemp/")


# filter decades
#decade_2020_sf_robinson <- decade_sf_robinson |> filter(decade == 2020)


# p1_decade <- ggplot() + 
#   geom_sf(data=decade_2020_sf_robinson,aes(color=avg_anomaly)) +
#   geom_sf(data = world_sf_robinson,  color="black") +
#   theme_void()