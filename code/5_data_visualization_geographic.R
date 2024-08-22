suppressMessages(library(tidyverse))
suppressMessages(library(sf))
suppressMessages(library(paletteer))
suppressMessages(library(patchwork))
suppressMessages(library("rnaturalearth"))
#library(viridis)

# identify and plot highest year(s)

plot_decades <- c(1990,2000,2010,2020)


# rnaturalearth data

coastline_sf <- ne_coastline(scale="medium")
graticules_sf <- st_graticule()
mollweide_crs <- "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m" 

# read transformed data
data_dir <- file.path("data", "2-transformed-data", "geographic")

# continents
file_path <- file.path(data_dir, "continents_sf.rds")
continents_sf <- read_rds(file_path)

# yearly
file_path <- file.path(data_dir, "yearly_sf.rds")
yearly_sf <- read_rds(file_path) 

# # decade
file_path <- file.path(data_dir, "decade_sf.rds")
decade_sf <- read_rds(file_path) 

# calculate temperature limts for colorbar

decade_max_value <- max(decade_sf$avg_anomaly, na.rm=TRUE) 
decade_min_value <- min(decade_sf$avg_anomaly, na.rm=TRUE)
decade_colorbar_limits <- max(abs(decade_max_value),abs(decade_min_value))* c(-1, 1)
decade_colorbar_limits

yearly_max_value <- max(yearly_sf$avg_anomaly, na.rm=TRUE) 
yearly_min_value <- min(yearly_sf$avg_anomaly, na.rm=TRUE)
yearly_colorbar_limits <- max(abs(yearly_max_value),abs(yearly_min_value))* c(-1, 1)
yearly_colorbar_limits

#plot_theme <- "ggthemes::Red-Blue-White Diverging"
plot_theme <- "pals::warmcool"

# grid cell average, filter single decade
plot_decade = 2020

plot_data <- decade_sf |> filter(decade == plot_decade)
plot_max_value <- max(plot_data$avg_anomaly, na.rm=TRUE) 
plot_min_value <- min(plot_data$avg_anomaly, na.rm=TRUE)
plot_colorbar_limits <- max(abs(plot_max_value),abs(plot_min_value))* c(-1, 1)
ggplot() +
  geom_sf(data=plot_data , aes(color=avg_anomaly)) +
  geom_sf(data=graticules_sf, color = "black", linewidth=0.05) +
  geom_sf(data = coastline_sf,  color="black", linewidth=0.1)+
  coord_sf(crs=mollweide_crs) +
  labs(
    title = paste("Average Temperature Anomaly for Decade Ending ", plot_decade),
    subtitle = "Compared with Climate Period 1951-1980",
    caption = "Source: NASA Goddard Institute for Space Studies\nhttps://data.giss.nasa.gov/gistemp/",
    color = "°C")  +
  scale_color_paletteer_c(plot_theme,
                          direction = -1,
                          limits = plot_colorbar_limits)+
  theme_void()

# continents average, simple avg, filter single decade
plot_data <- continents_sf |> filter(decade==plot_decade)
plot_max_value <- max(plot_data$avg_anomaly) 
plot_min_value <- min(plot_data$avg_anomaly)
plot_colorbar_limits <- max(abs(plot_max_value),abs(plot_min_value))* c(-1, 1)
plot_colorbar_limits
ggplot() +
  geom_sf(data=graticules_sf, color = "lightgrey") +
  geom_sf(data=plot_data, aes(fill=avg_anomaly), alpha=0.9) +
  geom_sf(data = coastline_sf,  color="black", linewidth=0.1) +
  coord_sf(crs=mollweide_crs) +
  labs(
    title=paste("Average Temperature Anomaly by Continent for Decade Ending", plot_decade),
    subtitle = "Compared with Climate Period 1951-1980",
    caption = "Source: NASA Goddard Institute for Space Studies\nhttps://data.giss.nasa.gov/gistemp/",
    fill = "°C")  +
  scale_fill_paletteer_c(plot_theme, 
                         direction = -1,
                         limits = plot_colorbar_limits)+
  theme_void()

# last 4 decades
plot_data_list <- list()
for (i in 1:4) {
  plot_data <- decade_sf |> filter(decade == plot_decades[i])
  p <- ggplot() + 
    geom_sf(data=plot_data , aes(color=avg_anomaly)) +
    geom_sf(data = coastline_sf,  color="black", linewidth=0.1) +
    coord_sf(crs=mollweide_crs) +
    labs(subtitle=paste("Decade ending",plot_decades[i]), color = "°C")  +
    scale_color_paletteer_c(plot_theme, 
                            direction = -1,
                            limits = decade_colorbar_limits) +
    theme_void()
  plot_data_list[[i]] <- p
}

plot_data_list[[1]] + plot_data_list[[2]] + plot_data_list[[3]] + plot_data_list[[4]] +
  plot_layout(guides = 'collect') +
  plot_annotation(
    title = "Average Temperature Anomaly by Decade",
    subtitle = "Compared with Climate Period 1951-1980",
    caption = "Source: NASA Goddard Institute for Space Studies\nhttps://data.giss.nasa.gov/gistemp/")



# # filter single year
# plot_year = 2024
# plot_year_sf <- yearly_sf |> filter(year == plot_year)
# 
# p <- ggplot() + 
#   geom_sf(data=plot_year_sf , aes(color=avg_anomaly)) +
#   geom_sf(data = coastline_sf,  color="black", linewidth=0.1) +
#   theme_void()
# p <- p +
#   labs(
#     title = paste("Average Temperature Anomaly for ", plot_year),
#     subtitle = "Compared with Climate Period 1951-1980",
#     color = "°C")  +
#   scale_color_paletteer_c(plot_theme, 
#                           direction = -1,
#                           limits = yearly_colorbar_limits)
# p



# continents last 4 decades
plot_data_list <- list()
for (i in 1:4) {
  plot_data <- continents_sf |> filter(decade == plot_decades[i])
  p <- ggplot() + 
    geom_sf(data=plot_data , aes(fill=avg_anomaly)) +
    geom_sf(data = coastline_sf,  color="black", linewidth=0.1) +
    coord_sf(crs=mollweide_crs) +
    labs(subtitle=paste("Decade ending",plot_decades[i]), fill = "°C")  +
    scale_fill_paletteer_c(plot_theme, 
                            direction = -1,
                            limits = plot_colorbar_limits) 
  plot_data_list[[i]] <- p
}
plot_data_list[[1]] + plot_data_list[[2]] + plot_data_list[[3]] + plot_data_list[[4]] +
  plot_layout(guides = 'collect') +
  plot_annotation(
    title = "Continent Average Temperature Anomaly by Decade",
    subtitle = "Compared with Climate Period 1951-1980",
    caption = "Source: NASA Goddard Institute for Space Studies\nhttps://data.giss.nasa.gov/gistemp/")
