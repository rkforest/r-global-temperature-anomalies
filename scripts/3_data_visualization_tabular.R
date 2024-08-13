library(tidyverse)
library(ggridges)

library(viridis)
suppressMessages(library(paletteer))
suppressMessages(library(patchwork))

plot_caption <- "Data Source: NASA Goddard Institute for Space Studies\nhttps://data.giss.nasa.gov/gistemp/"


# read data

data_dir <- file.path("data", "2-transformed-data", "tabular")

file_path <- file.path(data_dir, "global_monthly_df.rds")
global_monthly_df <- read_rds(file_path) 
file_path <- file.path(data_dir, "hemisphere_monthly_df.rds")
hemisphere_monthly_df <- read_rds(file_path) 
file_path <- file.path(data_dir, "zonal_3x_annual_df.rds")
zonal_3x_annual_df <- read_rds(file_path) 
file_path <- file.path(data_dir, "zonal_8x_annual_df.rds")
zonal_8x_annual_df <- read_rds(file_path) 

# time series plots

palette <- "pals::coolwarm"
y_limits <- c(min(global_monthly_df$anomaly),
              max(global_monthly_df$anomaly))
layout <- "
AAAAA
AAAAA
BBBCC
"

p1 <- ggplot(global_monthly_df, aes(x=year, y = anomaly, color = anomaly)) +
       geom_jitter() +
       ylim(y_limits ) +
       scale_color_paletteer_c(palette=palette, direction = 1) +
       geom_smooth(method = "lm") +
       labs(subtitle = "By Month",
            y = "Anomaly",
            color = "Anomaly\n°C")  +
       theme(axis.title.x=element_blank()) 

p2 <- global_monthly_df |> 
  group_by(year) |> 
  summarize(avg_anomaly = mean(anomaly)) |>
  ggplot(aes(x = year, y = avg_anomaly, color = avg_anomaly)) +
  geom_line(linewidth=0.8,show.legend = FALSE) +
  ylim(y_limits ) +
  scale_color_paletteer_c(palette=palette, direction = 1) + 
  labs(subtitle = "By Year",
       y = "Anomaly",
       color = "Anomaly\n°C") +
  theme(axis.title.x=element_blank())

p3 <- global_monthly_df |> 
      group_by(decade) |> 
      summarize(avg_anomaly = mean(anomaly)) |>
      ggplot(aes(x = decade, y = avg_anomaly, color = avg_anomaly)) +
       geom_line(linewidth=0.8,show.legend = FALSE) +
       geom_point(size=1.5,show.legend = FALSE) +
       ylim(y_limits ) +
       scale_color_paletteer_c(palette=palette, direction = 1) + 
       labs(subtitle = "By Decade",
            color = "Anomaly\n°C") +
       theme(axis.title.x=element_blank(),
             axis.title.y=element_blank())

p1 + p2 + p3 +
  plot_layout(design = layout) +
  plot_layout(guides = 'collect') +
  plot_annotation(
    title = "Global Average Temperature Anomalies\n1881-2024",
    caption = plot_caption)



# attribute plots
scale_beg <- 0.3
scale_end <- 0.7

p4<- global_monthly_df |>
     ggplot(aes(x=anomaly,fill=climate)) +
       geom_area(stat="bin",alpha=0.7, color="grey40") +
       labs(subtitle = "Frequency distribution",
            x = "Temperature Anomaly (°C)",
            y = "Count",
            fill = "Climate Period") +
       scale_fill_viridis(option="G",
                          begin=scale_beg,
                          end=scale_end,
                          direction=1,
                          discrete=TRUE)

p5 <- global_monthly_df |>
  ggplot(aes(x=anomaly,fill=climate)) +
  geom_histogram(binwidth=0.1, alpha=0.7, color="grey40") +
  labs(subtitle = "Histogram (binwidth 0.1 degrees)",
       x = "Temperature Anomaly (°C)",
       y = "Count",
       fill = "Climate Period") +
  scale_fill_viridis(option="G",
                     begin=scale_beg,
                     end=scale_end,
                     direction=1,
                     discrete=TRUE)

p6 <- global_monthly_df |> 
      ggplot(aes(x = climate, y = anomaly, fill=climate)) +
        geom_boxplot(alpha=0.7, show.legend = FALSE) +
        labs(subtitle = "Median, Interquartile Range, and Outliers",
             x = "Climate",
             y = "Anomaly °C",
             fill = "Climate") +
  scale_fill_viridis(option="G",
                     begin=scale_beg,
                     end=scale_end,
                     direction=1,
                     discrete=TRUE) +
  theme(axis.title.x=element_blank()) 


# layout <- "
# AAACCCCC
# BBBCCCCC
# "
layout <- "
ABB
ABB
CCC
CCC
CCC
"
p4 + p5 + p6 +  
  plot_layout(design = layout) +
  plot_layout(guides = 'collect') +
  plot_annotation(
    title = "Global Average Temperature Anomalies by Climate Period",
    caption = plot_caption)

# geographic distribution (latest climate period)

scale_beg <- 0.15
scale_end <- 0.85

viridis_option = "B"
scale_direction= -1

# hemisphere 
p7 <- hemisphere_monthly_df  |> 
       filter(year > 2010)  |> 
       ggplot(aes(x = hemisphere, y = anomaly, fill = hemisphere)) +
       geom_col(alpha=0.7, show.legend = FALSE) +
       labs(subtitle = "By Hemisphere",
            y = "Anomaly °C",
            fill = "Hemisphere") +
  scale_fill_viridis(option=viridis_option,
                     begin = scale_beg,
                     end = scale_end,
                     direction = scale_direction,
                      discrete =TRUE) +
  theme(axis.title.x=element_blank())  

# 3 zone 
plot_data <- zonal_3x_annual_df  |> 
  filter(year > 2010)
p8 <- ggplot(plot_data,
       aes(x = zone, y = anomaly, fill = zone)) +
       geom_col(alpha=0.7,show.legend = FALSE) +
       labs(subtitle = "By Latitude Zones (3 zones)",
            fill = "Zone") +
  scale_fill_viridis(option=viridis_option ,
                     begin = scale_beg,
                     end = scale_end,
                     direction = scale_direction,
                     discrete = TRUE) +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(), 
        axis.text.y=element_blank(), 
        axis.ticks.y=element_blank())

#  8 zone 
plot_data <- zonal_8x_annual_df  |> 
  filter(year > 2010) 
p9 <- ggplot(plot_data,
            aes(x = zone, y = anomaly, fill=zone)) +
  geom_col(alpha=0.7, show.legend = FALSE) +
  labs(subtitle = "By Latitude Zones (8 zones)",
       y = "Anomaly °C",
       fill = "Zone") +
  scale_fill_viridis(option=viridis_option ,
                     begin = scale_beg,
                     end = scale_end,
                     direction = scale_direction,
                     discrete = TRUE) +
  theme(axis.title.x=element_blank()) 

layout <- "
ABB
CCC
"

p7 + p8 + p9 +
  plot_layout(design = layout) +
  plot_annotation(
    title = "Distribution of Temperature Anomalies by Geographic Region",
    subtitle = "2011-2024",
    caption = plot_caption)


# bar plot by Hemisphere
p10 <- hemisphere_monthly_df  |>
  group_by(season) |>
  summarize(avg_anomaly = mean(anomaly)) |>
  ggplot(aes(x = season, y = avg_anomaly, fill=avg_anomaly)) +
  geom_col(show.legend=FALSE, alpha=0.7) +
  #ylim(y_limits ) +
  labs(subtitle = "By Season",
       y = "Anomaly",
       fill = "Climate")  +
  scale_fill_viridis(option="A",
                     begin=scale_beg,
                     end=scale_end,
                     direction=-1,
                     discrete=FALSE) 

# bar plot by Month
p11 <- global_monthly_df |>
  group_by(month) |>
  summarize(avg_anomaly = mean(anomaly)) |>
  ggplot(aes(x = month, y = avg_anomaly, fill=avg_anomaly)) +
  geom_col(show.legend=FALSE, alpha=0.7) +
  labs(subtitle = "By Month") +
  scale_fill_viridis(option="A",
                     begin=scale_beg,
                     end=scale_end,
                     direction=-1,
                     discrete=FALSE) +
theme(axis.title.x=element_blank(),
      axis.title.y=element_blank(), 
      axis.text.y=element_blank(), 
      axis.ticks.y=element_blank())

layout <- "
AAABBBBB
xxxxxxxx
"
p10 + p11 +
  plot_layout(design = layout) +
  plot_annotation(
    title = "Distribution of Temperature Anomalies by Date Period",
    #subtitle = "2011-2024",
    caption = plot_caption)
