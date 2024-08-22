suppressMessages(library(tidyverse))
suppressMessages(library(viridis))
suppressMessages(library(paletteer))
suppressMessages(library(patchwork))

# plot parameters
plot_caption <- "Data Source: NASA Goddard Institute for Space Studies\nhttps://data.giss.nasa.gov/gistemp/"
plot_transparency <- 0.9

scale_option <- "D"
scale_direction <- -1
scale_beg <- 0.2
scale_end <- 0.7

# directory paths

data_dir <- file.path("data", "2-transformed-data", "tabular")
output_dir <- file.path("output")

# read data

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

p1 <- ggplot(global_monthly_df, aes(x=year, y = anomaly)) +
        geom_point(aes(color = anomaly)) +
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
  ggplot(aes(x = year, y = avg_anomaly)) +
    geom_line(aes(color = avg_anomaly),
            linewidth=0.8,
            show.legend = FALSE) +
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

p_save_1 <- p1 + p2 + p3 +
  plot_layout(design = layout) +
  plot_layout(guides = 'collect') +
  plot_annotation(
    title = "Global Average Temperature Anomalies\n1881-2024",
    caption = plot_caption)

p_save_1 

# by climate period plots

p4<- global_monthly_df |>
     ggplot(aes(x=anomaly,fill=climate)) +
       geom_area(stat="bin",
                 alpha=plot_transparency,
                 color="grey40") +
       labs(subtitle = "Frequency distribution",
            x = "Temperature Anomaly (°C)",
            y = "Count",
            fill = "Climate Period") +
       scale_fill_viridis(option=scale_option,
                          begin=scale_beg,
                          end=scale_end,
                          direction=scale_direction,
                          discrete=TRUE)

p5 <- global_monthly_df |>
  ggplot(aes(x=anomaly,fill=climate)) +
  geom_histogram(binwidth = 0.1,
                 alpha = plot_transparency,
                 color="grey30") +
  labs(subtitle = "Histogram (binwidth 0.1 degrees)",
       x = "Temperature Anomaly (°C)",
       y = "Count",
       fill = "Climate Period") +
  scale_fill_viridis(option=scale_option,
                     begin=scale_beg,
                     end=scale_end,
                     direction=scale_direction,
                     discrete=TRUE)

p6 <- global_monthly_df |> 
      ggplot(aes(x = climate, y = anomaly, fill=climate)) +
        geom_boxplot(alpha = plot_transparency,
                     show.legend = FALSE) +
        labs(subtitle = "Median, Interquartile Range, and Outliers",
             x = "Climate",
             y = "Anomaly °C",
             fill = "Climate") +
  scale_fill_viridis(option=scale_option,
                     begin=scale_beg,
                     end=scale_end,
                     direction=scale_direction,
                     discrete=TRUE) +
  theme(axis.title.x=element_blank()) 

layout <- "
ABB
ABB
CCC
CCC
CCC
"

p_save_2 <- p4 + p5 + p6 +  
  plot_layout(design = layout) +
  plot_layout(guides = 'collect') +
  plot_annotation(
    title = "Global Average Temperature Anomalies by Climate Period",
    caption = plot_caption) 

p_save_2

# geographic distribution (latest climate period)

# hemisphere 
p7 <- hemisphere_monthly_df  |> 
       filter(year > 2010)  |> 
       group_by(hemisphere) |>
       summarize(avg_anomaly=mean(anomaly)) |>
       ggplot(aes(x = hemisphere, y = avg_anomaly, fill = hemisphere)) +
       geom_col(alpha = plot_transparency,
                show.legend = FALSE) +
       labs(subtitle = "By Hemisphere",
            y = "Anomaly °C",
            fill = "Hemisphere") +
  scale_fill_viridis(option=scale_option,
                     begin = scale_beg,
                     end = scale_end,
                     direction = scale_direction,
                     discrete =TRUE) +
  theme(axis.title.x=element_blank())  

# 3 zone 
p8 <-zonal_3x_annual_df  |> 
     filter(year > 2010)  |> 
     group_by(zone) |>
     summarize(avg_anomaly=mean(anomaly)) |>
     ggplot(aes(x = zone, y = avg_anomaly, fill = zone),show.legend = FALSE) +
       geom_col(alpha = plot_transparency, 
                show.legend = FALSE) +
       labs(subtitle = "By Latitude Zones (3 zones)",
            fill = "Zone") +
  scale_fill_viridis(option=scale_option ,
                     begin = scale_beg,
                     end = scale_end,
                     direction = scale_direction,
                     discrete = TRUE) +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(), 
        axis.text.y=element_blank(), 
        axis.ticks.y=element_blank())

#  8 zone 
p9 <- zonal_8x_annual_df  |> 
      filter(year > 2010)  |> 
      group_by(zone) |>
      summarize(avg_anomaly=mean(anomaly)) |>
      ggplot(aes(x = zone, y = avg_anomaly, fill=zone)) +
        geom_col(alpha = plot_transparency,
                 show.legend = FALSE) +
        labs(subtitle = "By Latitude Zones (8 zones)",
             y = "Anomaly °C",
             fill = "Zone") +
        scale_fill_viridis(option=scale_option ,
                     begin = scale_beg,
                     end = scale_end,
                     direction = scale_direction,
                     discrete = TRUE) +
        theme(axis.title.x=element_blank()) 

layout <- "
ABB
CCC
"

p_save_3 <- p7 + p8 + p9 +
  plot_layout(design = layout) +
  plot_annotation(
    title = "Temperature Anomalies by Geographic Region",
    subtitle = "2011-2024",
    caption = plot_caption) 

p_save_3     

# bar plot by Hemisphere
p10 <- hemisphere_monthly_df  |>
  group_by(season) |>
  summarize(avg_anomaly = mean(anomaly)) |>
  ggplot(aes(x = season, y = avg_anomaly, fill=avg_anomaly)) +
  geom_col(alpha = plot_transparency,
           show.legend = FALSE
           ) +
  #ylim(y_limits ) +
  labs(subtitle = "By Season",
       y = "Anomaly",
       fill = "Climate")  +
  scale_fill_viridis(option=scale_option,
                     begin=scale_beg,
                     end=scale_end,
                     direction=scale_direction,
                     discrete=FALSE) +
  theme(axis.title.x=element_blank())

# bar plot by Month
p11 <- global_monthly_df |>
  group_by(month) |>
  summarize(avg_anomaly = mean(anomaly)) |>
  ggplot(aes(x = month, y = avg_anomaly, fill=avg_anomaly)) +
  geom_col(alpha = plot_transparency,
           show.legend = FALSE) +
  labs(subtitle = "By Month") +
  scale_fill_viridis(option=scale_option,
                     begin=scale_beg,
                     end=scale_end,
                     direction=scale_direction,
                     discrete=FALSE) +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(), 
        axis.text.y=element_blank(), 
        axis.ticks.y=element_blank())

p_save_4 <- p10 + p11 +
  plot_layout(design = layout) +
  plot_annotation(
    title = "Global Average Temperature Anomalies by Season and Month",
    #subtitle = "2011-2024",
    caption = plot_caption) 

p_save_4




# save plots

output_file_name <- file.path(output_dir,"1-time-series.png")
ggsave(filename = output_file_name, plot = p_save_1)
output_file_name <- file.path(output_dir,"2-climate_period.png")
ggsave(filename = output_file_name, plot = p_save_2)
output_file_name <- file.path(output_dir,"3-geographic-region.png")
ggsave(filename = output_file_name, plot = p_save_3)
output_file_name <- file.path(output_dir,"4-seasons-months.png")
ggsave(filename = output_file_name, plot = p_save_4)


