# to do:
# - create boxplot using climate period 30 year averages column in global_monthly_df
# - create bar plot 
# - create a facet plot
# - create bar chart using geom_col
# - save plots using ggsave

library(tidyverse)
library(RColorBrewer)
library(viridis)

data_dir <- file.path("data", "2-transformed-data")

# read global monthly rds file
file_path <- file.path(data_dir, "global_monthly_df.rds")
global_monthly_df <- read_rds(file_path) 
glimpse(global_monthly_df)
tail(global_monthly_df)

# global monthly scatter plot
plot_data <- global_monthly_df |> filter(Area == "G")

ggplot(plot_data,
       aes(x=Year, y = Anomaly)) +
  geom_point(aes(color = Anomaly)) +
  scale_color_viridis(option="A",
                      begin=0.2,
                      end=0.8,
                      direction=1) +
  geom_smooth(method = "lm") +
  labs(
    title = "Global Temperature Anomalies",
    subtitle = "Monthly Values\n1880-2023",
    y = "Temperature Anomaly (°C)",
    color = "Temperature\nAnomaly\n°C"
  ) 

# global monthly histogram by 30 year climate period
plot_data <- global_monthly_df |> filter(Area == "G") 
ggplot(plot_data,
       aes(x=Anomaly,fill=Climate)) +
       geom_histogram(binwidth=0.1, alpha=0.5) +
  labs(
    title = "Global Temperature Anomalies",
    subtitle = "Monthly Anomalies by Climate Period",
    x = "Temperature Anomaly (°C)",
    y = "Count",
    fill = "30 Year\nClimate\nPeriod"
  ) 

# hemisphere plots: last 10 years, boxplot and density plot
plot_data <- global_annual_df |> filter(Id %in% c("N","S")) |> filter(Year > 2013)

ggplot(plot_data,
       aes(x = Id, y = Anomaly, color = Id)) +
       geom_boxplot() +
       coord_flip() +
       labs(
         title = "Global Temperature Anomalies by Hemisphere",
         subtitle = "Boxplot of Annual Values 2014-2023 (10 years)",
         y = "Temperature Anomaly (°C)",
         color = "Hemisphere"
       )  

ggplot(plot_data,
       aes(x = Anomaly,color = Id, fill=  Id)) +
  geom_density(alpha=0.5) +
  labs(
    title = "Global Temperature Anomalies by Hemisphere",
    subtitle = "Density Plot of Annual Values 2014-2023 (10 years)",
    y = "Temperature Anomaly (°C)",
    color = "Hemisphere"
  )  
  



       