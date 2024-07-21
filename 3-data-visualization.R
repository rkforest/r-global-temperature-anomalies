# to do:
# - create boxplot using climate period 30 year averages column in global_monthly_df
# - create bar plot 
# - create a facet plot
# - create bar chart using geom_col
# - save plots using ggsave

library(tidyverse)
library(RColorBrewer)
library(viridis)

data_dir <- file.path("data", "rds")

# read global annual rda file
file_path <- file.path(data_dir, "global_annual.rds")
global_annual_df <- read_rds(file_path) 
glimpse(global_annual_df)
levels(global_annual_df$Id)

# read global monthly rda file
file_path <- file.path(data_dir, "global_monthly.rds")
global_monthly_df <- read_rds(file_path) 
glimpse(global_monthly_df)
levels(global_monthly_df$Id)

# global monthly scatter plot
plot_data <- global_monthly_df |> filter(Id == "GLB")

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

# global monthly histogram last 30 years
plot_data <- global_monthly_df |> filter(Id == "GLB") |> filter(Year > 1993)
ggplot(plot_data,
       aes(x=Anomaly)) +
       geom_histogram(binwidth=0.1,color="darkblue",fill="lightblue") +
  labs(
    title = "Global Temperature Anomalies",
    subtitle = "Histogram of Monthly Values\nMost recent 30 years",
  ) 


# hemisphere plots: last 10 years, boxplot and density plot
plot_data <- global_annual_df |> filter(Id %in% c("NH","SH")) |> filter(Year > 2013)

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
  



       