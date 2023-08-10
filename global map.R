
library(sf)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(cowplot)
library(readxl)
library(rgdal)
library(rnaturalearth)
selected_cancer <- c(1)
avg_cancer_data <- data_male_all%>%
  filter(cancer_code %in% selected_cancer)%>%
  group_by(GEOID) %>%
  summarize(
    latitude = mean(Latitude, na.rm = TRUE),
    longitude = mean(Longitude, na.rm = TRUE),
    env_var_NDVI = mean(NDVI/10, na.rm = TRUE),
    env_var_temp = mean(skin_temperature, na.rm = TRUE),
    env_var_pre = mean(Precipitation, na.rm = TRUE),
    env_var_ozone = mean(Ozone*10, na.rm = TRUE),
    env_var_solar = mean(Solar_radiation, na.rm = TRUE),
  )
avg_cancer_data_sf <- st_as_sf(avg_cancer_data, coords = c("longitude", "latitude"), crs = 4326)
world_map <- st_read('C:/Haowen Wang/Cancer and climate change/SEER data/ne_10m_admin_0_countries.geojson')

map_plot_NDVI <- ggplot() +
  geom_sf(data = world_map, fill = "gray90", color = "gray70", size = 0.2) +
  geom_sf(data = avg_cancer_data_sf, aes(color = env_var_NDVI), size = 1.5, alpha = 0.8) +
  scale_color_gradientn(colors = c("#F4A460", "#3CB371"),  # Use darker shades of yellow and green
                        values = scales::rescale(c(0, max(avg_cancer_data_sf$env_var_NDVI))),
                        limits = c(0, max(avg_cancer_data_sf$env_var_NDVI)),
                        name = "Average annual NDVI") +
  scale_size_continuous(range = c(1, 6), name = "Average annual NDVI") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(title = "Distribution of average annual NDVI across cancer registries")


map_plot_ozone <- ggplot() +
  geom_sf(data = world_map, fill = "gray90", color = "gray70", size = 0.2) +
  geom_sf(data = avg_cancer_data_sf, aes(color = env_var_ozone), size = 1.5, alpha = 0.8) +
  scale_color_gradientn(colors = c( "#FFBB66","#33FFFF"),  # Use darker shades of yellow and green
                        values = scales::rescale(c(min(avg_cancer_data_sf$env_var_ozone), max(avg_cancer_data_sf$env_var_ozone))),
                        limits = c(min(avg_cancer_data_sf$env_var_ozone), max(avg_cancer_data_sf$env_var_ozone)),
                        name = "Average total ozone concentration/DU") +
  scale_size_continuous(range = c(1, 6), name = "Average total ozone concentration/DU") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(title = "Distribution of annual average total ozone concentration across cancer registries")


map_plot_temp <- ggplot() +
  geom_sf(data = world_map, fill = "gray90", color = "gray70", size = 0.2) +
  geom_sf(data = avg_cancer_data_sf, aes(color = env_var_temp),size = 1.5,alpha = 0.8) +
  scale_color_gradientn(colors = c("#66CDAA","#FF4500"),  # Use darker shades of yellow and green
                        values = scales::rescale(c(min(avg_cancer_data_sf$env_var_temp), max(avg_cancer_data_sf$env_var_temp))),
                        limits = c(min(avg_cancer_data_sf$env_var_temp), max(avg_cancer_data_sf$env_var_temp)),
                        name = "Average skin temperature/K") +
  scale_size_continuous(range = c(1, 6), name = "Average skin temperature/K") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(title = "Distribution of annuanl average skin temperature across cancer registries")



map_plot_pre <- ggplot() +
  geom_sf(data = world_map, fill = "gray90", color = "gray70", size = 0.2) +
  geom_sf(data = avg_cancer_data_sf, aes(color = env_var_pre),size = 1.5,alpha = 0.8) +
  scale_color_gradientn(colors = c("#FF4500","#66CDAA"),  # Use darker shades of yellow and green
                        values = scales::rescale(c(min(avg_cancer_data_sf$env_var_pre), max(avg_cancer_data_sf$env_var_pre))),
                        limits = c(min(avg_cancer_data_sf$env_var_pre), max(avg_cancer_data_sf$env_var_pre)),
                        name = "Average precipitation/mm") +
  scale_size_continuous(range = c(1, 6), name = "Average precipitation/mm") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(title = "Distribution of annual average daily precipitation across cancer registries")

map_plot_solar <- ggplot() +
  geom_sf(data = world_map, fill = "gray90", color = "gray70", size = 0.2) +
  geom_sf(data = avg_cancer_data_sf, aes(color = env_var_solar),size = 1.5,alpha = 0.8) +
  scale_color_gradientn(colors = c("#66CDAA","#FF4500"),  # Use darker shades of yellow and green
                        values = scales::rescale(c(min(avg_cancer_data_sf$env_var_solar), max(avg_cancer_data_sf$env_var_solar))),
                        limits = c(min(avg_cancer_data_sf$env_var_solar), max(avg_cancer_data_sf$env_var_solar)),
                        name = "Surface net solar radiation/10^6 Jm-2") +
  scale_size_continuous(range = c(1, 6), name = "Surface net solar radiation/10^6 Jm-2") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(title = "Distribution of annual average solar radiation across cancer registries")

getwd()
setwd("C:/Haowen Wang/Cancer and climate change/export")
map_plot_NDVI <- map_plot_NDVI + theme(legend.position = "bottom")
map_plot_temp <- map_plot_temp + theme(legend.position = "bottom")
map_plot_pre <- map_plot_pre + theme(legend.position = "bottom")
map_plot_ozone <- map_plot_ozone + theme(legend.position = "bottom")
map_plot_solar <- map_plot_solar + theme(legend.position = "bottom")
map_plot_NDVI
map_plot_temp
map_plot_pre
map_plot_ozone
map_plot_solar
ggsave("map_plot_pre.png", map_plot_pre, width = 12, height = 12, units = "in", dpi = 300)
ggsave("map_plot_temp.png", map_plot_temp, width = 12, height = 12, units = "in", dpi = 300)
ggsave("map_plot_ozone.png", map_plot_ozone, width = 12, height = 12, units = "in", dpi = 300)
ggsave("map_plot_NDVI.png", map_plot_NDVI, width = 12, height = 12, units = "in", dpi = 300)
ggsave("map_plot_solar.png", map_plot_solar, width = 12, height = 12, units = "in", dpi = 300)