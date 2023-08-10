# Load necessary packages
library(ggplot2)
library(reshape2)

# Data
data <- data.frame(
  Environmental_Factor = c("NDVI", "NDVI", "NDVI", "Solar Radiation", "Solar Radiation", "Solar Radiation", "Precipitation", "Temperature", "Temperature"),
  Cancer = c("Prostate", "Lung", "Colorectum", "Melanoma of the skin", "Prostate", "Testis", "Lung", "Melanoma of the skin", "Lung"),
  Value = c(-10.84, -6.66,-3.6, 3.43, -11.56, 1.71, -2.79, -2.4, -2.15)
)

# Reshape data
data_wide <- dcast(data, Environmental_Factor ~ Cancer, value.var = "Value")

# Melt data for ggplot
data_long <- melt(data_wide, id.vars = "Environmental_Factor")

# Create heatmap
main_results <- ggplot(data_long, aes(x = Environmental_Factor, y = variable, fill = value)) +
  geom_tile(color = "black") +
  geom_text(aes(label = round(value, 2)), size = 5, na.rm = TRUE) +
  scale_fill_gradient2(low = "deepskyblue2", high = "indianred2", mid = "white", 
                       midpoint = 0, limit = c(-15, 15), 
                       name = "Coefficients", na.value = "white") +
  theme_minimal()  +
  theme(
    plot.title = element_text(size = 20, hjust = 0.5),  # Increase title size and center it
    axis.title = element_text(size = 16),  # Increase axis label size
    axis.text = element_text(size = 14),  # Increase axis text size
    legend.text = element_text(size = 12),  # Increase legend text size
    legend.title = element_text(size = 14),  # Increase legend title size
    axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 4),  # Increase spacing under x-axis labels
    plot.margin = margin(2, 1, 2, 1, "cm"),
    plot.title.position = "plot" # Increase plot margins
  )  +
  labs(x = "Climatic factors", y = "Cancer sites")
main_results
ggsave("Figure3.pdf", main_results, width = 12, height = 8, units = "in",dpi=300)
ggsave("Figure3.jpg", main_results, width = 12, height = 8, units = "in",dpi=300)
