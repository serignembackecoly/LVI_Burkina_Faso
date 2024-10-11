# Load necessary libraries
library(tidyverse)
library(fmsb)

# Load data
lvi_data <- read_csv2(file = "data/LVI_score.csv")

# Prepare data for radar chart
data <- lvi_data %>%
  pivot_longer(cols = 2:4, names_to = "provinces", values_to = "values") %>%
  pivot_wider(names_from = components, values_from = values)

# Add maximum and minimum values for radar chart
max_values <- data.frame(rbind(rep(0.8, 9)))
min_values <- data.frame(rbind(rep(0, 9)))
min_max <- rbind(max_values, min_values)
min_max <- min_max %>%
  mutate(group = c("Max", "Min"), .before = 1)

# Combine data and create radar chart
colnames(min_max) <- colnames(data)
lvi <- rbind(min_max, data)
rownames(lvi) <- lvi$group
lvi <- lvi[, -1]  # Remove the "group" column

# Radar chart parameters
var_names <- c("Profile \n Socio-Demographique", "Stratégies \n de Subsitance",
               "Liens \n Sociaux", "Alimentation", "Eau", "Santé",
               "Foncier", "Insécurité", "Catastrophes \nNaturelles")

# Create radar chart
Cairo::Cairo("figure/spider_diagram_LVI_french.png", res = 500, height = 20, width = 22, units = "cm", bg = "white")
par(mar = c(1, 2, 2, 2))
radarchart(lvi, axistype = 1,
           cglcol = "gray55", cglty = 1, axislabcol = "black", caxislabels = seq(0, 0.8, 0.1), cglwd = 0.8,
           vlcex = 1.5, seg = 8, plwd = 3.5,
           pcol = c("red", "blue", "green"),
           vlabels = var_names)

# Add legend
legend(
  x = "topright", legend = c("Kongoussi", "Léo", "Gaoua"),
  bty = "n", pch = 16, col = c("red", "blue", "green"),
  text.col = "black", cex = 1.2, pt.cex = 1.2
)
#par(op)
dev.off()
