# Librairies used
library(tidyverse)
library(fmsb)
library(Cairo)

# The triangle chart
lvi_ipcc <- read.csv2("LVI_IPCC.csv", header = T, check.names = F)

Max <- data.frame(rbind(rep(0.7,3)))
Min <- data.frame(rbind(rep(0,3)))
min_max <- rbind(Max, Min)
min_max <- min_max %>%
  mutate(ff = c("Max", "Min"),.before = 1)
colnames(min_max) <- colnames(lvi_ipcc)
data_ipcc <- rbind(min_max, lvi_ipcc) %>% 
              column_to_rownames(var = "rowname")
varnames <- c("Adaptive\nCapacity", "Sensitivity", "Exposure" )
#png("SpiderDiagramLVI-IPCC.png", res = 500, height = 14, width = 15, units = "cm")
Cairo("Spider_diagram_LVI-IPCC.png", res = 500,
      height = 15, width = 15,bg = "white", units = "cm") 

disposition <- par(mar = c(1, 2, 2, 2))
radarchart(data_ipcc, axistype=1 , 
           
           
           #custom the grid
           cglcol="gray55", cglty=1, axislabcol="black", caxislabels=seq(0,.7,.1), cglwd=0.8,
           
           #custom labels
           vlcex=1.3 , plwd = 1.5, seg = 7,
           
           pcol = c("red",  "blue", "green"),
           
           vlabels =  varnames)

# Add an horizontal legend
legend(
  x = "topright", legend = c("Kongoussi", "Léo", "Gaoua"),
  bty = "n", pch = 16 , col = c("red",  "blue", "green"),
  text.col = "black", cex = 1, pt.cex = 1
)
par(disposition)
dev.off()



# Load necessary libraries
library(tidyverse)
library(fmsb)
library(Cairo)

# Load data
lvi_ipcc <- read_csv2("LVI_IPCC.csv")

# Create maximum and minimum values
max_values <- data.frame(rbind(rep(0.7, 3)))
min_values <- data.frame(rbind(rep(0, 3)))
min_max <- rbind(max_values, min_values)
min_max <- min_max %>%
  mutate(group = c("Max", "Min"), .before = 1)

# Combine data and create radar chart
colnames(min_max) <- colnames(lvi_ipcc)
data_ipcc <- rbind(min_max, lvi_ipcc)
rownames(data_ipcc) <- data_ipcc$rowname
data_ipcc <- data_ipcc[, -1]  # Remove the "rowname" column

# Radar chart parameters
var_names <- c("Adaptive\nCapacity", "Sensitivity", "Exposure")

# Create radar chart
Cairo("spider_diagram_LVI-IPCC.png", res = 500, height = 15, width = 15, bg = "white", units = "cm")
par(mar = c(1, 2, 2, 2))
radarchart(data_ipcc, axistype = 1,
           cglcol = "gray55", cglty = 1, axislabcol = "black", caxislabels = seq(0, 0.7, 0.1), cglwd = 0.8,
           vlcex = 1.3, plwd = 1.5, seg = 7,
           pcol = c("red", "blue", "green"),
           vlabels = var_names)

# Add legend
legend(
  x = "topright", legend = c("Kongoussi", "Léo", "Gaoua"),
  bty = "n", pch = 16, col = c("red", "blue", "green"),
  text.col = "black", cex = 1, pt.cex = 1
)
par(op)
dev.off()
