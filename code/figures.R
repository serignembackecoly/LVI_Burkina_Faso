# Librairies used
library(tidyverse)
library(fmsb)
# Load data
lvi_data <- read_csv2(file = "LVI_score.csv")

df <- lvi_data %>% 
  pivot_longer(cols = 2:4, names_to = "provinces", values_to = "values") %>% 
  pivot_wider(names_from = components, values_from = values)

colnames(df)[1] <- "group"

Max <- data.frame(rbind(rep(0.8,9)))
Min <- data.frame(rbind(rep(0,9)))
min_max <- rbind(Max, Min)
min_max <- min_max %>%
mutate(ff = c("Max", "Min"),.before = 1)
colnames(min_max) <- colnames(df)
lvi <- rbind(min_max, df)
# Set the column as row names and then remove the column
lvi <- lvi %>%
  column_to_rownames(var = "group")
varnames <- c("Socio-Demographic\nProfile", "Livelihood\nStrategies",
              "Social\nNetwork", "Food", "Water", "Health",
              "Land", "Insecurity","Natural\nDisasters" )
# The default radar chart 

  #png("SpiderDiagramLVI.png", res = 500, height = 20, width = 22, units = "cm")
  Cairo("Spider_diagram_LVI.png", res = 500, height = 20,
        width = 22, units = "cm", bg = "white")
  
  
  op <- par(mar = c(1, 2, 2, 2))
  # Create the radar charts
  radarchart(lvi, axistype=1 , 
             
             
             #custom the grid
             cglcol="gray55", cglty=1, axislabcol="black", caxislabels=seq(0,.8,.1), cglwd=0.8,
             
             #custom labels
             vlcex=1.5 , seg = 8, plwd = 3.5,
             
             pcol = c("red",  "blue", "green"),
             
             vlabels = c("Socio-Demographic\nProfile", "Livelihood\nStrategies",
                         "Social\nNetwork", "Food", "Water", "Health",
                         "Land", "Insecurity","Natural\nDisasters" )
  )
  
  # Add an horizontal legend
  legend(
    x = "topright", legend = c("Kongoussi", "Léo", "Gaoua"),
    bty = "n", pch = 16 , col = c("red",  "blue", "green"),
    text.col = "black", cex = 1.2, pt.cex = 1.2
  )
  par(op)
  dev.off()
  