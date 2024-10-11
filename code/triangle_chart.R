# Librairies used
library(tidyverse)
library(fmsb)
library(Cairo)

# The triangle chart
lvi_ipcc <- read.csv2("data/LVI_IPCC.csv", header = T, check.names = F)

Max <- data.frame(rbind(rep(0.7,3)))
Min <- data.frame(rbind(rep(0,3)))
min_max <- rbind(Max, Min)
min_max <- min_max %>%
  mutate(ff = c("Max", "Min"),.before = 1)
colnames(min_max) <- colnames(lvi_ipcc)
data_ipcc <- rbind(min_max, lvi_ipcc) %>% 
              column_to_rownames(var = "rowname")
varnames <- c("Capacité \n d'Adaptation", "Sensibilité", "Exposition" )
#png("SpiderDiagramLVI-IPCC.png", res = 500, height = 14, width = 15, units = "cm")
Cairo::Cairo("Spider_diagram_LVI-IPCC_french.png", res = 500,
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
