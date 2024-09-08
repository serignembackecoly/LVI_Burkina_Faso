# Librairies used #
# The default radar chart 

png("SpiderDiagramLVI.png", res = 400, height = 15, width = 15, units = "cm")

radarchart(data, axistype=1 , 
           
           
           #custom the grid
           cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,.8,.1), cglwd=0.8,
           
           #custom labels
           vlcex=0.8 , seg = 8, plwd = 2,
           
           pcol = c("red",  "blue", "green"),
           
           vlabels = c("Socio-Demographic\nProfile", "Livelihood\nStrategies",
                       "Social\nNetwork", "Food", "Water", "Health",
                       "Land", "Insecurity","Natural\nDisasters" )
)
dev.off()

# The triangle chart
data_lvi_ipcc <- read.csv2("LVI-IPCC.csv", header = T, check.names = F)
data_ipcc <- rbind(rep(.7,3) , rep(0,3) , data_lvi_ipcc)
rownames(data_ipcc) <- c("Max", "Min","Sahelian", "Sudano-Sahelian", "Sudanian")

png("SpiderDiagramLVI-IPCC.png", res = 400, height = 17, width = 17, units = "cm")

radarchart(data_ipcc, axistype=1 , 
           
           
           #custom the grid
           cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,.8,.1), cglwd=0.8,
           
           #custom labels
           vlcex=0.8 , seg = 8, plwd = 2,
           
           pcol = c("red",  "blue", "green"),
           
           vlabels = c("Adaptive\nCapacity", "Sensitivity", "Exposure" )
)
dev.off()