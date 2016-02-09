source("wsjStylePlot.R")

extraIncomes <- c(2000000, 10000000, 50000000)

barStylePlot("Single", 0, "M", customIncomes = extraIncomes)
  coord_flip(ylim = c(0, 1))

barStylePlot("Single", 0, "M", "pool", customIncomes = extraIncomes)

png("bartest.png", 768, 1024)
barStylePlot("Married/Joint", 2, "M", "pool", customIncomes = extraIncomes) + 
  coord_flip(ylim = c(0, .9), xlim = c(2.9, 11.3))
dev.off()