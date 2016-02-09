source("wsjStylePlot.R")

extraIncomes <- c(2000000, 10000000, 50000000)

barStylePlot("Single", 0, "M", customIncomes = extraIncomes)
  coord_flip(ylim = c(0, 1))

barStylePlot("Single", 0, "M", "pool", customIncomes = extraIncomes)

png("bartest.png", 768, 1037)
barStylePlot("Single", 0, "F", "ignore", customIncomes = extraIncomes) + 
  coord_flip(ylim = c(0, .6), xlim = c(1.7, 10.5)) + 
  labs(x = "", y = "", title = "") +
  theme(legend.position  = "none",
        plot.margin  = grid::unit(c(2.7,0,1.25,.25), "lines"))
dev.off()