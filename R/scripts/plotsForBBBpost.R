source("R/plots/barStylePlot.R")

extraIncomes <- c(2000000, 10000000, 50000000)

#single moms plot final
png("barplot_singlemoms.png", 768, 1037)
barStylePlot("Single", 1, "F", "ignore") + 
  coord_flip(ylim = c(0, .5), xlim = c(1.7, 7.5)) + 
  labs(x = "", y = "", title = "") +
  theme(legend.position  = "none",
        plot.margin  = grid::unit(c(2.7,0,2.5,.25), "lines"))
dev.off()