source("R/plots/barStylePlot.R")

extraIncomes <- c(2000000, 10000000, 50000000)
margins <- grid::unit(c(2.7,2.1,2.5,.25), "lines")
w <- 800
h <- 1037

#single moms plot final
png("img/png/barplot_singlemoms.png", w, h)
barStylePlot("Single", 1, "F", "ignore") + 
  coord_flip(ylim = c(0, .5), xlim = c(1.7, 7.2)) + 
  labs(x = "", y = "", title = "") +
  theme(legend.position  = "none",
        plot.margin  = margins)
dev.off()

#bachelors plot final
png("img/png/barplot_bachelor.png", w, h)
barStylePlot("Single", 0, "M", "ignore") + 
  coord_flip(ylim = c(0, .5), xlim = c(1.7, 8.1)) + 
  labs(x = "", y = "", title = "") +
  theme(legend.position  = "none",
        plot.margin  = margins)
dev.off()

#billionaires plot
png("img/png/barplot_billionaire.png", w, h)
barStylePlot("Married/Joint", 2, "M", "ignore", extraIncomes) +
  coord_flip(ylim = c(0, .63), xlim = c(4.5, 10.2)) + 
  labs(x = "", y = "", title = "") +
  theme(legend.position  = "none",
        plot.margin  = margins)
dev.off()

#businesses plot
png("img/png/barplot_business.png", w, h)
barStylePlot("Married/Joint", 2, "M", "isolate") +
  coord_flip(ylim = c(0, .5), xlim = c(1.5, 7.15)) + 
  labs(x = "", y = "", title = "") +
  theme(legend.position  = "none",
        plot.margin  = margins)
dev.off()

#pooled plot
png("img/png/barplot_pooled.png", w, h)
barStylePlot("Married/Joint", 2, "M", "pool", extraIncomes) +
  coord_flip(ylim = c(0, .65), xlim = c(2.6, 10.05)) + 
  labs(x = "", y = "", title = "") +
  theme(legend.position  = "none",
        plot.margin  = margins)
dev.off()


#family rehash plot
png("img/png/barplot_family.png", w, h)
barStylePlot("Married/Joint", 2, "M", "ignore") +
  coord_flip(ylim = c(0, .42), xlim = c(1.7, 7.15)) + 
  labs(x = "", y = "", title = "") +
  theme(legend.position  = "none",
        plot.margin  = margins)
dev.off()

#split plot
png("img/png/barplot_split.png", w, 600)
barStylePlot("Married/Joint", 2, "M", "split", labelPercents = F) +
  coord_flip(ylim = c(0, .6), xlim = c(1.7, 7.1)) + 
  labs(x = "", y = "", title = "") +
  theme(legend.position  = "none",
        plot.margin  = margins)
dev.off()