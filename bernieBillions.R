shadowOffsetX <- .002
shadowOffsetY <- -.002
shadowSize <- 2.5
shadowColor <- c("grey30", "grey30")
#line colors bernie, current
#trendColors <- c("#287CBF", "#44445c")
trendColors <- c("#2d8cd8", "#48476f")


#fill colors savings, increase
#fillColors <- c("#1FC77F", "#FFB450")
fillColors <- c("#21DC91", "#FF9400")
png("bernieBillions.png", width = 1024, height = 768)
ggplot(filter(datDiff, payer == "Individual"), aes(x = income)) +
  geom_ribbon(aes(ymax = iTop, ymin = iBottom, fill = "Increase")) +
  geom_ribbon(aes(ymax = dTop, ymin = dBottom, fill = "Savings")) +
  geom_line(aes(y = eTaxBern, color = "Bernie"), size = 2.75) +
  geom_line(aes(y = eTaxCur, color = "Current"), size = 2.75) +
  scale_x_log10(breaks = c(60000, 250000, 1000000, 5000000),
                labels = scales::dollar) +
#   scale_x_continuous(breaks = c(.05, .25, .5, .75, .95), 
#                      labels = centileLabeler) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual("Tax Plan", values = trendColors) +
  scale_fill_manual("Change under Bernie's Plans", 
                    limits = c("Savings", "Increase"),
                    values = fillColors) +
  #guides(color = guide_legend(order = 1)) +
  #guides(color = F, fill = F) +
  theme(axis.line = element_blank(), 
        legend.position = "bottom", 
        text = element_text(size = 24),
        axis.title.y = element_text(margin = margin(0, 15, 0, 0)),
        axis.title.x = element_text(margin = margin(15, 0, 0, 0)),
        plot.title = element_text(margin = margin(0, 0, 30, 0), size = 40),
        panel.grid.major.x = 
          element_line(colour = "grey80", size = 1, linetype = 2),
        panel.background = element_blank(),
        axis.ticks.x = element_blank()) +
  labs(y = "Tax and Healthcare Burden (% of income)", 
       x = "Income for a Family of 4 (USD)", 
       title = "Just How Much Would Bernie Sanders Tax Me?") #+
#   coord_cartesian(xlim = c(0, 1.15)) +
#   annotate("text", x = .21, y = .33, 
#            label = "Taxes + Healthcare Expense Now",
#            angle = 0, hjust = 0, color = trendColors[2], size = 12,
#            fontface = "bold") + 
#   annotate("text", x = .23, y = -.19, 
#            label = "Bernie's Plans for Taxes, Healthcare and More",
#            angle = 28, hjust = 0, color = trendColors[1], size = 12,
#            fontface = "bold") +
#   
#   geom_label(aes(y = mid, label = lab, hjust = hjust), data = savings,
#              size = 6)

dev.off()

