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
png("bernieEmployer.png", width = 1024, height = 768)
#minor tweek to keep distinct fill regions from reaching across the graph to
#connect with each other
mutate_each(datDiff, funs(ifelse(is.na(.), 0.1216513, .)), starts_with("i"), 
            starts_with("d")) %>%
  ggplot(aes(x = percentile)) + 
  facet_grid(payer ~ .) +
  geom_ribbon(aes(ymax = iTop, ymin = iBottom, fill = "Increase")) +
  geom_ribbon(aes(ymax = dTop, ymin = dBottom, fill = "Savings")) +
  geom_line(aes(y = eTaxBern + shadowOffsetY , x = percentile + shadowOffsetX), 
            size = shadowSize, color = shadowColor[1]) + 
  geom_line(aes(y = eTaxCur + shadowOffsetY, x = percentile + shadowOffsetX), 
            size = shadowSize, color = shadowColor[2]) + 
  geom_line(aes(y = eTaxBern, color = "Bernie"), size = 2.75) +
  geom_line(aes(y = eTaxCur, color = "Current"), size = 2.75) +
  scale_x_continuous(breaks = c(.05, .25, .5, .75, .95), 
                     labels = centileLabeler) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual("Tax Plan", values = trendColors) +
  scale_fill_manual("Change under Bernie's Plans", 
                    limits = c("Savings", "Increase"),
                    values = fillColors) +
  guides(color = guide_legend(order = 1)) +
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


dev.off()