shadowOffsetX <- .003
shadowOffsetY <- -.003
shadowSize <- 2.5
shadowColor <- "grey30"

ggplot(filter(datDiff, payer == "Individual"), aes(x = percentile)) +
  geom_ribbon(aes(ymax = iTop, ymin = iBottom, fill = "Increase")) +
  geom_ribbon(aes(ymax = dTop, ymin = dBottom, fill = "Savings")) +
  geom_line(aes(y = eTaxBern + shadowOffsetY , x = percentile + shadowOffsetX), 
            size = shadowSize, color = shadowColor) + 
  geom_line(aes(y = eTaxCur + shadowOffsetY, x = percentile + shadowOffsetX), 
            size = shadowSize, color = shadowColor) + 
  geom_line(aes(y = eTaxBern, color = "Bernie"), size = 3) +
  geom_line(aes(y = eTaxCur, color = "Current"), size = 3) +
  #scale_x_log10(breaks = percentiles$income, labels = percentiles$xlabs) +
  scale_x_continuous(breaks = c(.05, .25, .5, .75, .95), labels = centileLabeler) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual("Tax Plan", values = c("#287CBF", "#EB514F")) +
  scale_fill_manual("Change under Bernie's Plans", 
                    limits = c("Savings", "Increase"),
                    values = c("#7db6e3", "#f18b89")) +
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
       title = "Just How Much Would Bernie Sanders Tax Me?") +
  coord_cartesian(xlim = c(0, 1))
